#' Definitions of reference with 'LFP' signal type
#' @description Please use a safer \code{\link{new_reference}} function to
#' create instances. This documentation is to describe the member methods
#' of the electrode class \code{LFP_reference}
#'
#' @examples
#' \dontrun{
#'
#' # Download subject demo/DemoSubject
#'
#'
#' subject <- as_rave_subject("demo/DemoSubject")
#'
#' # Electrode 14 as reference electrode (Bipolar referencing)
#' e <- new_reference(subject = subject, number = "ref_14",
#'                    signal_type = "LFP")
#'
#' # Reference "ref_13-16,24" (CAR or white-matter reference)
#' ref <- new_reference(subject = subject, number = "ref_13-16,24",
#'                      signal_type = "LFP")
#' ref
#'
#' # Set epoch
#' e$set_epoch(epoch = 'auditory_onset')
#'
#' # Set loading window
#' e$trial_intervals <- list(c(-1, 2))
#'
#' # Preview
#' print(e)
#'
#' # Now epoch power
#' power <- e$load_data("power")
#' names(dimnames(power))
#'
#' # Subset power
#' subset(power, Time ~ Time < 0, Electrode ~ Electrode == 14)
#'
#' # clear cache on hard disk
#' e$clear_cache()
#'
#' }
#'
#' @export
LFP_reference <- R6::R6Class(
  classname = 'LFP_reference',
  inherit = RAVEAbstarctElectrode,
  portable = FALSE,
  lock_class = TRUE,
  private = list(
    .type = 'LFP',
    .location = 'iEEG',
    .is_reference = TRUE,
    .power_enabled = TRUE
  ),
  public = list(

    #' @description print reference summary
    print = function(){
      cat("<Reference electrode>\n")
      cat(sprintf("  Project: %s\n", self$subject$project_name))
      cat(sprintf("  Subject: %s\n", self$subject$subject_code))
      cat(sprintf("  Reference label: %s\n", self$number))
      cat("  Location type:", self$location, "\n")
      cat("  Signal type:", self$type, "\n")
      cat("  Sample rates:\n")
      cat("    - Analog-trace (voltage):", self$raw_sample_rate, "\n")
      cat("    - Power-phase (coefficients):", self$power_sample_rate, "\n")
      if(length(self$epoch)){
        cat("  Epoch: ", self$epoch_name, "\n")
      } else {
        cat("* Epoch not set\n")
      }
      if(length(self$trial_intervals)){
        cat("  Trial windows: ", deparse1(self$trial_intervals), "\n")
      } else {
        cat("* Trial windows not set\n")
      }
    },

    #' @description set reference for current electrode
    #' @param reference either \code{NULL} or \code{LFP_electrode} instance
    set_reference = function(reference){
      # Do nothing as this is already a reference electrode
      if(length(reference)){
        stop("Please do not add reference to another reference")
      }
    },

    #' @description constructor
    #' @param subject,number,quiet see constructor in
    #' \code{\link{RAVEAbstarctElectrode}}
    initialize = function(subject, number, quiet = FALSE){
      super$initialize(subject, number)

      ref_electrodes <- gsub("[^0-9,\\ -]+", '', number)
      e <- dipsaus::parse_svec(ref_electrodes)

      if(length(e) == 0){
        self$number <- 'noref'
      } else {
        if(length(e) == 1){
          self$number <- e
        } else {
          # check subject reference directory
          self$number <- sprintf('ref_%s', dipsaus::deparse_svec(e))
          if(!file.exists(file.path(self$subject$reference_path,
                                    sprintf("%s.h5", self$number)))){
            if(!quiet) {
              catgl("Reference file {self$number}.h5 is missing", level = "WARNING")
            }
          }
        }
      }

    },


    # data method

    #' @description load non-referenced wavelet coefficients (internally used)
    #' @param reload whether to reload cache
    #' @returns if the reference number if \code{NULL} or \code{'noref'}, then
    #' returns 0, otherwise returns a \code{\link[filearray]{FileArray-class}}
    .load_noref_wavelet = function(reload = FALSE){

      srate <- self$power_sample_rate
      stopifnot2(!is.na(srate), msg = "Cannot find power sample rate. Have you applied time-frequency decomposition?")
      tidx <- unlist(lapply(self$trial_intervals, function(x){
        x <- round(x * srate)
        seq(x[1], x[2])
      }))
      stopifnot2(length(tidx), msg = "Trial window has length of 0")

      if(!length(self$number) || self$number == "noref"){
        return(0)
      }

      epoch_tbl <- self$epoch$table

      freq <- self$subject$meta_data("frequencies")
      nfreq <- nrow(freq)
      ntrial <- nrow(epoch_tbl)
      ntime <- length(tidx)

      noref_cache_path <- file.path(self$cache_root, "noref")
      arr_path <- file.path(noref_cache_path, "wavelet-coefficient")

      if(file.exists(arr_path)){
        if(reload){
          unlink(arr_path, recursive = TRUE, force = TRUE)
        } else {
          tryCatch({
            return(filearray::filearray_checkload(
              filebase = arr_path, mode = "readonly",
              rave_data_type = "wavelet-coefficient",
              symlink_ok = FALSE, valid = TRUE
            ))
          }, error = function(e){
            unlink(arr_path, recursive = TRUE, force = TRUE)
          })
        }
      }

      dir_create2(noref_cache_path)
      blocks <- unique(epoch_tbl$Block)
      if(!all(blocks %in% self$subject$blocks)){
        blocks <- blocks[!blocks %in% self$subject$blocks]
        stop("Some blocks cannot be found: ", paste(blocks, collapse = ", "))
      }

      # freq x time x trial
      arr <- filearray::filearray_create(
        arr_path,
        dimension = c(nfreq, ntime, ntrial, 1),
        type = "complex",
        partition_size = 1
      )
      arr$set_header("rave_data_type", "wavelet-coefficient", save = FALSE)

      dimnames(arr) <- list(
        Frequency = freq$Frequency,
        Time = tidx / srate,
        Trial = sort(epoch_tbl$Trial),
        Electrode = self$number
      )

      if(using_netdrive()){
        # Copy the HDF5 file to temporary folder and read
        tempdir(check = TRUE)
        power_file <- tempfile(fileext = ".h5", pattern = "temppower_")
        phase_file <- tempfile(fileext = ".h5", pattern = "tempphase_")
        on.exit({
          if(file.exists(power_file)){
            unlink(power_file)
          }
          if(file.exists(phase_file)){
            unlink(phase_file)
          }
        }, add = TRUE, after = TRUE)
        file.copy(self$power_file, to = power_file)
        file.copy(self$phase_file, to = phase_file)
      } else {
        power_file <- self$power_file
        phase_file <- self$phase_file
      }

      for(b in blocks){
        sel <- epoch_tbl$Block == b
        if(!any(sel)){
          next
        }

        trials <- epoch_tbl$Trial[sel]
        onsets <- epoch_tbl$Time[sel]
        tp <- sapply(onsets, function(o){
          idx <- round(o * srate)
          idx + tidx
        })

        if( !is.numeric(self$number) ){
          h5_name <- sprintf('/wavelet/coef/%s', b)
          block_data <- load_h5(file = power_file, name = h5_name, ram = HDF5_EAGERLOAD)
          coef <- block_data[,tp,]
          dim(coef) <- c(nrow(coef), dim(tp), 2)
          coef <- coef[,,,1] * exp(1i * coef[,,,2])
        } else {
          h5_name <- sprintf('/raw/power/%s', b)
          block_data <- load_h5(file = power_file, name = h5_name, ram = HDF5_EAGERLOAD)
          power <- block_data[, tp]
          dim(power) <- c(nrow(power), dim(tp))
          h5_name <- sprintf('/raw/phase/%s', b)
          block_data <- load_h5(file = phase_file, name = h5_name, ram = HDF5_EAGERLOAD)
          phase <- block_data[, tp]
          dim(phase) <- c(nrow(phase), dim(tp))
          coef <- sqrt(power) * exp(1i * phase)
        }
        arr[,,trials,1] <- coef
      }

      arr$set_header("valid", TRUE)
      arr$.mode <- "readonly"
      return(arr)
    },

    #' @description load non-referenced voltage (internally used)
    #' @param srate voltage signal sample rate
    #' @param reload whether to reload cache
    .load_noref_voltage = function(reload = FALSE){

      if(!length(self$number) || self$number == "noref"){
        return(0)
      }

      noref_cache_path <- file.path(self$cache_root, "noref")
      arr_path <- file.path(noref_cache_path, "voltage")

      if(file.exists(arr_path)){
        if(reload){
          unlink(arr_path, recursive = TRUE, force = TRUE)
        } else {
          tryCatch({
            return(filearray::filearray_checkload(
              filebase = arr_path, mode = "readonly",
              rave_data_type = "voltage",
              symlink_ok = FALSE, valid = TRUE
            ))
          }, error = function(e){
            unlink(arr_path, recursive = TRUE, force = TRUE)
          })
        }
      }

      dir_create2(noref_cache_path)

      srate <- self$raw_sample_rate

      tidx <- unlist(lapply(self$trial_intervals, function(x){
        x <- round(x * srate)
        seq(x[1], x[2])
      }))
      epoch_tbl <- self$epoch$table

      blocks <- unique(epoch_tbl$Block)

      if(!all(blocks %in% self$subject$blocks)){
        blocks <- blocks[!blocks %in% self$subject$blocks]
        stop("Some blocks cannot be found: ", paste(blocks, collapse = ", "))
      }

      ntrial <- nrow(epoch_tbl)
      ntime <- length(tidx)

      # freq x time x trial
      arr <- filearray::filearray_create(
        arr_path,
        dimension = c(ntime, ntrial, 1),
        type = "double",
        partition_size = 1
      )
      arr$set_header("rave_data_type", "voltage", save = FALSE)

      dimnames(arr) <- list(
        Time = tidx / srate,
        Trial = sort(epoch_tbl$Trial),
        Electrode = self$number
      )

      for(b in blocks){
        sel <- epoch_tbl$Block == b
        if(!any(sel)){
          next
        }

        trials <- epoch_tbl$Trial[sel]
        onsets <- epoch_tbl$Time[sel]
        tp <- sapply(onsets, function(o){
          idx <- round(o * srate)
          idx + tidx
        })

        if( file.exists(self$voltage_file) ) {
          if( !is.numeric(self$number) ){
            h5_name <- sprintf('/voltage/%s', b)
            block_data <- load_h5(file = self$voltage_file, name = h5_name, ram = HDF5_EAGERLOAD)
          } else {
            h5_name <- sprintf('/raw/voltage/%s', b)
            block_data <- load_h5(file = self$voltage_file, name = h5_name, ram = HDF5_EAGERLOAD)
          }
        } else {
          if( !is.numeric(self$number) ){
            stop("Cannot find the voltage signal for calculated reference signal: ", self$number, ". Please generate the reference first.")
          } else {
            h5_name <- sprintf('/notch/%s', b)
            block_data <- load_h5(file = self$preprocess_file, name = h5_name, ram = HDF5_EAGERLOAD)
          }
        }

        voltage <- block_data[tp]
        dim(voltage) <- dim(tp)
        arr[,trials,1] <- voltage
      }

      arr$set_header("valid", TRUE)
      arr$.mode <- "readonly"
      return(arr)
    },

    #' @description load referenced wavelet coefficients (internally used)
    #' @param type type of data to load
    #' @param reload whether to reload cache
    .load_wavelet = function(type = c("power", "phase", "wavelet-coefficient"),
                             reload = FALSE){
      type <- match.arg(type)

      noref_e <- self$.load_noref_wavelet()
      if( is.numeric(noref_e) && noref_e == 0 ){ return(0) }
      if( type == "wavelet-coefficient" ){
        return(noref_e)
      }

      arr_path <- file.path(self$cache_root, self$reference_name, type)

      # noref_cache_path <- file.path(self$cache_root, "noref")
      # arr_path <- file.path(noref_cache_path, "wavelet-coefficient")

      if(file.exists(arr_path)){
        if(reload){
          unlink(arr_path, recursive = TRUE, force = TRUE)
        } else {
          tryCatch({
            return(filearray::filearray_checkload(
              filebase = arr_path, mode = "readonly",
              rave_data_type = type,
              symlink_ok = FALSE, valid = TRUE
            ))
          }, error = function(e){
            unlink(arr_path, recursive = TRUE, force = TRUE)
          })
        }
      }

      dir_create2(dirname(arr_path))
      dim <- dim(noref_e)
      arr <- filearray::filearray_create(
        filebase = arr_path,
        dimension = dim,
        type = "float",
        partition_size = 1
      )
      arr$set_header("rave_data_type", type, save = FALSE)
      dimnames(arr) <- dimnames(noref_e)

      # noref_e

      f <- switch (
        type,
        "power" = {
          filearray::fmap(list(noref_e), function(v){
            Mod(v[[1]])^2
          }, .y = arr)
        },
        "phase" = {
          filearray::fmap(list(noref_e), function(v){
            Arg(v[[1]])
          }, .y = arr)
        }, {
          stop("Code bug: unregistered data type: ", type)
        }
      )

      arr$set_header("valid", TRUE)
      arr$.mode <- "readonly"
      arr
    },

    #' @description load referenced voltage (internally used)
    #' @param reload whether to reload cache
    .load_voltage = function(reload = FALSE){
      self$.load_noref_voltage(reload = reload)
    },


    #' @description method to load electrode data
    #' @param type data type such as \code{"power"}, \code{"phase"},
    #' \code{"voltage"}, \code{"wavelet-coefficient"}.
    load_data = function(type = c(
      "power", "phase", "voltage", "wavelet-coefficient")){

      type <- match.arg(type)
      if(type == "voltage"){
        return(self$.load_voltage())
      } else {
        return(self$.load_wavelet(type))
      }

    },

    #' @description load electrode block-wise data (with reference),
    #' useful when epoch is absent
    #' @param blocks session blocks
    #' @param type data type such as \code{"power"}, \code{"phase"},
    #' \code{"voltage"}, \code{"wavelet-coefficient"}. Note that if type
    #' is voltage, then 'Notch' filters must be applied; otherwise 'Wavelet'
    #' transforms are required.
    #' @param simplify whether to simplify the result
    #' @returns If \code{simplify} is enabled, and only one block is loaded,
    #' then the result will be a vector (\code{type="voltage"}) or a matrix
    #' (others), otherwise the result will be a named list where the names
    #' are the blocks.
    load_blocks = function(blocks, type = c("power", "phase", "voltage", "wavelet-coefficient"), simplify = TRUE) {
      type <- match.arg(type)
      if(!length(blocks)) {
        if(simplify){ return(NULL) }
        return(list())
      }
      stopifnot2(all(blocks %in% self$subject$blocks),
                 msg = "Electrode `load_blocks`: all blocks must exist")

      # check whether the reference is single electrode
      single_electrode <- is.numeric(self$number)

      re <- NULL

      if(single_electrode) {

        if(type == 'voltage') {
          notch_filtered <- self$subject$notch_filtered[self$subject$electrodes %in% self$number]
          if(!isTRUE(notch_filtered)) {
            stop("load_blocks: electrode ", self$number, " has not been Notch-filtered.")
          }
          re <- load_blocks_voltage_single(self = self, blocks = blocks)
        } else {
          waveleted <- self$subject$has_wavelet[self$subject$electrodes %in% self$number]
          if(!isTRUE(waveleted)) {
            stop("load_blocks: electrode ", self$number, " has not been wavelet-transformed")
          }
          re <- load_blocks_wavelet_single(self = self, blocks = blocks, type = type)
        }

      } else {

        if(type == 'voltage') {
          re <- load_blocks_voltage_multi(self = self, blocks = blocks)
        } else {
          re <- load_blocks_wavelet_multi(self = self, blocks = blocks, type = type)
        }

      }

      if(simplify && length(blocks) == 1){
        re <- re[[1]]
      }
      return(re)
    },

    #' @description method to clear cache on hard drive
    #' @param ... ignored
    clear_cache = function(...){
      try({
        dir <- self$cache_root
        if(!is.na(dir) && dir.exists(dir)){
          unlink(dir, recursive = TRUE)
        }
      }, silent = TRUE)
    },

    #' @description method to clear memory
    #' @param ... ignored
    clear_memory = function(...){
    }


  ),
  active = list(

    #' @field exists whether electrode exists in subject
    exists = function(){
      if( is.numeric(self$number) ){
        super$exists
      } else if( isTRUE(self$number == 'noref') ) {
        return(TRUE)
      } else {
        file.exists(file.path(self$subject$reference_path,
                              self$h5_fname))
      }
    },

    #' @field h5_fname 'HDF5' file name
    h5_fname = function(){
      sprintf('%s.h5', self$number)
    },


    #' @field valid whether current electrode is valid: subject exists and
    #' contains current electrode or reference; subject electrode type matches
    #' with current electrode type
    valid = function(){
      return(self$exists)
    },

    #' @field raw_sample_rate voltage sample rate
    raw_sample_rate = function(){
      sel <- self$subject$electrode_types == self$type
      if(any(sel)){
        self$subject$raw_sample_rates[sel][[1]]
      } else {
        NA
      }
    },

    #' @field power_sample_rate power/phase sample rate
    power_sample_rate = function(){
      sel <- self$subject$electrode_types == self$type
      if(any(sel)){
        self$subject$power_sample_rate[sel][[1]]
      } else {
        NA
      }
    },

    #' @field preprocess_info preprocess information
    preprocess_info = function(){
      # self$subject$preprocess_settings$electrode_info(electrode = self$number)
      NULL
    },

    #' @field power_file path to power 'HDF5' file
    power_file = function(){
      if(is.numeric(self$number)){
        return(super$power_file)
      } else {
        return(file.path(self$subject$reference_path,
                         self$h5_fname))
      }
    },

    #' @field phase_file path to phase 'HDF5' file
    phase_file = function(){
      if(is.numeric(self$number)){
        return(super$phase_file)
      } else {
        return(file.path(self$subject$reference_path,
                         self$h5_fname))
      }
    },

    #' @field voltage_file path to voltage 'HDF5' file
    voltage_file = function(){
      if(!is.numeric(self$number)){
        return(file.path(self$subject$reference_path,
                         self$h5_fname))
      } else {
        return(super$voltage_file)
      }
    }

  )
)



load_blocks_voltage_single <- function(self, blocks) {
  "This is internally used, no check is performed. Please check blocks, wavelet..."
  # load directly from HDF5 file

  if(file.exists(self$voltage_file)) {
    re <- structure(lapply(blocks, function(block){
      load_h5(self$voltage_file,
              name = sprintf("/raw/voltage/%s", block),
              ram = TRUE)
    }), names = blocks)
  } else if(file.exists(self$preprocess_file)){
    re <- structure(lapply(blocks, function(block){
      load_h5(self$preprocess_file,
              name = sprintf("/notch/%s", block),
              ram = TRUE)
    }), names = blocks)
  } else {
    stop("Voltage data file is missing or corrupted: [subject: ", self$subject$subject_id, ", electrode: ", self$number, "]")
  }

  re
}

load_blocks_wavelet_single <- function(self, blocks, type) {
  "This is internally used, no check is performed. Please check blocks, wavelet..."
  # load directly from HDF5 file
  if(type == "power") {
    fun <- function(block) {
      t(load_h5(self$power_file,
                name = sprintf("/raw/power/%s", block),
                ram = TRUE))
    }
  } else if(type == "phase") {
    fun <- function(block) {
      t(load_h5(self$phase_file,
                name = sprintf("/raw/phase/%s", block),
                ram = TRUE))
    }
  } else {
    fun <- function(block){
      power <- load_h5(self$power_file,
                       name = sprintf("/raw/power/%s", block),
                       ram = TRUE)
      phase <- load_h5(self$phase_file,
                       name = sprintf("/raw/phase/%s", block),
                       ram = TRUE)
      t(sqrt(power) * exp(1i * phase))
    }
  }

  re <- structure(lapply(blocks, fun), names = blocks)
  re
}

load_blocks_voltage_multi <- function(self, blocks) {
  if(isTRUE(self$number %in% c("noref", ""))) {
    return(structure(as.list(rep(0, length(blocks))), names = blocks))
  }
  # get the cache
  cache_root <- gsub("\\.h5$", "", self$voltage_file, ignore.case = TRUE)
  re <- structure(lapply(blocks, function(block){
    cache_path <- file.path(cache_root, block, "voltage")
    tryCatch({
      arr <- filearray::filearray_checkload(
        filebase = cache_path, mode = "readonly",
        staged = TRUE, rave_data_type = "block-voltage"
      )
      arr[]
    }, error = function(e){
      if(dir.exists(cache_path)) {
        unlink(cache_path, recursive = TRUE)
      }
      dir_create2(dirname(cache_path))

      # load from H5
      if(file.exists(self$voltage_file)) {
        ref <- load_h5(self$voltage_file,
                       name = sprintf("/voltage/%s", block),
                       ram = TRUE)
      } else {
        stop("Cannot find calculated reference data of: ", self$number)
        # ref <- load_h5(self$preprocess_file,
        #                name = sprintf("/notch/%s", block),
        #                ram = TRUE)
      }

      arr <- filearray::filearray_create(
        filebase = cache_path, dimension = c(length(ref), 1L),
        type = "double", partition_size = 1L
      )
      arr[] <- ref
      arr$set_header("rave_data_type", "block-voltage", save = FALSE)
      arr$set_header("staged", TRUE)
      arr$.mode <- "readonly"
      ref
    })

  }), names = blocks)
  return(re)
}

load_blocks_wavelet_multi <- function(self, blocks, type) {
  if(isTRUE(self$number %in% c("noref", ""))) {
    return(structure(as.list(rep(0, length(blocks))), names = blocks))
  }

  # get the cache
  cache_root <- gsub("\\.h5$", "", self$power_file, ignore.case = TRUE)
  re <- structure(lapply(blocks, function(block){
    cache_path <- file.path(cache_root, block, "wavelet")
    data <- tryCatch({
      arr <- filearray::filearray_checkload(
        filebase = cache_path, mode = "readonly",
        staged = TRUE, rave_data_type = "block-wavelet-coefficient"
      )
      arr[]
    }, error = function(e){
      if(dir.exists(cache_path)) {
        unlink(cache_path, recursive = TRUE)
      }
      dir_create2(dirname(cache_path))

      # load from H5
      ref <- load_h5(self$power_file,
                             name = sprintf("/wavelet/coef/%s", block),
                             ram = TRUE)
      dm <- dim(ref)[c(2,1)]
      ref <- t(ref[,,1] * exp(1i * ref[,,2]))
      arr <- filearray::filearray_create(
        filebase = cache_path, dimension = dm,
        type = "complex", partition_size = 1L
      )
      arr[] <- ref
      arr$set_header("rave_data_type", "block-wavelet-coefficient", save = FALSE)
      arr$set_header("staged", TRUE)
      arr$.mode <- "readonly"
      ref
    })

    if(type == "power") {
      data <- Mod(data)^2
    } else if(type == "phase") {
      data <- Arg(data)
    }
    data

  }), names = blocks)
  re
}


