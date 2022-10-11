#' Definitions of reference with 'LFP' signal type
#' @description Please use a safer \code{\link{new_electrode}} function to
#' create instances. This documentation is to describe the member methods
#' of the electrode class \code{LFP_electrode}
#'
#' @examples
#'
#' # Download subject demo/DemoSubject
#'
#' subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)
#'
#' if(dir.exists(subject$path)) {
#'
#' # Electrode 14 in demo/DemoSubject
#' e <- new_electrode(subject = subject, number = 14, signal_type = "LFP")
#'
#' # Load CAR reference "ref_13-16,24"
#' ref <- new_reference(subject = subject, number = "ref_13-16,24",
#'                      signal_type = "LFP")
#' e$set_reference(ref)
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
#' # Draw baseline
#' tempfile <- tempfile()
#' bl <- power_baseline(power, baseline_windows = c(-1, 0),
#'                      method = "decibel", filebase = tempfile)
#' collapsed_power <- collapse2(bl, keep = c(2,1))
#' # Visualize
#' dname <- dimnames(bl)
#' image(collapsed_power, x = dname$Time, y = dname$Frequency,
#'       xlab = "Time (s)", ylab = "Frequency (Hz)",
#'       main = "Mean power over trial (Baseline: -1~0 seconds)",
#'       sub = glue('Electrode {e$number} (Reference: {ref$number})'))
#' abline(v = 0, lty = 2, col = 'blue')
#' text(x = 0, y = 20, "Audio onset", col = "blue", cex = 0.6)
#'
#' # clear cache on hard disk
#' e$clear_cache()
#' ref$clear_cache()
#'
#' }
#'
#' @export
LFP_electrode <- R6::R6Class(
  classname = 'LFP_electrode',
  inherit = RAVEAbstarctElectrode,
  portable = FALSE,
  lock_class = TRUE,
  private = list(
    .type = 'LFP',
    .location = 'iEEG',
    .is_reference = FALSE,
    .power_enabled = TRUE,
    check_dimensions = function(type = c("voltage", "power", "phase", "wavelet-coefficient")){
      type <- match.arg(type)
      # Check time-points
      if(type == "voltage"){
        srate <- self$raw_sample_rate
        freq <- NULL
      } else {
        srate <- self$power_sample_rate
        freq <- self$subject$meta_data("frequencies")
        if(!is.data.frame(freq) || !length(freq) || !length(freq[[1]])){
          stop("Frequency length is zero. Have you applied time-frequency decomposition?")
        }
      }
      tidx <- unlist(lapply(self$trial_intervals, function(x){
        x <- round(x * srate)
        seq(x[1], x[2])
      }))
      stopifnot2(length(tidx), msg = "Trial window has length of 0")

      # Check blocks
      epoch_tbl <- self$epoch$table
      blocks <- unique(epoch_tbl$Block)
      if(!all(blocks %in% self$subject$blocks)){
        blocks <- blocks[!blocks %in% self$subject$blocks]
        stop("Some blocks cannot be found: ", paste(blocks, collapse = ", "))
      }
      list(
        tidx = tidx,
        srate = srate,
        epoch_tbl = epoch_tbl,
        blocks = blocks,
        freq = freq
      )
    }
  ),
  public = list(

    #' @description print electrode summary
    print = function(){
      cat("<Electrode>\n")
      cat(sprintf("  Project: %s\n", self$subject$project_name))
      cat(sprintf("  Subject: %s\n", self$subject$subject_code))
      cat(sprintf("  Electrode number: %s\n", self$number))
      cat(sprintf("  Referenced against: %s\n", self$reference_name))
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
      if(!length(reference)){
        return(super$set_reference(NULL))
      }
      if(is.character(reference)){
        if(reference == "noref"){
          return(super$set_reference(NULL))
        }
        ref_inst <- LFP_reference$new(subject = self$subject, number = reference)
        if(!ref_inst$exists){
          stop("Cannot find reference: ", reference)
        }
        return(super$set_reference(ref_inst))
      }
      if(!inherits(reference, "LFP_reference")){
        stop("Reference must be a valid `LFP_reference` instance or characters")
      }
      if(!reference$exists){
        stop("Cannot find reference: ", reference$number)
      }
      return(super$set_reference(reference))

    },

    #' @description constructor
    #' @param subject,number,quiet see constructor in
    #' \code{\link{RAVEAbstarctElectrode}}
    initialize = function(subject, number, quiet = FALSE){
      super$initialize(subject, number)

      has_power <- file.exists(self$power_file)
      has_phase <- file.exists(self$phase_file)
      has_volt <- file.exists(self$voltage_file)
      if(!all(has_power, has_phase, has_volt)){
        if(!quiet) {
          catgl("Electrode {self$number} is missing {ifelse(has_power, '', 'power')}{ifelse(has_phase, '', ', phase')}{ifelse(has_volt, '', ', voltage')} data\n", level = "WARNING")
        }
      }
    },

    #' @description load non-referenced wavelet coefficients (internally used)
    #' @param reload whether to reload cache
    #' @return if the reference number if \code{NULL} or \code{'noref'}, then
    #' returns 0, otherwise returns a \code{\link[filearray]{FileArray-class}}
    .load_noref_wavelet = function(reload = FALSE){

      check_res <- private$check_dimensions("wavelet-coefficient")

      arr_path <- file.path(self$cache_root, "noref", "wavelet-coefficient")
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
      dir_create2(dirname(arr_path))

      tidx <- check_res$tidx
      srate <- check_res$srate
      epoch_tbl <- check_res$epoch_tbl
      blocks <- check_res$blocks
      freq <- check_res$freq

      nfreq <- nrow(freq)
      ntrial <- nrow(epoch_tbl)
      ntime <- length(tidx)

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

      check_res <- private$check_dimensions("voltage")
      arr_path <- file.path(self$cache_root, "noref", "voltage")

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

      dir_create2(dirname(arr_path))

      tidx <- check_res$tidx
      srate <- check_res$srate
      epoch_tbl <- check_res$epoch_tbl
      blocks <- check_res$blocks
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

      private$check_dimensions(type = "wavelet-coefficient")
      arr_path <- file.path(self$cache_root, self$reference_name, type)
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

      noref_e <- self$.load_noref_wavelet()

      if(!inherits(self$reference, "LFP_reference") || self$reference_name == "noref"){
        ref_e <- 0
        no_reference <- TRUE
        if( type == "wavelet-coefficient" ){
          return(noref_e)
        }
      } else {
        ref_e <- self$reference$.load_noref_wavelet()
        no_reference <- FALSE
      }

      dir_create2(dirname(arr_path))
      dim <- dim(noref_e)
      arr <- filearray::filearray_create(
        filebase = arr_path,
        dimension = dim,
        type = ifelse(type == "wavelet-coefficient", "complex", "float"),
        partition_size = 1
      )
      arr$set_header("rave_data_type", type, save = FALSE)
      dimnames(arr) <- dimnames(noref_e)

      # noref_e

      f <- switch (
        type,
        "power" = {
          if(no_reference){
            filearray::fmap(list(noref_e), function(v){
              Mod(v[[1]])^2
            }, .y = arr)
          } else {
            filearray::fmap(list(noref_e, ref_e), function(v){
              Mod(v[[1]] - v[[2]])^2
            }, .y = arr)
          }
        },
        "phase" = {
          if(no_reference){
            filearray::fmap(list(noref_e), function(v){
              Arg(v[[1]])
            }, .y = arr)
          } else {
            filearray::fmap(list(noref_e, ref_e), function(v){
              Arg(v[[1]] - v[[2]])
            }, .y = arr)
          }
        },
        "wavelet-coefficient" = {
          if(no_reference){
            return(noref_e)
          } else {
            filearray::fmap(list(noref_e, ref_e), function(v){
              v[[1]] - v[[2]]
            }, .y = arr)
          }
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
      # self$.load_noref_voltage(reload = reload)

      check_res <- private$check_dimensions(type = "voltage")
      arr_path <- file.path(self$cache_root, self$reference_name, "voltage")
      arr <- filearray_checkload_or_remove(
        filebase = arr_path, mode = "readonly",
        symlink_ok = FALSE, valid = TRUE,
        rave_data_type = "voltage"
      )
      if(inherits(arr, "FileArray")){
        return(arr)
      }

      noref_e <- self$.load_noref_voltage()
      if(self$reference_name == "noref"){
        return(noref_e)
      }
      ref_e <- self$reference$.load_noref_voltage()

      tidx <- check_res$tidx
      srate <- check_res$srate
      epoch_tbl <- check_res$epoch_tbl
      blocks <- check_res$blocks

      ntrial <- nrow(epoch_tbl)
      ntime <- length(tidx)

      # when noref and type=="wavelet-coefficient"
      dim <- dim(noref_e)
      arr <- filearray_create2(
        filebase = arr_path,
        dimension = dim,
        type = "double",
        partition_size = 1,
        dimnames = dimnames(noref_e)
      )
      arr$set_header("rave_data_type", "voltage", save = FALSE)

      filearray::fmap(list(noref_e, ref_e), function(input){
        input[[1]] - input[[2]]
      }, .y = arr)

      arr$set_header("valid", TRUE)
      arr$.mode <- "readonly"

      arr
    },

    #' @description load raw voltage (no process)
    #' @param reload whether to reload cache
    .load_raw_voltage = function(reload = FALSE){

      # subject <- raveio::as_rave_subject("devel/PAV007")
      # self <- raveio::new_electrode(subject, 14)
      # private <- self$.__enclos_env__$private
      # self$trial_intervals <- c(-1, 2)
      # self$epoch <- raveio:::RAVEEpoch$new(subject, "stimulation")

      check_res <- private$check_dimensions(type = "voltage")

      # get array dimensions
      tidx <- as.integer(check_res$tidx)
      srate <- check_res$srate
      epoch_tbl <- check_res$epoch_tbl
      blocks <- check_res$blocks
      ntrial <- nrow(epoch_tbl)
      ntime <- length(tidx)

      arr_path <- file.path(self$cache_root, "noref", "raw-voltage")
      if(reload && dir.exists(arr_path)) {
        unlink(arr_path, recursive = TRUE)
      }
      arr <- filearray::filearray_load_or_create(
        filebase = arr_path, type = "float", symlink_ok = FALSE,
        partition_size = 1L, verbose = FALSE, mode = "readwrite",
        sample_rate = srate, n_time_points = ntime,
        tidx_start = tidx[[1]], blocks = blocks, n_trials = ntrial,
        dimension = c(ntime, ntrial, 1),
        rave_data_type = "raw-voltage",
        on_missing = function(arr) {
          dimnames(arr) <- list(
            Time = tidx / srate,
            Trial = sort(epoch_tbl$Trial),
            Electrode = self$number
          )
        }
      )
      if(!isTRUE(arr$get_header("valid"))) {
        for(b in blocks) {
          sel <- epoch_tbl$Block == b
          if(!any(sel)){ next }
          trials <- epoch_tbl$Trial[sel]
          onsets <- epoch_tbl$Time[sel]
          tp <- sapply(onsets, function(o){
            idx <- round(o * srate)
            idx + tidx
          })

          h5_name <- sprintf('/raw/%s', b)
          block_data <- load_h5(file = self$preprocess_file, name = h5_name, ram = HDF5_EAGERLOAD)
          voltage <- block_data[tp]
          dim(voltage) <- dim(tp)
          arr[,trials,1] <- voltage
        }
      }
      arr$set_header("valid", TRUE)
      arr$.mode <- "readonly"
      arr
    },


    #' @description method to load electrode data
    #' @param type data type such as \code{"power"}, \code{"phase"},
    #' \code{"voltage"}, \code{"wavelet-coefficient"}, and
    #' \code{"raw-voltage"}. For \code{"power"}, \code{"phase"},
    #' and \code{"wavelet-coefficient"}, 'Wavelet' transforms are required.
    #' For \code{"voltage"}, 'Notch' filters must be applied. All these
    #' types except for \code{"raw-voltage"} will be referenced.
    #' For \code{"raw-voltage"}, no reference will be performed since the data
    #' will be the "raw" signal (no processing).
    load_data = function(type = c(
      "power", "phase", "voltage", "wavelet-coefficient",
      "raw-voltage"
    )){

      type <- match.arg(type)
      switch(
        type,
        "raw-voltage" = {
          self$.load_raw_voltage()
        },
        "voltage" = {
          self$.load_voltage()
        },
        {
          self$.load_wavelet(type)
        }
      )

    },

    #' @description load electrode block-wise data (with no reference),
    #' useful when epoch is absent
    #' @param blocks session blocks
    #' @param type data type such as \code{"power"}, \code{"phase"},
    #' \code{"voltage"}, \code{"raw-voltage"} (with no filters applied, as-is
    #' from imported), \code{"wavelet-coefficient"}. Note that if type
    #' is \code{"raw-voltage"}, then the data only needs to be imported;
    #' for \code{"voltage"} data, 'Notch' filters must be applied; for
    #' all other types, 'Wavelet' transforms are required.
    #' @param simplify whether to simplify the result
    #' @return If \code{simplify} is enabled, and only one block is loaded,
    #' then the result will be a vector (\code{type="voltage"}) or a matrix
    #' (others), otherwise the result will be a named list where the names
    #' are the blocks.
    load_blocks = function(blocks, type = c("power", "phase", "voltage", "wavelet-coefficient", "raw-voltage"),
                           simplify = TRUE) {
      type <- match.arg(type)
      if(!length(blocks)) {
        if(simplify){ return(NULL) }
        return(list())
      }
      stopifnot2(all(blocks %in% self$subject$blocks),
                 msg = "Electrode `load_blocks`: all blocks must exist")

      sel <- self$subject$electrodes %in% self$number
      imported <- self$subject$preprocess_settings$data_imported[sel]
      if(!isTRUE(imported)) {
        stop("load_blocks: please import electrode ", self$number, " first.")
      }

      if(type == "raw-voltage") {
        dat <- structure(lapply(blocks, function(block){
          load_h5(self$preprocess_file,
                  name = sprintf("/raw/%s", block),
                  ram = TRUE)
        }), names = blocks)
        if(simplify && length(blocks) == 1) {
          dat <- dat[[1]]
        }
        return(dat)
      }


      # check whether notch filtered
      notch_filtered <- self$subject$notch_filtered[sel]
      has_wavelet <- self$subject$has_wavelet[sel]

      if(type == "voltage" && !isTRUE(notch_filtered)) {
        stop("load_blocks: please apply notch filters to electrode ", self$number, " first.")
      }
      if(type != "voltage" && !isTRUE(has_wavelet)) {
        stop("load_blocks: please apply wavelets to electrode ", self$number, " first.")
      }

      has_reference <- !is.null(self$reference) && !self$reference$number %in% c('noref', '')

      if(type == "voltage") {
        dat <- load_blocks_voltage_single(self = self, blocks = blocks)
        if(has_reference) {
          ref <- self$reference$load_blocks(blocks = blocks, simplify = FALSE, type = "voltage")
          for(block in blocks) {
            dat[[block]] <- dat[[block]] - ref[[block]]
          }
        }
      } else {

        if(has_reference) {
          ref <- self$reference$load_blocks(blocks = blocks, simplify = FALSE, type = "wavelet-coefficient")
          dat <- load_blocks_wavelet_single(self = self, blocks = blocks, type = "wavelet-coefficient")
          for(block in blocks) {
            dat[[block]] <- dat[[block]] - ref[[block]]
            if(type == "power") {
              dat[[block]] <- Mod(dat[[block]])^2
            } else if (type == "phase") {
              dat[[block]] <- Arg(dat[[block]])
            }
          }
        } else {
          dat <- load_blocks_wavelet_single(self = self, blocks = blocks, type = type)
        }
      }

      if(simplify && length(blocks) == 1) {
        dat <- dat[[1]]
      }

      return(dat)

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

    #' @field h5_fname 'HDF5' file name
    h5_fname = function(){
      sprintf('%s.h5', self$number)
    },

    #' @field valid whether current electrode is valid: subject exists and
    #' contains current electrode or reference; subject electrode type matches
    #' with current electrode type
    valid = function(){
      if(!self$exists) {return(FALSE)}
      elec <- self$subject$electrodes
      if(!self$number %in% elec){ return(FALSE) }
      # type matches with subject
      if(!isTRUE(self$subject$electrode_types[elec %in% self$number] == self$type)){
        return(FALSE)
      }
      return(TRUE)
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
      self$subject$preprocess_settings$electrode_info(electrode = self$number)
    },

    #' @field power_file path to power 'HDF5' file
    power_file = function(){
      super$power_file
    },

    #' @field phase_file path to phase 'HDF5' file
    phase_file = function(){
      super$phase_file
    },

    #' @field voltage_file path to voltage 'HDF5' file
    voltage_file = function(){
      super$voltage_file
    }

  )
)



# self = LFP_electrode$new('demo/DemoSubject', 14); self$trial_intervals <- c(-1,2); self$set_epoch("auditory_onset")
# self$load_data('wave')
# self$load_data('volt')
