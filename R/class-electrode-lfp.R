#' Definition for 'LFP' electrodes
#'
#' @examples
#' \dontrun{
#'
#' # Download demo subject KC
#'
#' # Electrode 14
#' e <- LFP_electrode$new(subject = 'demo/DemoSubject',
#'                        number = 14, is_reference = FALSE)
#'
#' # Reference "ref_13-16,24"
#' ref <- LFP_electrode$new(subject = 'demo/DemoSubject',
#'                          number = "ref_13-16,24", is_reference = TRUE)
#'
#' # ------ Reference ------
#' # By default there is no reference
#' e$reference_name     # "noref
#'
#' # set reference
#' e$set_reference(reference = ref)
#' e$reference_name     # "ref_13-16,24"
#'
#' # Set epoch
#' e$set_epoch(epoch = 'auditory_onset')
#'
#' # Now epoch power
#' power <- e$epoch_power(before_onset = 1, after_onset = 2)
#'
#' # Trial x Frequency x Time x Electrodes
#' power
#'
#' # Subset power
#' subset(power, Time ~ Time < 0, Electrode ~ Electrode == 14)
#'
#' # clear memory in RAM and cache on hard disk
#' e$clear_cache()
#' e$clear_memory()
#'
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
    persisted_voltage_unref = NULL,
    persisted_power_unref = NULL,
    persisted_phase_unref = NULL,
    persisted_coef_ref = NULL,
    referenced_power_cache_file = character(0)
  ),
  public = list(

    #' @field type type of electrode
    type = 'LFP',

    #' @description set reference for current electrode
    #' @param reference either \code{NULL} or \code{LFP_electrode} instance
    set_reference = function(reference){
      if(length(reference) && is.character(reference)){
        # check if reference is "noref"
        if(reference == "noref"){
          reference <- NULL
        } else {
          ref_e <- stringr::str_remove_all(reference, "(^ref_)|(.h5)")
          elec <- dipsaus::parse_svec(ref_e)
          if(length(elec) < 1){
            stop("Cannot find reference: ", reference)
          }
          ref_inst <- LFP_electrode$new(
            subject = self$subject, number = reference,
            is_reference = TRUE)
          if(!ref_inst$exists){
            stop("Cannot find reference: ", reference)
          }
          reference <- ref_inst
        }
      }
      self$.set_reference(reference)
    },

    #' @description constructor
    #' @param subject,number,is_reference see constructor in
    #' \code{\link{RAVEAbstarctElectrode}}
    initialize = function(subject, number, is_reference = FALSE){
      super$initialize(subject, number, is_reference)
      if(is_reference){
        # this is a reference electrode
        self$is_reference <- TRUE
        ref_electrodes <- stringr::str_match(number, 'ref_([0-9\\-,\\ ]+)')[,2]

        # no reference value, 'noref'
        if(is.na(ref_electrodes)){
          ref_electrodes <- ''
        }

        e <- dipsaus::parse_svec(ref_electrodes)
        if(length(e) == 0){
          self$number <- 'noref'
        } else {
          if(length(e) == 1){
            self$number <- e
          } else {
            # check subject reference directory
            self$number <- sprintf('ref_%s', ref_electrodes)
            if(!file.exists(file.path(self$subject$reference_path,
                                      sprintf("%s.h5", self$number)))){
              rave_warn("Reference file {self$number}.h5 is missing")
            }
          }
        }
      }

    },

    # data method

    #' @description load non-referenced wavelet coefficients (internally used)
    #' @param reload whether to reload cache
    .load_noref_wavelet = function(reload = FALSE){
      noref_cache_path <- file.path(self$cache_root, "noref")
      arr_path <- file.path(noref_cache_path, "coef")

      if(file.exists(arr_path)){
        if(reload){
          unlink(arr_path, recursive = TRUE, force = TRUE)
        } else {
          return(filearray::filearray_load(arr_path, "readonly"))
        }
      }

      dir_create2(noref_cache_path)

      srate <- self$subject$power_sample_rate
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

      freq <- self$subject$meta_data("frequencies")
      nfreq <- nrow(freq)
      ntrial <- nrow(epoch_tbl)
      ntime <- length(tidx)

      error <- TRUE
      # freq x time x trial
      arr <- filearray::filearray_create(
        arr_path,
        dimension = c(nfreq, ntime, ntrial, 1),
        type = "complex",
        partition_size = 1
      )
      on.exit({
        if(error){
          arr$delete()
        }
      })

      dimnames(arr) <- list(
        Frequency = freq$Frequency,
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

        if( self$is_reference ){
          h5_name <- sprintf('/wavelet/coef/%s', b)
          block_data <- load_h5(file = self$power_file, name = h5_name)
          coef <- block_data[,tp,]
          dim(coef) <- c(nrow(coef), dim(tp), 2)
          coef <- coef[,,,1] * exp(1i * coef[,,,2])
        } else {
          h5_name <- sprintf('/raw/power/%s', b)
          block_data <- load_h5(file = self$power_file, name = h5_name)
          power <- block_data[, tp]
          dim(power) <- c(nrow(power), dim(tp))
          h5_name <- sprintf('/raw/phase/%s', b)
          block_data <- load_h5(file = self$phase_file, name = h5_name)
          phase <- block_data[, tp]
          dim(phase) <- c(nrow(phase), dim(tp))
          coef <- sqrt(power) * exp(1i * phase)
        }
        arr[,,trials,1] <- coef
      }

      error <- FALSE
      return(arr)
    },

    #' @description load non-referenced voltage (internally used)
    #' @param srate voltage signal sample rate
    #' @param reload whether to reload cache
    .load_noref_voltage = function(srate, reload = FALSE){
      noref_cache_path <- file.path(self$cache_root, "noref")
      arr_path <- file.path(noref_cache_path, "voltage")

      if(file.exists(arr_path)){
        if(reload){
          unlink(arr_path, recursive = TRUE, force = TRUE)
        } else {
          return(filearray::filearray_load(arr_path, "readonly"))
        }
      }

      dir_create2(noref_cache_path)

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

      error <- TRUE
      # freq x time x trial
      arr <- filearray::filearray_create(
        arr_path,
        dimension = c(ntime, ntrial, 1),
        type = "complex",
        partition_size = 1
      )
      on.exit({
        if(error){
          arr$delete()
        }
      })

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

        if( self$is_reference ){
          h5_name <- sprintf('/voltage/%s', b)
          block_data <- load_h5(file = self$voltage_file, name = h5_name)
        } else {
          h5_name <- sprintf('/raw/voltage/%s', b)
          block_data <- load_h5(file = self$voltage_file, name = h5_name)
        }
        voltage <- block_data[tp]
        dim(voltage) <- dim(tp)
        arr[,trials,1] <- voltage
      }

      error <- FALSE
      return(arr)
    },

    #' @description load referenced wavelet coefficients (internally used)
    #' @param type type of data to load
    #' @param reload whether to reload cache
    .load_wavelet = function(type = c("power", "phase", "coef"),
                             reload = FALSE){
      type <- match.arg(type)
      arr_path <- file.path(self$cache_root, self$reference_name, type)

      if(dir.exists(arr_path)){
        if(reload){
          unlink(arr_path, recursive = TRUE, force = TRUE)
        } else {
          return(filearray::filearray_load(
            arr_path, "readonly"
          ))
        }
      }

      noref_arr_path <- file.path(self$cache_root, "noref", "coef")

      noref_e <- self$.load_noref_wavelet()
      ref <- 0
      if(self$reference_name != "noref"){
        ref <- self$reference$.load_noref_wavelet()
      }

      # when noref and type=="coef"
      if(dir.exists(arr_path)){
        return(filearray::filearray_load(
          arr_path, "readonly"
        ))
      }

      # reference on the fly
      error <- TRUE
      dir_create2(dirname(arr_path))
      dim <- dim(noref_e)
      arr <- filearray::filearray_create(
        filebase = arr_path,
        dimension = dim,
        type = ifelse(type == "coef", "complex", "double"),
        partition_size = 1
      )
      on.exit({
        if(error){
          arr$.mode <- "readwrite"
          arr$delete()
        }
      })

      dimnames(arr) <- dimnames(noref_e)

      f <- switch (
        type,
        "coef" = function(input){
          # reference != NULL
          input[[1]] - input[[2]]
        },
        "power" = function(input){
          if(length(input) == 1){
            Mod(input[[1]])^2
          } else {
            Mod(input[[1]] - input[[2]])^2
          }
        },
        "phase" = function(input){
          if(length(input) == 1){
            Arg(input[[1]])
          } else {
            Arg(input[[1]] - input[[2]])
          }
        }
      )

      if(length(self$reference)){
        filearray::fmap(list(noref_e, ref), f, .y = arr)
      } else {
        filearray::fmap(list(noref_e), f, .y = arr)
      }

      arr$.mode <- "readonly"
      error <- FALSE
      arr
    },

    #' @description load referenced voltage (internally used)
    #' @param reload whether to reload cache
    .load_voltage = function(reload = FALSE){
      if(self$is_reference){
        stop("`.load_voltage` is not intended to be called directly for reference electrodes")
      }
      arr_path <- file.path(self$cache_root, self$reference_name, "voltage")

      if(dir.exists(arr_path)){
        if(reload){
          unlink(arr_path, recursive = TRUE, force = TRUE)
        } else {
          return(filearray::filearray_load(
            arr_path, "readonly"
          ))
        }
      }

      srate <- self$subject$raw_sample_rates[self$subject$electrodes == self$number][[1]]

      stopifnot2(length(srate), msg = sprintf("Cannot obtain sample rate for voltage data (electrode %s)", self$number))

      noref_arr_path <- file.path(self$cache_root, "noref", "voltage")
      dir_create2(dirname(arr_path))

      noref_e <- self$.load_noref_voltage(srate = srate)
      ref <- 0
      if(self$reference_name != "noref"){
        ref <- self$reference$.load_noref_voltage(srate = srate)
      }

      # when noref and type=="coef"
      if(dir.exists(arr_path)){
        return(filearray::filearray_load(
          arr_path, "readonly"
        ))
      }

      # reference on the fly
      error <- TRUE
      dim <- dim(noref_e)
      arr <- filearray::filearray_create(
        filebase = arr_path,
        dimension = dim,
        type = "double",
        partition_size = 1
      )
      on.exit({
        if(error){
          arr$.mode <- "readwrite"
          arr$delete()
        }
      })

      dimnames(arr) <- dimnames(noref_e)

      filearray::fmap(list(noref_e, ref), function(input){
        input[[1]] - input[[2]]
      }, .y = arr)

      arr$.mode <- "readonly"
      error <- FALSE
      arr
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
        if(type == "wavelet-coefficient"){
          type <- "coef"
        }
        return(self$.load_wavelet(type))
      }

    },

    #' @description load voltage data, non-referenced
    #' @param block experiment block
    #' @param persist whether to persist in the instance, default is false,
    #' however, if this function will be called multiple times, set it to true.
    #' @return voltage data before reference
    load_unreferenced_voltage = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_voltage_unref)){
          return(private$persisted_voltage_unref)
        }
      }


      if(is.numeric(self$number)){
        # load from data
        fst_path <- file.path(self$subject$cache_path, 'voltage', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path <- file.path(self$subject$cache_path, 'voltage', 'ref', block, self$fst_fname)
        }
        h5_path <- file.path(self$subject$data_path, 'voltage', self$h5_fname)
        h5_name <- sprintf('/raw/voltage/%s', block)

        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        # load from reference folder
        h5_path <- file.path(self$subject$reference_path, self$h5_fname)
        h5_name <- sprintf('/voltage/%s', block)
        re <- load_h5(h5_path, h5_name, read_only = TRUE, ram = FALSE)
      }

      if(persist){
        private$persisted_voltage_unref <- re[]
        return(private$persisted_voltage_unref)
      }

      re

    },

    #' @description load power data, non-referenced
    #' @param block experiment block
    #' @param persist whether to persist in the instance, default is false,
    #' however, if this function will be called multiple times, set it to true.
    #' @return power data before reference
    load_unreferenced_power = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_power_unref)){
          return(private$persisted_power_unref)
        }
      }

      if(is.numeric(self$number)){
        # load from data
        fst_path <- file.path(self$subject$cache_path, 'power', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path <- file.path(self$subject$cache_path, 'power', 'ref', block, self$fst_fname)
        }
        h5_path <- file.path(self$subject$data_path, 'power', self$h5_fname)
        h5_name <- sprintf('/raw/power/%s', block)

        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        if(!persist){
          rave_fatal('load_unreferenced_power is not for reference_electrode unless persist is TRUE')
        }

        # load from reference folder
        h5_path <- file.path(self$subject$reference_path, self$h5_fname)
        h5_name <- sprintf('/wavelet/coef/%s', block)
        re <- load_h5(h5_path, h5_name, read_only = TRUE, ram = TRUE)

        re <- re[,,1, drop = FALSE] ^ 2
        dim(re) <- dim(re)[1:2]
        private$persisted_power_unref <- re
        return(re)

      }

      if(persist){
        private$persisted_power_unref <- re[]
        return(private$persisted_power_unref)
      }

      re

    },

    #' @description load phase data, non-referenced
    #' @param block experiment block
    #' @param persist whether to persist in the instance, default is false,
    #' however, if this function will be called multiple times, set it to true.
    #' @return phase data before reference
    load_unreferenced_phase = function(block, persist = FALSE){
      stopifnot2(block %in% self$subject$blocks, msg = sprintf(
        'Block %s doesn not exists', block
      ))
      stopifnot2(self$exists, msg = sprintf('Electrode %s is invalid', self$number))

      if(persist){
        if(!is.null(private$persisted_phase_unref)){
          return(private$persisted_phase_unref)
        }
      }

      if(is.numeric(self$number)){
        # load from data
        fst_path <- file.path(self$subject$cache_path, 'phase', 'raw', block, self$fst_fname)
        if(!file.exists(fst_path) && isTRUE(self$cached_reference == 'noref')){
          fst_path <- file.path(self$subject$cache_path, 'phase', 'ref', block, self$fst_fname)
        }
        h5_path <- file.path(self$subject$data_path, 'phase', self$h5_fname)
        h5_name <- sprintf('/raw/phase/%s', block)

        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        if(self$number == 'noref'){
          return(0)
        }
        if(!persist){
          rave_fatal('load_unreferenced_phase is not for reference_electrode unless persist is TRUE')
        }

        # load from reference folder
        h5_path <- file.path(self$subject$reference_path, self$h5_fname)
        h5_name <- sprintf('/wavelet/coef/%s', block)
        re <- load_h5(h5_path, h5_name, read_only = TRUE, ram = TRUE)

        re <- re[,,2, drop = FALSE]
        dim(re) <- dim(re)[1:2]
        private$persisted_phase_unref <- re
        return(re)

      }

      if(persist){
        private$persisted_phase_unref <- re[]
        return(private$persisted_phase_unref)
      }

      re

    },

    # reference

    #' @description reference power for given block
    #' @param block character, experiment block
    #' @return referenced power
    reference_power = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_power(block, persist = FALSE)[])
      }
      # check whether cached
      has_cached <- self$reference_equals_cached

      if( has_cached ){
        fst_path <- file.path(self$subject$cache_path, 'power', 'ref', block, self$fst_fname)
        h5_path <- file.path(self$subject$data_path, 'power', self$h5_fname)
        h5_name <- sprintf('/ref/power/%s', block)
        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        power <- self$load_unreferenced_power(block = block, persist = FALSE)
        phase <- self$load_unreferenced_phase(block = block, persist = FALSE)
        ref_power <- self$reference$load_unreferenced_power(block = block, persist = TRUE)
        ref_phase <- self$reference$load_unreferenced_phase(block = block, persist = TRUE)

        coef <- sqrt(power[]) * exp(phase[] * 1i) - sqrt(ref_power) * exp(ref_phase * 1i)
        re <- Mod(coef) ^ 2
      }

      re
    },

    #' @description reference phase for given block
    #' @param block character, experiment block
    #' @return referenced phase
    reference_phase = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_phase(block, persist = FALSE)[])
      }
      has_cached <- self$reference_equals_cached

      if( has_cached ){
        fst_path <- file.path(self$subject$cache_path, 'phase', 'ref', block, self$fst_fname)
        h5_path <- file.path(self$subject$data_path, 'phase', self$h5_fname)
        h5_name <- sprintf('/ref/phase/%s', block)
        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        power <- self$load_unreferenced_power(block = block, persist = FALSE)
        phase <- self$load_unreferenced_phase(block = block, persist = FALSE)
        ref_power <- self$reference$load_unreferenced_power(block = block, persist = TRUE)
        ref_phase <- self$reference$load_unreferenced_phase(block = block, persist = TRUE)

        coef <- sqrt(power[]) * exp(phase[] * 1i) - sqrt(ref_power) * exp(ref_phase * 1i)
        re <- Arg(coef)
      }

      re
    },

    #' @description reference voltage for given block
    #' @param block character, experiment block
    #' @return referenced voltage
    reference_voltage = function(block){
      # check whether the reference has been cached
      stopifnot2(!self$is_reference, msg = 'Cannot reference a reference ;D')
      if(is.null(self$reference) || isTRUE(self$reference$number == 'noref') ){
        # noref!
        return(self$load_unreferenced_voltage(block, persist = FALSE)[])
      }
      has_cached <- self$reference_equals_cached

      if( has_cached ){
        fst_path <- file.path(self$subject$cache_path, 'voltage', 'ref', block, self$fst_fname)
        h5_path <- file.path(self$subject$data_path, 'voltage', self$h5_fname)
        h5_name <- sprintf('/ref/voltage/%s', block)
        re <- load_fst_or_h5(
          fst_path = fst_path,
          h5_path = h5_path, h5_name = h5_name,
          fst_need_transpose = TRUE,
          fst_need_drop = TRUE,
          ram = FALSE
        )
      } else {
        # calculate reference by yourself
        voltage <- self$load_unreferenced_voltage(block = block, persist = FALSE)
        ref_voltage <- self$reference$load_unreferenced_voltage(block = block, persist = TRUE)
        re <- voltage-ref_voltage
      }

      re
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
      private$persisted_voltage_unref <- NULL
      private$persisted_power_unref <- NULL
      private$persisted_phase_unref <- NULL
      private$persisted_coef_ref <- NULL
      if(inherits(self$reference, 'RAVEAbstarctElectrode')){
        self$reference$clear_memory()
      }
    }

  ),
  active = list(
    #' @field exists whether electrode exists in subject
    exists = function(){
      if(!self$is_reference || is.numeric(self$number)){
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
      if(!self$exists) {return(FALSE)}
      if(self$is_reference) {return(TRUE)}
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
      elec <- self$subject$electrodes
      srate <- self$subject$raw_sample_rates[elec %in% self$number]
      if(!length(srate)){
        srate <- NA
      }
      srate
    },

    #' @field power_sample_rate power/phase sample rate
    power_sample_rate = function(){
      elec <- self$subject$electrodes
      srate <- self$subject$power_sample_rate[elec %in% self$number]
      if(!length(srate)){
        srate <- NA
      }
      srate
    },

    #' @field preprocess_info preprocess information
    preprocess_info = function(){
      self$subject$preprocess_settings$electrode_info(electrode = self$number)
    },

    #' @field power_file path to power 'HDF5' file
    power_file = function(){
      if(self$is_reference){
        return(file.path(self$subject$reference_path,
                              self$h5_fname))
      } else {
        return(super$power_file)
      }
    },

    #' @field phase_file path to phase 'HDF5' file
    phase_file = function(){
      if(self$is_reference){
        return(file.path(self$subject$reference_path,
                         self$h5_fname))
      } else {
        return(super$phase_file)
      }
    },

    #' @field voltage_file path to voltage 'HDF5' file
    voltage_file = function(){
      if(self$is_reference){
        return(file.path(self$subject$reference_path,
                         self$h5_fname))
      } else {
        return(super$voltage_file)
      }
    }

  )
)
