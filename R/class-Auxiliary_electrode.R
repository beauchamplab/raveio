#' Class definition for auxiliary channels
#' @export
Auxiliary_electrode <- R6::R6Class(
  classname = 'Auxiliary_electrode',
  inherit = RAVEAbstarctElectrode,
  portable = FALSE,
  lock_class = TRUE,
  private = list(
    .type = 'Auxiliary',
    .location = 'Others',
    .is_reference = FALSE,
    .power_enabled = FALSE,
    check_dimensions = function(type = c("voltage")){
      # always voltage
      type <- match.arg(type)

      # Check time-points
      srate <- self$raw_sample_rate

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

      stitch_events <- self$stitch_events
      if(length(stitch_events) == 2) {
        stitch_events_pre <- self$epoch$get_event_colname(stitch_events[[1]])
        stitch_events_post <- self$epoch$get_event_colname(stitch_events[[2]])
      } else {
        stitch_events_pre <- "Time"
        stitch_events_post <- "Time"
      }

      list(
        tidx = tidx,
        tidx_positive = tidx > 0,
        srate = srate,
        epoch_tbl = epoch_tbl,
        stitch_columns = c(stitch_events_pre, stitch_events_post),
        blocks = blocks
      )
    }
  ),
  public = list(

    #' @description print electrode summary
    print = function(){
      cat("<Auxiliary channel>\n")
      cat(sprintf("  Project: %s\n", self$subject$project_name))
      cat(sprintf("  Subject: %s\n", self$subject$subject_code))
      cat(sprintf("  Channel number: %s\n", self$number))
      cat(sprintf("  Referenced against: %s\n", self$reference_name))
      cat("  Location type:", self$location, "\n")
      cat("  Signal type:", self$type, "\n")
      cat("  Sample rates:\n")
      cat("    - Analog-trace (voltage):", self$raw_sample_rate, "\n")
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
      super$set_reference(NULL)
    },

    #' @description constructor
    #' @param subject,number,quiet see constructor in
    #' \code{\link{RAVEAbstarctElectrode}}
    initialize = function(subject, number, quiet = FALSE){
      super$initialize(subject, number)
      has_volt <- file.exists(self$voltage_file) || file.exists(self$preprocess_file)
      if( !has_volt && !quiet ) {
        catgl("Auxiliary channel {self$number} is missing {ifelse(has_volt, '', ', voltage')} data\n", level = "WARNING")
      }
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
      tidx_positive <- check_res$tidx_positive
      stitch_columns <- check_res$stitch_columns

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
        Trial = epoch_tbl$Trial,
        Electrode = self$number
      )

      for(b in blocks){
        sel <- epoch_tbl$Block == b
        if(!any(sel)){
          next
        }

        trials <- which(sel)
        # onsets <- epoch_tbl$Time[sel]
        # tp <- sapply(onsets, function(o){
        #   idx <- round(o * srate)
        #   idx + tidx
        # })
        onsets1 <- epoch_tbl[[ stitch_columns[[1]] ]][sel]
        onsets2 <- epoch_tbl[[ stitch_columns[[2]] ]][sel]
        tp <- apply(cbind(onsets1, onsets2), 1L, function(o){
          idx <- round(o * srate)
          re <- tidx
          re[!tidx_positive] <- re[!tidx_positive] + idx[[1]]
          re[tidx_positive] <- re[tidx_positive] + idx[[2]]
          re
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
            # Load notch filtered signals, or simply raw
            if(isTRUE(self$subject$notch_filtered[self$subject$electrodes %in% self$number])) {
              h5_name <- sprintf('/notch/%s', b)
            } else {
              h5_name <- sprintf('/raw/%s', b)
            }
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

    #' @description load raw voltage (no process)
    #' @param reload whether to reload cache
    .load_raw_voltage = function(reload = FALSE){

      # DIPSAUS DEBUG START
      # subject <- raveio::as_rave_subject("test/DemoSubject")
      # self <- raveio::new_electrode(subject, 13)
      # private <- self$.__enclos_env__$private
      # self$trial_intervals <- c(-1, 2)
      # self$epoch <- raveio:::RAVEEpoch$new(subject, "auditory_onset")

      check_res <- private$check_dimensions(type = "voltage")

      # get array dimensions
      tidx <- as.integer(check_res$tidx)
      srate <- check_res$srate
      epoch_tbl <- check_res$epoch_tbl
      blocks <- check_res$blocks
      ntrial <- nrow(epoch_tbl)
      ntime <- length(tidx)
      tidx_positive <- check_res$tidx_positive
      stitch_columns <- check_res$stitch_columns

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
            Trial = epoch_tbl$Trial,
            Electrode = self$number
          )
        }
      )
      if(!isTRUE(arr$get_header("valid"))) {
        for(b in blocks) {
          sel <- epoch_tbl$Block == b
          if(!any(sel)){ next }
          trials <- which(sel)
          # onsets <- epoch_tbl$Time[sel]
          # tp <- sapply(onsets, function(o){
          #   idx <- round(o * srate)
          #   idx + tidx
          # })
          onsets1 <- epoch_tbl[[ stitch_columns[[1]] ]][sel]
          onsets2 <- epoch_tbl[[ stitch_columns[[2]] ]][sel]
          tp <- apply(cbind(onsets1, onsets2), 1L, function(o){
            idx <- round(o * srate)
            re <- tidx
            re[!tidx_positive] <- re[!tidx_positive] + idx[[1]]
            re[tidx_positive] <- re[tidx_positive] + idx[[2]]
            re
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
    load_data = function(type = c("raw-voltage", "voltage")){

      type <- match.arg(type)
      switch(
        type,
        "voltage" = {
          self$.load_noref_voltage()
        },
        {
          self$.load_raw_voltage()
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
    #' @returns If \code{simplify} is enabled, and only one block is loaded,
    #' then the result will be a vector (\code{type="voltage"}) or a matrix
    #' (others), otherwise the result will be a named list where the names
    #' are the blocks.
    load_blocks = function(blocks, type = c("raw-voltage", "voltage"), simplify = TRUE) {
      type <- match.arg(type)
      if(!length(blocks)) {
        if(simplify){ return(NULL) }
        return(list())
      }
      stopifnot2(all(blocks %in% self$subject$blocks),
                 msg = "Channel `load_blocks`: all blocks must exist")

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

      if(type == "voltage" && !isTRUE(notch_filtered)) {
        stop("load_blocks: please apply notch filters to electrode ", self$number, " first.")
      }

      dat <- load_blocks_voltage_single(self = self, blocks = blocks)

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

    #' @field preprocess_info preprocess information
    preprocess_info = function(){
      self$subject$preprocess_settings$electrode_info(electrode = self$number)
    },

    #' @field voltage_file path to voltage 'HDF5' file
    voltage_file = function(){
      super$voltage_file
    }

  )
)
