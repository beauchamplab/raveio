#' Class definition to load data from 'BlackRock' 'Micro-systems' files
#' @description Currently only supports minimum file specification version
#' \code{2.3}. Please contact the package maintainer or 'RAVE' team
#' if older specifications are needed
#' @export
BlackrockFile <- R6::R6Class(
  classname = "BlackrockFile",
  portable = TRUE,

  private = list(
    .NS_MAX = 6L,

    .path = character(0L),
    .filebase = character(0L),

    .nev_file = character(0L),
    .nev = NULL,

    .ns1_file = character(0L),
    .ns1_data = NULL,

    .ns2_file = character(0L),
    .ns2_data = NULL,

    .ns3_file = character(0L),
    .ns3_data = NULL,

    .ns4_file = character(0L),
    .ns4_data = NULL,

    .ns5_file = character(0L),
    .ns5_data = NULL,

    .ns6_file = character(0L),
    .ns6_data = NULL,

    .initialize = function(path, header_only = TRUE, force = FALSE, verbose = FALSE, nev_data = TRUE) {

      # Get parent directory and file prefix
      path <- gsub(sprintf("\\.(ccf|nev|ns[1-%d])$", private$.NS_MAX),
                   '', path[[1]], ignore.case = TRUE)
      dir_path <- normalizePath(dirname(path), mustWork = TRUE)
      filebase <- filenames(path)

      fs <- list.files(dir_path, pattern = sprintf("\\.(nev|ns[1-%d])$", private$.NS_MAX),
                       ignore.case = TRUE, full.names = FALSE,
                       all.files = TRUE, recursive = FALSE,
                       include.dirs = FALSE)

      is_nev <- tolower(fs) == sprintf("%s.nev", tolower(filebase))

      nsx <- seq_len(private$.NS_MAX)
      is_nsx <- tolower(fs) %in% sprintf("%s.ns%.0f", tolower(filebase), nsx)
      nev_path <- file.path(dir_path, fs[is_nev])
      nsx_files <- file.path(dir_path, fs[is_nsx])

      if(length(nev_path) == 0) {
        stop("Cannot find any .nev file with this path: ", path)
      }

      private$.path <- dir_path
      private$.filebase <- filebase
      private$.nev_file <- fs[is_nev][[1]]
      private$.nev <- NULL

      for(i in seq_len(private$.NS_MAX)) {
        private[[sprintf(".ns%d_file", i)]] <- character(0L)
        private[[sprintf(".ns%d_data", i)]] <- NULL
      }


      lapply(fs[is_nsx], function(fname) {
        nm <- unlist(strsplit(fname, "\\."))
        ext <- tolower(nm[[length(nm)]])
        private[[sprintf(".%s_file", ext)]] <- fname
      })

      # load nev
      headers <- read_nsx_nev(paths = nsx_files, nev_path = nev_path,
                              header_only = header_only, verbose = verbose,
                              ram = FALSE, force_update = force,
                              nev_data = nev_data)

      # save nev data
      if(nev_data || !inherits(private$.nev, "fastmap2")) {
        private$.nev <- headers$nev
      } else {
        private$.nev$data_packets <- headers$nev$data_packets
        private$.nev$basic_header <- headers$nev$basic_header
      }


      # save nsx data
      for(fname in names(headers$nsx)) {
        nm <- unlist(strsplit(fname, "\\."))
        ext <- tolower(nm[[length(nm)]])
        private[[sprintf(".%s_data", ext)]] <- headers$nsx[[fname]]
      }

      return()

    },

    .has_data = function() {
      sapply(paste0("ns", seq_len(private$.NS_MAX)), function(nm) {
        nsx <- private[[sprintf(".%s_data", nm)]]
        if(!length(nsx)) {
          return(FALSE)
        }
        return(inherits(nsx$data, "FileArray"))
      }, simplify = TRUE, USE.NAMES = TRUE)
    },

    .electrode_ids = function() {
      has_nsx <- self$has_nsx
      nms <- names(has_nsx)[has_nsx]

      time_origin <- private$.nev$basic_header$time_origin$value

      re <- lapply(nms, function(nm) {
        header <- private[[sprintf(".%s_data", nm)]]
        if(!length(header$ext_header$CC) || !nrow(header$ext_header$CC)) {
          return()
        }
        global_srate <- header$basic_header$time_resolution_timestamp$value
        channel_srate <- global_srate / header$basic_header$period$value
        # TODO: what if nsx is created the next day of nev? that's weird...
        start_time <- sum((
          header$basic_header$time_origin$value - time_origin
        ) * c(
          0, 0, 0, 0, 3600000, 60000, 1000, 1
        )) / 1000

        # Already tried to convert to uV
        data.frame(
          Electrode = header$ext_header$CC$electrode_id,
          Label = header$ext_header$CC$electrode_label,
          SampleRate = channel_srate,
          NSType = nm,
          NSOrder = seq_along(header$ext_header$CC$electrode_id),
          TimeStart = start_time,
          stringsAsFactors = FALSE
        )
      })
      re <- dipsaus::drop_nulls(re)
      do.call("rbind", re)
    }

  ),

  public = list(

    #' @field block character, session block ID
    block = character(),

    #' @description print user-friendly messages
    print = function() {
      has_nsx <- self$has_nsx
      available_nsx <- names(has_nsx)[has_nsx]

      etable <- self$electrode_table
      sample_rates <- self$sample_rates
      recording_duration <- self$recording_duration
      epoch <- self$get_epoch()

      nsx_info <- lapply(available_nsx, function(nm) {
        sel <- etable$NSType == nm
        elec <- etable$Electrode[sel]
        sprintf(
          "  - %s: sample rate [%.1f Hz], duration [%.2f sec], electrode [%s, n=%d]\n",
          nm,
          sample_rates[[nm]], recording_duration[[nm]],
          dipsaus::deparse_svec(elec), length(elec)
        )
      })


      cat(c(
        "BlackRock Micro-systems file: [", private$.filebase, ']\n',
        "Directory: ", private$.path, "\n",
        "Version: ", paste0(sprintf("%.0f", self$version), collapse = "."), "\n",
        "Block: ", self$block, "\n",
        "# of comments/trial: ", ifelse(is.data.frame(epoch), nrow(epoch), "N/A"), "\n",
        "Available NSx: ", paste0("ns", seq_len(private$.NS_MAX)[self$has_nsx], collapse = ", "), "\n",
        unlist(nsx_info)
      ), sep = "")
    },

    #' @description constructor
    #' @param path the path to 'BlackRock' file, can be with or without file
    #' extensions
    #' @param block session block ID; default is the file name
    #' @param nev_data whether to load comments and 'waveforms'
    initialize = function(path, block, nev_data = TRUE) {
      if(missing(block)) {
        block <- filenames(path)
        block <- gsub("\\.(nev|ns[0-9]|ccf)$", "", block, ignore.case = TRUE)
      }
      self$block <- block
      private$.initialize(path, nev_data = nev_data)
    },

    #' @description get 'NEV' file path
    #' @return absolute file path
    nev_path = function() {
      file.path(private$.path, private$.nev_file)
    },

    #' @description get 'NSx' file paths
    #' @param which which signal file to get, or \code{NULL} to return all
    #' available paths, default is \code{NULL}; must be integers
    #' @return absolute file paths
    nsx_paths = function(which = NULL) {
      which <- as.integer(which)
      if(!length(which)) {
        which <- which(self$has_nsx)
      } else if (any(is.na(which))) {
        stop("$nsx_paths: parameter `which` must be an integer")
      }
      sapply(sprintf("ns%d", which), function(nm) {
        file.path(private$.path, private[[sprintf(".%s_file", nm)]])
      }, simplify = FALSE, USE.NAMES = TRUE)
    },

    #' @description refresh and load 'NSx' data
    #' @param force whether to force reload data even if the data has been
    #' loaded and cached before
    #' @param verbose whether to print out messages when loading
    #' @param nev_data whether to refresh 'NEV' extended data; default is false
    #' @return nothing
    refresh_data = function(force = FALSE, verbose = TRUE, nev_data = FALSE) {
      private$.initialize(self$base_path, header_only = FALSE,
                          force = force, verbose = verbose, nev_data = nev_data)
      invisible()
    },

    #' @description get epoch table from the 'NEV' comment data packet
    #' @return a data frame
    get_epoch = function() {

      srate <- self$sample_rate_nev_timestamp

      re <- lapply(as.list(private$.nev$data_packets), function(packet) {
        if(!identical(packet$value$event, "comment")) {
          return(NULL)
        }
        # extract comment, usually epoch
        data.frame(
          Block = self$block,
          Time = packet$value$timestamp / srate,
          Condition = packet$value$comment,
          stringsAsFactors = FALSE
        )
      })
      re <- dipsaus::drop_nulls(re)
      if(!length(re)) {
        return(data.frame(
          Block = character(0L),
          Time = numeric(0L),
          Condition = character(0L),
          stringsAsFactors = FALSE
        ))
      }
      re <- do.call("rbind", re)
      re
    },

    #' @description get 'waveform' of the spike data
    #' @return a list of spike 'waveform' (without normalization)
    get_waveform = function() {
      srate_timestamp <- self$sample_rate_nev_timestamp
      wave_table <- private$.nev$extended_header$NEUEVWAV
      srate_waveform <- private$.nev$basic_header$time_resolution_samples$value
      re <- lapply(as.list(private$.nev$data_packets), function(packet) {
        if(!identical(packet$value$event, "spike")) {
          return(NULL)
        }
        re <- packet$value
        re$time <- re$timestamp / srate_timestamp
        re$electrode <- re$packet_id

        sel <- wave_table$electrode_id == re$electrode
        if(!any(sel)) {
          return(NULL)
        }
        spike_width <- wave_table$spike_width[sel][[1]]
        re$sample_rate <- srate_waveform
        re$duration <- spike_width / srate_waveform

        re$timestamp <- NULL
        re$packet_id <- NULL
        re$reserved <- NULL
        re$event <- NULL
        re
      })
      re <- dipsaus::drop_nulls(re)
      attr(re, "NEUEVWAV") <- wave_table
      re
    },

    #' @description get electrode data
    #' @param electrode integer, must be a length of one
    #' @param nstype which signal bank, for example, \code{'ns3'}, \code{'ns5'}
    #' @return a normalized numeric vector (analog signals with \code{'uV'}
    #' as the unit)
    get_electrode = function(electrode, nstype = NULL) {

      if(length(electrode) != 1) {
        stop("$get_electrode: electrode length must be one")
      }
      elec_table <- private$.electrode_ids()
      sel <- elec_table$Electrode %in% electrode
      if(length(nstype)) {
        sel <- sel & elec_table$NSType %in% nstype
      }
      sel <- which(sel)

      if(!length(sel)) {
        stop("$get_electrode: electrode cannot be found [", electrode, "]")
      }

      sel <- sel[[1]]

      ns_type <- elec_table$NSType[[sel]]
      ns_order <- elec_table$NSOrder[[sel]]
      if(!private$.has_data()[[ns_type]]) {
        self$refresh_data(verbose = FALSE)
      }

      header <- private[[sprintf(".%s_data", ns_type)]]
      re <- header$data[, ns_order]
      attr(re, "meta") <- elec_table[sel, ]
      attr(re, "unit") <- "uV"
      re
    }

  ),

  active = list(

    #' @field base_path absolute base path to the file
    base_path = function() {
      file.path(private$.path, private$.filebase)
    },

    #' @field version 'NEV' specification version
    version = function() {
      private$.nev$basic_header$file_spec$value
    },

    #' @field electrode_table electrode table
    electrode_table = function() {
      private$.electrode_ids()
    },

    #' @field sample_rate_nev_timestamp sample rate of 'NEV' data packet
    #' time-stamps
    sample_rate_nev_timestamp = function() {
      re <- private$.nev$basic_header$time_resolution_timestamp$value
      if(!length(re)) {
        re <- NA
      }
      re[[1]]
    },

    #' @field has_nsx named vector of 'NSx' availability
    has_nsx = function() {
      nms <- paste0("ns", seq_len(private$.NS_MAX))
      structure(
        sapply(nms, function(nm) {
          length(private[[sprintf(".%s_file", nm)]]) == 1
        }, simplify = TRUE, USE.NAMES = TRUE),
        names = nms
      )
    },

    #' @field recording_duration recording duration of each 'NSx'
    recording_duration = function() {
      has_nsx <- self$has_nsx
      nms <- names(has_nsx)[has_nsx]
      sapply(nms, function(nm) {
        header <- private[[sprintf(".%s_data", nm)]]
        ntp <- header$data_header$data_header$value$number_of_data_points
        srate <- header$basic_header$time_resolution_timestamp$value /
          header$basic_header$period$value
        ntp / srate
      })
    },

    #' @field sample_rates sampling frequencies of each 'NSx' file
    sample_rates = function() {
      has_nsx <- self$has_nsx
      nms <- names(has_nsx)[has_nsx]
      sapply(nms, function(nm) {
        header <- private[[sprintf(".%s_data", nm)]]
        header$basic_header$time_resolution_timestamp$value /
          header$basic_header$period$value
      })
    }

  )
)


#' Convert 'BlackRock' 'NEV/NSx' files
#' @param file path to any 'NEV/NSx' file
#' @param block the block name, default is file name
#' @param subject subject code to save the files; default is \code{NULL}
#' @param to save to path, must be a directory; default is under the file path.
#' If \code{subject} is provided, then the default is \code{subject} raw
#' directory path
#' @param comments whether to extract comment section as epoch; default is true
#' @param format output format, choices are \code{'mat'} or \code{'hdf5'}
#' @return The results will be stored in directory specified by \code{to}.
#' Please read the output message carefully.
#' @export
convert_blackrock <- function(
    file, block = NULL, subject = NULL, to = NULL, comments = TRUE,
    format = c("mat", "hdf5")) {

  # DIPSAUS DEBUG START
  # file <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/YDY/block058/EMU-058_subj-YDY_task-noisyAV_run-06_NSP-2.ns5'
  # format <- "mat"
  # block <- NULL
  # to <- NULL
  # comments <- FALSE

  format <- match.arg(format)
  nev_data <- isTRUE(as.logical(comments))

  if(length(block) != 1 || !nzchar(block)) {
    block <- filenames(file)
    block <- gsub("\\.(ccf|nev|ns[1-6])", "", block, ignore.case = TRUE)
  }
  catgl("Loading NEV/NSx files...", level = "INFO")
  brfile <- BlackrockFile$new(file, block = block, nev_data = nev_data)

  electrodes <- as.integer(brfile$electrode_table$Electrode)
  catgl("Found channels: ", dipsaus::deparse_svec(electrodes), level = "INFO")

  # load blackrock file
  brfile$refresh_data(nev_data = nev_data)


  if(!length(to)) {
    if(length(subject)) {
      to <- file.path(raveio_getopt("raw_data_dir"), subject[[1]], block)
    } else {
      to <- file.path(dirname(file), block)
    }
  }
  if(dir.exists(to)) {
    backup_file(to, remove = TRUE)
  }
  to <- dir_create2(to)

  save_signal <- function(s, e) {
    # meta <- as.list(attr(s, "meta"))
    s <- as.vector(s)
    switch(
      format,
      "mat" = {
        fname <- file.path(to, sprintf("channel_%s.mat", e))
        R.matlab::writeMat(fname, data = s)
      },
      "h5" = {
        fname <- file.path(to, sprintf("channel_%s.h5", e))
        save_h5(s, file = fname, name = "data", quiet = TRUE, replace = TRUE)
      },
      {
        stop("Unsupported format")
      }
    )
  }

  dipsaus::lapply_async2(electrodes, function(e) {
    s <- brfile$get_electrode(e)
    save_signal(s, e)
    return()
  }, plan = FALSE, callback = function(e) {
    sprintf("Writing data|Electrode %s", e)
  })

  safe_write_csv(brfile$electrode_table, file = file.path(to, "channels.csv"),
                 row.names = FALSE, quiet = TRUE)

  if(nev_data) {
    epoch_table <- brfile$get_epoch()
    safe_write_csv(epoch_table, file.path(to, "epoch_nev_export.csv"),
                   row.names = FALSE, quiet = TRUE)
  }


  catgl("Done. Please check the output path: [{to}]", level = "INFO")

  return(invisible(to))
}
