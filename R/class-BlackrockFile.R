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

    .initialize = function(path, header_only = TRUE, force = FALSE, verbose = FALSE) {

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
                              ram = FALSE, force_update = force)

      # save nev data
      private$.nev <- headers$nev

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

    block = character(),

    initialize = function(path, block) {
      self$block <- block
      private$.initialize(path)
    },

    nev_path = function() {
      file.path(private$.path, private$.nev_file)
    },

    nsx_paths = function(which = NULL) {
      if(is.null(which)) {
        which <- which(self$has_nsx)
      }
      sapply(sprintf("ns%.0f", which), function(nm) {
        file.path(private$.path, private[[sprintf(".%s_file", nm)]])
      }, simplify = FALSE, USE.NAMES = TRUE)
    },

    refresh_data = function(force = FALSE, verbose = TRUE) {
      private$.initialize(self$base_path, header_only = FALSE,
                          force = force, verbose = verbose)
      invisible()
    },

    load_nsx_data = function(which = NULL, force = FALSE, verbose = FALSE) {
      nsx_paths <- self$nsx_paths(which)

      for(nm in names(nsx_paths)) {
        nsx_path <- nsx_paths[[nm]]
        info <- blackrock_specification(nsx_path)
        nsx <- parse__nsx(nsx_path, info$config$specification,
                         header_only = FALSE,
                         verbose = verbose)
        # Post processing
        nsx <- blackrock_postprocess(nsx = nsx, nev = private$.nev)
        private[[sprintf(".%s_data", nm)]] <- nsx
      }

      .NotYetImplemented()
    },

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
      re <- do.call("rbind", re)
      re
    },

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

    get_electrode = function(electrode) {

      if(length(electrode) != 1) {
        stop("$get_electrode: electrode length must be one")
      }
      elec_table <- private$.electrode_ids()
      sel <- which(elec_table$Electrode %in% electrode)

      if(!length(sel)) {
        stop("$get_electrode: electrode cannot be found [", electrode, "]")
      }

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

    base_path = function() {
      file.path(private$.path, private$.filebase)
    },

    version = function() {
      private$.nev$basic_header$file_spec$value
    },

    electrode_table = function() {
      private$.electrode_ids()
    },

    sample_rate_nev_timestamp = function() {
      re <- private$.nev$basic_header$time_resolution_timestamp$value
      if(!length(re)) {
        re <- NA
      }
      re[[1]]
    },

    has_nsx = function() {
      sapply(paste0("ns", seq_len(private$.NS_MAX)), function(nm) {
        length(private[[sprintf(".%s_file", nm)]]) == 1
      }, simplify = TRUE, USE.NAMES = TRUE)
    },

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
