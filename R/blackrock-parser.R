
validate_spec <- function(name, type, size, n = 1, names = NULL, ...) {
  # check type first
  size_ <- byte_size_lut[[type]]
  if(!missing(size) && length(size) == 1) {
    if(!is.null(size_) && !size %in% size_) {
      stop("Cannot parse name [", name, "]: the type [", type, "] cannot have ", size, " bytes.")
    }
    size_ <- size
  } else if (!length(size_)) {
    stop("Cannot parse name [", name, "]: unknown size for data type: ", type)
  } else {
    size_ <- size_[[1]]
  }
  if(!length(n)) {
    n <- 1L
  } else {
    n <- as.integer(n)
    if (length(n) > 1 || any(is.na(n))) {
      stop("Element length `n` must be an integer")
    }
  }

  re <- list(...)
  re$name <- name
  re$type <- type
  re$size <- size_
  re$n <- n
  re$names <- names
  re$.bytes <- n * size_
  return(re)
}

#' @export
`print.nev-nsx-entry` <- function(x, ...) {
  cat(sprintf("[%s]: %s\n", x$name, deparse1(x$value)))
}

#' @export
`print.nev-nsx-entry-list` <- function(x, ...) {
  for(ii in seq_len(length(x))) {
    print(x[[ii]])
  }
}


parse_uint8 <- function(x, ...) {
  # rawToUInt8(x)
  ravetools::raw_to_uint8(x)
}
parse_int8 <- function(x, ...) {
  # rawToInt8(x)
  ravetools::raw_to_int8(x)
}
parse_uint16 <- function(x, ...) {
  # rawToUInt16(x)
  ravetools::raw_to_uint16(x)
}
parse_int16 <- function(x, ...) {
  # rawToInt16(x)
  ravetools::raw_to_int16(x)
}
parse_uint32 <- function(x, ...) {
  # rawToUInt32(x)
  ravetools::raw_to_uint32(x)
}
parse_int32 <- function(x, ...) {
  # rawToInt32(x)
  ravetools::raw_to_int32(x)
}
parse_uint64 <- function(x, ...) {
  # There is no R data type that can hold uint64
  # luckily, blackrock uses uint64 to store timestamp, which
  # should not exceed the half limit, and int64 should suffice
  ravetools::raw_to_int64(x)
}
parse_int64 <- function(x, ...) {
  # rawToInt64(x)
  ravetools::raw_to_int64(x)
}
parse_float <- function(x, ...) {
  # rawToInt64(x)
  ravetools::raw_to_float(x)
}

parse_string <- function(x, ...) {
  # rawToString(x)
  ravetools::raw_to_string(x)
}
parse_bit <- function(x, ...) {
  rawToBits(x)
}
parse_raw <- function(x, ...) {
  x
}
parse_reserved <- function(x, ...) {
  return()
}
parse_packet <- function(x, item, ...) {
  names <- names(item$specs)
  idx <- 0
  packet <- lapply(names, function(name) {
    sub_specs <- item$specs[[name]]
    sub_specs$name <- name
    sub_specs <- do.call(validate_spec, sub_specs)
    re <- parse_item(
      x[idx + seq_len(sub_specs$.bytes)],
      sub_specs
    )
    idx <<- idx + sub_specs$.bytes
    re$value
  })
  names(packet) <- names
  if(length(item$event)) {
    packet$event <- item$event
  }
  packet
}
parse_comment_packet <- function(x, item, ...) {
  names <- names(item$specs)
  idx <- 0
  packet <- lapply(names, function(name) {
    sub_specs <- item$specs[[name]]
    sub_specs$name <- name
    sub_specs <- do.call(validate_spec, sub_specs)
    re <- parse_item(
      x[idx + seq_len(sub_specs$.bytes)],
      sub_specs
    )
    idx <<- idx + sub_specs$.bytes
    re$value
  })
  if(is.character(packet[[length(packet)]]) && length(x) > length(idx)) {
    s <- parse_string(x[-seq_len(idx)])
    packet[[length(packet)]] <- paste0(packet[[length(packet)]], s)
  }
  names(packet) <- names
  packet$event <- item$event
  packet
}


parse_item <- function(slice_data, item) {
  # item <- section_specs[[ii]]
  # slice_idx <- section_slice_idx[ii, ]
  # slice_data <- section_data[seq(slice_idx[[1]], slice_idx[[2]])]
  parser <- get(sprintf("parse_%s", item$type), mode = "function",
                envir = asNamespace("raveio"), inherits = FALSE)
  if(!is.function(parser)) {
    stop("Cannot obtain parser function for type: ", item$type)
  }
  if(item$n > 1) {
    re <- matrix(slice_data, ncol = item$n, byrow = FALSE)
    re <- apply(re, 2, parser, item = item)
  } else {
    re <- parser(slice_data, item = item)
  }

  if(length(item$names)) {
    names(re) <- item$names
  }
  structure(
    list(
      name = item$name,
      raw = slice_data,
      value = re
    ),
    class = c(
      sprintf("nev-nsx-%s", item$name),
      "nev-nsx-entry"
    )
  )
}

parse__sequential <- function(conn, section_specs) {
  dictionary <- section_specs$dictionary
  keys <- names(dictionary)
  specs <- lapply(keys, function(name) {
    item <- dictionary[[name]]
    item$name <- name
    item <- do.call(validate_spec, item)

    data <- readBin(conn, what = "raw", n = item$size * item$n,
                    size = 1L, endian = "little")

    parse_item(slice_data = data, item = item)
  })
  names(specs) <- keys
  structure(
    specs,
    names = keys,
    class = c("nev-nsx-entry-list", "list")
  )
}
parse__with_string_key <- function(conn, section_specs, n_items,
                                   as_data_frame = TRUE) {
  key_rule <- do.call(validate_spec, section_specs$key_rule)
  dictionary <- section_specs$dictionary
  key_parser <- get(sprintf("parse_%s", key_rule$type), mode = "function")

  initial_read <- key_rule$start_byte + key_rule$.bytes

  re <- lapply(seq_len(n_items), function(ii) {

    data <- readBin(conn, what = "raw", n = initial_read,
                    size = 1L, endian = "little")
    key <- key_parser(data[key_rule$start_byte + seq_len(key_rule$.bytes)])
    item <- dictionary[[key]]
    if(is.null(item)) {
      stop("Cannot find specification for keyword: [", key, "]")
    }
    item$name <- key
    item <- do.call(validate_spec, item)

    p2_length <- item$.bytes - initial_read

    if(p2_length < 0) {
      stop("Wrong specification: data packet size is not enough to aquire packet key/ID. To obtain the key, it requires [", initial_read, "] bytes, but the packet size is: [", item$.bytes, "]")
    }
    if(length(p2_length)) {
      data_part2 <- readBin(conn, what = "raw", n = p2_length,
                            size = 1L, endian = "little")
      data <- c(data, data_part2)
    }

    parse_item(slice_data = data, item = item)
  })

  if(!as_data_frame) {
    return(re)
  }

  # make table for each header type
  names <- sapply(re, "[[", "name")
  tables <- lapply(split(re, names), function(li) {
    do.call("rbind", lapply(li, function(x) {
      as.data.frame(x$value, stringsAsFactors = FALSE)
    }))
  })
  names <- sapply(tables, function(x) {
    x[[key_rule$name]][[1]]
  })
  names(tables) <- names

  tables
}
parse__with_numeric_key <- function(conn, section_specs, data_packet_sizes) {
  re <- dipsaus::fastqueue2()
  if(!data_packet_sizes) {
    return(re)
  }
  key_rule <- do.call(validate_spec, section_specs$key_rule)
  key_parser <- get(sprintf("parse_%s", key_rule$type), mode = "function")
  dictionary <- section_specs$dictionary
  keys <- dipsaus::fastmap2(missing_default = NA)
  lapply(names(dictionary), function(key) {
    keys[dipsaus::parse_svec(key)] <- key
    return(key)
  })
  for(key in names(dictionary)) {
    item <- dictionary[[key]]
    item$name %?<-% key
    dictionary[[key]] <- do.call(validate_spec, item)
  }

  # read the read of data
  while(length({
    data <- readBin(conn, what = "raw", n = data_packet_sizes,
                    size = 1L, endian = "little")
  }) == data_packet_sizes) {

    key <- key_parser(data[key_rule$start_byte + seq_len(key_rule$.bytes)])
    dict_key <- keys[[key]]
    if(is.na(dict_key)) {
      stop(sprintf("Unknown [%s=%s]", key_rule$name, key))
    }
    item <- dictionary[[dict_key]]
    if(is.null(item)) {
      stop("Cannot find specification for keyword: [", key, "]")
    }

    packet <- parse_item(slice_data = data, item = item)
    re$add(packet)
  }
  class(re) <- c("nev-nsx-entry-list", "fastqueue2", "list")
  re
}

parse__nev <- function(nev_path, specification, nev_data = TRUE) {
  conn <- file(nev_path, "rb")
  on.exit({
    close(conn)
  })
  basic_header <- parse__sequential(conn, specification[[1]])

  n_ext_headers <- basic_header$number_of_extended_headers$value
  data_packet_sizes <- basic_header$bytes_in_data_packet$value

  ext_header <- parse__with_string_key(
    conn, specification[[2]], n_items = n_ext_headers)

  re <- dipsaus::fastmap2()
  re$basic_header <- basic_header
  re$extended_header <- ext_header

  # TODO: Postprocessing
  data_packets2 <- dipsaus::fastqueue2()

  if( nev_data ) {
    data_packets <- parse__with_numeric_key(
      conn, specification[[3]],
      data_packet_sizes = data_packet_sizes)

    # parse waveform
    waveform_flag <- rawToBits(basic_header$additional_flags$raw)
    waveform_dtype <- NA
    if(length(waveform_flag)) {
      waveform_flag <- as.integer(waveform_flag[[1]])
      if(waveform_flag == 1) {
        waveform_dtype <- "int16"
      }
    }
    electrode_ids <- ext_header$NEUEVWAV$electrode_id
    spike_widths <- ext_header$NEUEVWAV$spike_width
    bytes_per_waveforms <- ext_header$NEUEVWAV$bytes_per_waveform

    while(!is.null({packet <- data_packets$remove()})) {

      if(length(packet$value$waveform) && is.raw(packet$value$waveform)) {

        electrode_id <- packet$value$packet_id
        sel <- electrode_ids == electrode_id
        waveform <- packet$value$waveform
        if(any(sel)) {
          spike_width <- spike_widths[sel]
          bytes_per_waveform <- bytes_per_waveforms[sel]
          if(bytes_per_waveform == 0) {
            bytes_per_waveform <- 1
          }



          # translate waveform
          if(is.na(waveform_dtype)) {

            bytes <- 2^ceiling(log2(bytes_per_waveform))
            waveform <- matrix(waveform, nrow = bytes_per_waveform)
            waveform_dtype <- sprintf("int%s", bytes * 8)
            parser <- get(sprintf("parse_%s", waveform_dtype), envir = asNamespace("raveio"), mode = "function", inherits = FALSE)
            waveform <- apply(waveform, 2, function(w) {
              w <- c(w, rep(as.raw(0), bytes - bytes_per_waveform))
              parser(w)
            })
          } else {
            parser <- get(sprintf("parse_%s", waveform_dtype), envir = asNamespace("raveio"), mode = "function", inherits = FALSE)
            waveform <- parser(waveform)
          }

          packet$value$waveform <- waveform
        }

      }
      data_packets2$add(packet)

    }

    class(data_packets2) <- class(data_packets)
  }

  re$data_packets <- data_packets2
  re
}

parse__nsx <- function(nsx_path, specification, header_only = FALSE, verbose = TRUE,
                       filebase = tempfile(), force_update = FALSE) {
  re <- dipsaus::fastmap2()
  conn <- file(nsx_path, "rb")
  on.exit({ close(conn) })
  basic_header <- parse__sequential(conn, specification[[1]])
  re$basic_header <- basic_header

  ext_header <- parse__with_string_key(conn, specification[[2]], basic_header$channel_count$value)
  re$ext_header <- ext_header

  data_header <- parse__sequential(conn = conn, section_specs = specification[[3]])

  conn_data_offset <- basic_header$bytes_in_headers$value + length(data_header$data_header$raw)

  re$data_header <- data_header

  # create signature
  signature <- dipsaus::digest(list(
    basic_header = basic_header,
    ext_header = ext_header,
    data_header = data_header
  ))

  re$header_signature <- signature

  if(!header_only) {
    # Read the rest of data
    data_specs <- specification[[4]]$dictionary$data_points
    n_timepoints <- data_header$data_header$value$number_of_data_points
    n_channels <- basic_header$channel_count$value

    if(n_timepoints == 0) {
      stop("Cannot read BlackRock NSx file. Cannot obtain a positive number of time-points from the NSx headers")
    }


    arr <- tryCatch(
      expr = {
        if(force_update) {
          stop("Force updating NSx data file.")
        }
        filearray::filearray_checkload(
          filebase = filebase, mode = "readwrite",
          symlink_ok = FALSE, signature = signature,
          rave_data_type = "BlackRock NSx data array",
          units = "uV",
          ready = TRUE
        )
      },
      error = function(e) {
        if(file.exists(filebase)) {
          unlink(filebase, recursive = TRUE)
        }
        dir_create2(dirname(filebase))
        arr <- filearray::filearray_create(
          filebase = filebase,
          dimension = c(n_timepoints, n_channels),
          type = "float", partition_size = 1
        )
        arr$.mode <- "readwrite"
        arr$.header$signature <- signature
        arr$.header$rave_data_type <- "BlackRock NSx data array"
        arr$.header$units <- "uV"
        arr
      }
    )
    if(!isTRUE(arr$get_header(key = "ready", default = FALSE))) {

      parition_size <- ceiling(2^21 / n_channels)
      niters <- ceiling(n_timepoints / parition_size)
      data_specs$name <- "data_partition"

      # Calculate digital to analog transform
      min_digit <- ext_header$CC$min_digital_value[seq_len(n_channels)]
      min_analog <- ext_header$CC$min_analog_value[seq_len(n_channels)]
      ratio <- (
        ext_header$CC$max_analog_value - ext_header$CC$min_analog_value
      ) / (
        ext_header$CC$max_digital_value - ext_header$CC$min_digital_value
      )
      ratio <- ratio[seq_len(n_channels)]

      units <- sapply(ext_header$CC$units, function(unit) {
        switch (
          unit,
          "V" = { 1e6 },
          "mV" = { 1e3 },
          { 1 }
        )
      })[seq_len(n_channels)]
      min_analog <- min_analog * units
      ratio <- ratio * units

      parser <- get(sprintf("parse_%s", data_specs$type),
                    mode = "function", envir = asNamespace('raveio'),
                    inherits = FALSE)

      progress <- dipsaus::progress2("Loading NSx", max = niters,
                                     shiny_auto_close = TRUE, quiet = !verbose)


      pts_total <- n_timepoints * n_channels
      pts_read <- 0
      lapply(seq_len(niters), function(ii) {
        progress$inc(sprintf("Partition %d", ii))

        data_specs$n <- parition_size * n_channels

        if( data_specs$n > pts_total - pts_read ) {
          data_specs$n <- pts_total - pts_read
        }
        pts_read <<- pts_read + data_specs$n
        data_specs <- do.call(validate_spec, data_specs)

        data <- readBin(conn, what = "raw",
                        n = data_specs$.bytes,
                        size = 1L, endian = "little")

        data <- parser(data)
        ntp <- length(data) / n_channels

        if( round(ntp) != ntp ) {
          warning("Number of points is not integer. The data might be incomplete")
          ntp <- floor(ntp)
          if( ntp > 0 ) {
            data <- data[seq_len(n_channels * ntp)]
          }
        }

        if( ntp > 0 ) {
          dim(data) <- c(n_channels, ntp)
          data <- (data - min_digit) * ratio + min_analog

          arr[seq_len(ntp) + parition_size * (ii - 1), ] <- t(data)
        }

        return()
      })

      arr$set_header(key = "ready", value = TRUE)

    }

    re$data <- arr
  }


  re
}

blackrock_specification <- function(path) {
  header <- readBin(path, what = "raw", size = 1L, endian = "little", n = 10)
  file_type <- parse_string(header[seq_len(8)])
  file_version <- parse_uint8(header[c(9, 10)])

  file_type <- switch (
    file_type,
    NEURALEV = { "nev" },
    BREVENTS = { "nev" },
    BRSMPGRP = { "nsx" },
    NEURALCD = { "nsx" },
    NEURALSG = { "nsx" },
    {
      stop("`read_nsx_nev`: Unsupported files format [", file_type, "] in file: ", path)
    }
  )

  # get specification
  spec_file <- system.file("specifications", sprintf("blackrock-%s-%d.%d.yaml", file_type, file_version[[1]], file_version[[2]]), package = "raveio")

  if(!file.exists(spec_file)) {
    stop(sprintf(
      "`read_nsx_nev`: Unable to find file specification file of [.%s] with version [%d.%d]. The version might be too old (<= 2.1) or too new. Please contact RAVE develop team to add file specification.",
      file_type, file_version[[1]], file_version[[2]]))
  }
  spec <- yaml::read_yaml(spec_file)
  list(
    type = file_type,
    version = file_version,
    config = spec,
    file_size = file.size(path)
  )
}

blackrock_postprocess <- function(nsx, nev = NULL) {
  # reserved for future post-processing
  nsx
}

#' @title Read 'BlackRock' event and signal files
#' @description Current implementation supports minimum 2.3 file
#' specification version. Please contact the package maintainer to add
#' specification configurations if you want us to support older versions.
#' @param paths 'NSx' signal files, usually with file extensions such as
#' \code{'.ns1'}, \code{'.ns2'}, \code{'.ns3'}, \code{'.ns4'}, \code{'.ns5'}.
#' @param nev_path 'NEV' event files, with file extension \code{'.nev'}
#' @param header_only whether to load header information only and avoid
#' reading signal arrays
#' @param nev_data whether to load \code{'.nev'} comments and 'waveforms'
#' @param verbose whether to print out progress when loading signal array
#' @param ram whether to load signals into the memory rather than storing
#' with \code{\link[filearray]{filearray}}; default is false
#' @param force_update force updating the channel data even if the headers
#' haven't changed
#' @param temp_path temporary directory to store the channel data
#' @export
read_nsx_nev <- function(paths, nev_path = NULL,
                         header_only = FALSE, nev_data = TRUE,
                         verbose = TRUE, ram = FALSE, force_update = FALSE,
                         temp_path = file.path(tempdir(), "blackrock-temp")) {
  if(!all(file.exists(paths))) {
    stop("read_nsx_nev: at least one path cannot be found.")
  }

  fnames <- filenames(paths)

  if(length(nev_path)) {
    nev_info <- blackrock_specification(nev_path)
    if(!identical(nev_info$type, "nev")) {
      stop("`read_nsx_nev`: the given `nev_path` is not a valid neural-event file (.nev): ",
           nev_path)
    }
    nev <- parse__nev(nev_path, nev_info$config$specification, nev_data)
  } else {
    nev <- NULL
  }

  progress <- dipsaus::progress2(title = "Reading blackrock", max = length(paths), shiny_auto_close = TRUE, quiet = header_only || !verbose || !length(paths))

  nsx <- structure(
    lapply(paths, function(path) {
      info <- blackrock_specification(path)

      if(!identical(info$type, "nsx")) {
        stop("read_nsx_nev: path is not a valid nsx file (.ns1, .ns2, ..., .ns5): ", path)
      }

      progress$inc(sprintf("Parsing %s", filenames(path)))

      filebase <- file.path(temp_path, paste0(filenames(path), ".filearray"))

      re <- parse__nsx(path, info$config$specification,
                       header_only = header_only,
                       verbose = verbose, filebase = filebase,
                       force_update = force_update)
      # Post processing
      re <- blackrock_postprocess(nsx = re, nev = nev)

      if(ram && !header_only) {
        re$data <- re$data[drop = FALSE]
      }
      re
    }),
    names = fnames
  )


  list(
    nev = nev,
    nsx = nsx
  )
}
