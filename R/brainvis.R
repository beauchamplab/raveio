# BrainVision formats

read_vmrk <- function(file) {
  # http://pressrelease.brainproducts.com/markers/

  items <- readLines(file)

  # obtain markers
  pattern <- stringr::regex("^Mk([0-9]*)?=(.*)$", dotall = TRUE)
  sel <- stringr::str_detect(items, pattern)

  comments <- items[!sel]
  items <- items[sel]

  if(!length(items)){
    return()
  }

  # parse header comments, get marker infos
  tmp <- paste(comments, collapse = '')
  marker_info <- stringr::str_extract(tmp, 'Mk<([^=]+)>=(<[^<>]+>[,; ]+){0,}')
  marker_info <- stringr::str_split(marker_info, '([<>,;]+)|(^Mk)')[[1]]
  marker_info <- stringr::str_trim(marker_info)
  marker_info <- marker_info[!marker_info %in% c('', '=')]

  markers <- stringr::str_match(items, pattern = pattern)
  markers <- cbind( markers[,2],
                    stringr::str_split(markers[,3], ',', simplify = TRUE)
  )

  # don't know the last two columns, remove them
  markers <- markers[, seq_len(6)]
  colnames(markers) <- c('MarkerNumber', 'Type', 'Description', 'StartPosition', 'Size', 'Channel')

  markers <- as.data.frame(markers)
  markers$MarkerNumber <- as.integer(markers$MarkerNumber)
  markers$StartPosition <- as.integer(markers$StartPosition)
  markers$Size <- as.integer(markers$Size)
  markers$Channel <- as.integer(markers$Channel)

  list(
    comments = comments,
    header = marker_info,
    content = as.data.frame(markers)
  )

}

read_ini <- function (path, encoding = getOption("encoding")) {
  regexp_section <- "^\\s*\\[\\s*(.+?)\\s*]"
  regexp_keyval <- "^\\s*[^=]+=.+"
  regexp_comment <- "^\\s*[;#]"
  # trim <- function(x) gsub("^\\s*(.*?)\\s*$", "\\1", x)
  re <- list()
  con <- file(path, open = "r", encoding = encoding)
  on.exit(close(con))
  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)

    # EOF
    if (!length(line)) { break }

    # Comment line
    if (grepl(regexp_comment, line)) { next }

    # Section line
    if (grepl(regexp_section, line)) {
      matches <- regexec(regexp_section, line)
      current_section <- regmatches(line, matches)[[1]][2]
    }

    if (grepl(regexp_keyval, line)) {
      s <- strsplit(line, "=")[[1]]
      key <- trimws(s[[1]], which = "both")
      value <- trimws(paste0(s[-1], collapse = "="), which = "both")
      re[[current_section]] <- c(re[[current_section]], structure(list(value), names = key))
    }
  }
  re
}

#' Load from 'BrainVision' file
#' @description Read in \code{'eeg'} or \code{'ieeg'} data from 'BrainVision'
#' files with \code{.eeg} or \code{.dat} extensions.
#' @param file path to \code{'vhdr'} header file
#' @param header header object returned by \code{read_eeg_header}
#' @param path optional, path to data file if original data file is missing or
#' renamed; must be absolute path.
#'
#' @return \code{read_eeg_header} returns a list containing information below:
#' \item{raw}{raw header contents}
#' \item{common}{a list of descriptors of header}
#' \item{channels}{table of channels, including number,
#' reference, resolution and unit}
#' \item{sample_rate}{sampling frequency}
#' \item{root_path}{directory to where the data is stored}
#' \item{channel_counts}{total channel counts}
#' \item{markers}{\code{NULL} if marker file is missing, or list of marker
#' description and table containing 6 columns.}
#' \code{read_eeg_data} returns header, signal data and data description:
#' \item{data}{a matrix of signal values. Each row is a channel and each
#' column is a time point.}
#'
#' @details A 'BrainVision' dataset is usually stored separately in header
#' file (\code{.vhdr}), marker file (\code{.vmrk}, optional) and
#' data file (\code{.eeg} or \code{.dat}). These files must store under a
#' same folder to be read into R.
#'
#' Header data contains channel information. Data "channel" contains
#' channel name, reference, resolution and physical unit. "resolution"
#' times digital data values is the physical value of the recorded data.
#' \code{read_eeg_data} makes this conversion internally .
#' "unit" is the physical unit of recordings. By default \code{'uV'} means
#' micro-volts.
#'
#' Marker file that ends with \code{.vmrk} is optional. If the file is
#' indicated by header file and exists, then a marker table will be included
#' when reading headers. A marker table contains six columns: marker number,
#' type, description, start position (in data point), size (duration in
#' data points), and target channel (0 means applied for all channels).
#'
#' Signal file name is usually contained within header file. Therefore it is
#' desired that the signal file name never changed once created. However,
#' in some cases when the signal files are renamed and cannot be indexed
#' by header files, please specify \code{path} to force load signals from
#' a different file.
#'
#' @examples
#'
#' header_file <- 'sub-01_ses-01_task-visual_run-01_ieeg.vhdr'
#'
#' if( file.exists(header_file) ){
#'   # load a subject header
#'   header <- read_eeg_header(header_file)
#'
#'   # load entire signal
#'   data <- read_eeg_data(header)
#'
#'   data$description
#' }
#'
#' @name read-brainvision-eeg
NULL


#' @rdname read-brainvision-eeg
#' @export
read_eeg_header <- function(file) {
  file <- normalizePath(file)
  # vhdr <- ini::read.ini(file)
  vhdr <- read_ini(file)

  # https://www.brainproducts.com/files/public/products/more/BrainVisionCoreDataFormat_1-0.pdf

  # Get channel information
  channel_names <- names(vhdr[["Channel Infos"]])
  channel_info <- stringr::str_split(vhdr[["Channel Infos"]], ',', simplify = TRUE)
  if(ncol(channel_info) == 3){
    colnames(channel_info) <- c('number', 'reference', 'resolution')
    channel_info <- as.data.frame(channel_info, row.names = channel_names)
    channel_info$unit <- 'uV'
  } else {
    colnames(channel_info) <- c('number', 'reference', 'resolution', 'unit')
    channel_info <- as.data.frame(channel_info, row.names = channel_names)
  }

  # adjust storage mode?
  channel_info$resolution <- as.numeric(channel_info$resolution)
  sel <- channel_info$reference == ''
  channel_info$reference[sel] <- NA


  common <- vhdr[["Common Infos"]]

  # read markers file
  # Name of optional marker file. If it exists the marker file
  # contains a list of markers assigned to the EEG data.
  # For the format of the marker file refer to the section
  # “Marker file” below. The placeholder $b can be used in
  # the file name (see example for keyword DataFile).
  # -- It is assumed that the marker file is in the same folder as the header file.
  root <- dirname(file)
  vmrk_file <- common$MarkerFile
  markers <- NULL
  if(length(vmrk_file) == 1 && is.character(vmrk_file)){
    vmrk_file <- file.path(root, vmrk_file)
    if(isTRUE(file.exists(vmrk_file))){
      # read markers file
      markers <- read_vmrk(vmrk_file)
    }
  }

  # TODO: Currently I ignore coordinates because
  # rave assume you do your own localization, maybe someday I'll support it,
  # not today. in case some other information is needed, save raw header
  dipsaus::list_to_fastmap2(list(
    raw = vhdr,
    common = common,
    channels = channel_info,
    markers = markers,
    root_path = root,
    channel_counts = as.integer(common$NumberOfChannels),
    sample_rate = as.numeric(common$SamplingInterval)
  ))
}

#' @rdname read-brainvision-eeg
#' @export
read_eeg_data <- function(header, path = NULL) {

  if(is.null(path)){
    path <- header$root_path
  }
  if(!file.exists(path)){
    stop('Cannot find .eeg/.dat path')
  }
  if(file.exists(path) && !dir.exists(path)){
    # path is a file, we assume the directory stores data
    fname <- stringr::str_extract(path, '[^/\\\\]+$')
    path <- dirname(path)
  } else {
    fname <- header$common$DataFile
  }

  if(fname != header$common$DataFile){
    if(file.exists(file.path(path, header$common$DataFile))){
      warning('File name ', sQuote(fname), ' does not match with data file stored in header. Original file ',
              sQuote(header$common$DataFile), ' exists, read the original file instead.')
      fname <- header$common$DataFile
    }else {
      warning('File name ', sQuote(fname), ' does not match with data file stored in header. Original file ',
              sQuote(header$common$DataFile), ' is missing. Read ', sQuote(fname), ' instead.')
    }

  }

  file <- file.path(path, fname)

  nchannels <- nrow(header$channels)

  binary <- stringr::str_to_upper(header$common$DataFormat) == 'BINARY'

  orientation <- stringr::str_to_upper(header$common$DataOrientation)

  # In case of MULTIPLEXED all channel data are written sequentially in one
  # line for one sampling point in time. The next line contains the data
  # for the next sampling point in time
  multiplexed <- stringr::str_detect(orientation, 'MULTI')

  if(binary){
    # get type
    # BinaryFormat Encoding of EEG data. Possible values:
    #   IEEE_FLOAT_32: IEEE floating-point format, single precision, 4 bytes per value
    #   INT_16: 16-bit signed integer (length = 2 bytes)
    binary_format <- stringr::str_to_upper(header$raw$`Binary Infos`$BinaryFormat)
    # two cases: float_32 or int_16. Might be other methods? not sure
    binary_format <- stringr::str_split(binary_format, '_')[[1]]
    binary_format <- binary_format[length(binary_format) - c(0,1)]
    len <- as.integer(binary_format[[1]]) / 8
    binary_format <- binary_format[[2]]
    if(binary_format == 'FLOAT'){
      type <- 'double'
    } else if (binary_format == 'INT'){
      type <- 'integer'
    } else {
      stop('Found binary format ', sQuote(binary_format), ' in header file. Not supported yet. ',
           'Only IEEE_FLOAT_32 (float) and INT_16 (int) are supported.')
    }

    s <- readBin(file, what = type, size = len, n = file.info(file)$size/len)

    # if multiplexed, by row
    s <- matrix(s, nrow = nchannels, byrow = !multiplexed)
  } else {
    # format is not binary - ascii or text case
    text_info <- header$raw[["ASCII Infos"]]
    SkipLines <- as.integer(text_info$SkipLines)
    SkipColumns <- as.integer(text_info$SkipColumns)
    DecimalSymbol <- text_info$DecimalSymbol
    s <- data.table::fread(file = file, skip = SkipLines, dec = DecimalSymbol,
                           sep = " ", header = FALSE)
    if(length(SkipColumns) > 0 && any(SkipColumns > 0)){
      s <- s[, -SkipColumns, with = FALSE]
    }
    if(multiplexed){
      # need transpose to channel x timepoints
      s <- data.table::transpose(s)
    }
    s <- as.matrix(s)
  }

  # Adjustment
  # Individual properties for the channel are specified
  # separated by commas:
  #   <channel name>,[<reference channel name>],[<resolution in "unit">],[<unit>]
  # Example 1
  # Ch1=Fp1,,0.1,uV
  # The first channel has the channel name "Fp1". The
  # common reference channel is used as the reference
  # channel because no entry has been made. The
  # resolution in "unit" is 0.1 (the resolution is the value by
  #                              which the value of the data point has to be multiplied to
  #                              convert it to the channel unit). The unit is uV.
  # BrainVision Analyzer 2 interprets empty units as uV.

  resolution <- as.numeric(header$channels$resolution)

  if(length(resolution) != nrow(s)){
    # not likely, but in case this happends, stop loudly, don't give
    # wrong results
    stop('Number of channels does not match with data read from the file.')
  }

  if(!all(resolution == 1)){
    s <- s * resolution
  }

  dipsaus::list_to_fastmap2(list(
    header = header,
    data = s,
    description = 'channel by time-points'
  ))

}


