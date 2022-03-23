# readin edf file format

check_edf <- function(){
  if(!dipsaus::package_installed('edfReader')){
    stop("Package ", sQuote('edfReader'), ' is not installed. ',
         'Please run \n  install.packages("edfReader")')
  }
}

# path <- '~/Downloads/eeg_recording/ma0844az_1-1+.edf'

#' Read 'EDF(+)' or 'BDF(+)' file headers
#' @description Wrapper of \code{\link[edfReader]{readEdfHeader}}, but added
#' some information
#' @param path file path, passed to \code{readEdfHeader}
#' @seealso \code{\link[edfReader]{readEdfHeader}}
#' @details The added names are: \code{isAnnot2}, \code{sampleRate2}, and
#' \code{unit2}. To avoid conflict with other names, there is a "2" appended
#' to each names. \code{isAnnot2} indicates whether each channel is annotation
#' channel or recorded signals. \code{sampleRate2} is a vector of sample rates
#' for each channels. \code{unit2} is physical unit of recorded signals.
#' For 'iEEG' data, this is electric potential unit, and choices are \code{'V'}
#' for volt, \code{'mV'} for millivolt, and \code{'uV'} for micro-volt.
#' For more details, see \url{https://www.edfplus.info/specs/edftexts.html}
#' @return A list is header information of an 'EDF/BDF' file.
#'
#' @export
read_edf_header <- function(path){
  check_edf()

  header <- edfReader::readEdfHeader(path)

  header$isAnnot2 <- header$sHeaders$isAnnotation
  header$sampleRate2 <- header$sHeaders$sRate
  header$unit2 <- header$sHeaders$physicalDim

  header
}


#' Read 'EDF(+)' or 'BDF(+)' file signals
#' @param path file path, passed to \code{readEdfHeader}
#' @param signal_numbers channel/electrode numbers
#' @param convert_volt convert voltage (electric potential) to a new unit,
#' \code{NA} means no conversion, other choices are \code{'V'}, \code{'mV'}, and
#' \code{'uV'}.
#' @return A list containing header information, signal lists, and
#' channel/electrode names. If \code{signal_numbers} is specified,
#' the corresponding names should appear as \code{selected_signal_names}.
#' \code{get_signal()} can get physical signals after unit conversion.
#' @export
read_edf_signal <- function(path, signal_numbers = NULL,
                            convert_volt = c('NA', 'V', 'mV', 'uV')){
  header <- read_edf_header(path)
  signals <-
    edfReader::readEdfSignals(header,
                              signals = 'All',
                              simplify = FALSE,
                              fragments = FALSE, physical = TRUE)

  signal_names <- names(signals)

  signal_order <- sapply(signal_names, function(nm){
    signals[[nm]]$RSignalNumber
  }, simplify = TRUE, USE.NAMES = TRUE)

  elec_names <- sapply(signal_numbers, function(e){
    sel <- signal_order == e
    if(!any(sel)) { return(NA) }
    signal_names[sel][[1]]
  }, simplify = TRUE, USE.NAMES = TRUE)

  if(any(isTRUE(is.na(convert_volt)))){
    convert_volt <- NA
  } else {
    convert_volt <- match.arg(convert_volt)
    if(convert_volt == 'NA'){
      convert_volt <- NA
    }
  }


  if(!is.na(convert_volt)){
    cv <- c(1, 1e-3, 1e-6)[c('V', 'mV', 'uV') == convert_volt]
  } else {
    cv <- NA
  }


  get_signal <- function(number){
    sel <- signal_order == number
    if(!any(sel)){ stop('Cannot find channel/electrode number') }
    nm <- signal_names[sel][[1]]
    unit <- header$unit2[sel][[1]]
    s <- signals[[nm]]$signal
    if(length(cv) && !is.na(cv)){
      cv2 <- c(1, 1e-3, 1e-6)[c('V', 'mV', 'uV') == unit]
      if(length(cv2) == 1){
        s <- (cv2 / cv) * s
        unit <- convert_volt
      }
    }
    list(
      signal = s,
      unit = unit
    )
  }

  selected_signal_details <- lapply(signal_numbers, get_signal)

  return(list(
    header = header,
    signals = signals,
    all_signal_names = signal_names,
    all_signal_numbers = signal_order,
    selected_signal_names = elec_names,
    selected_signal_numbers = signal_numbers,
    selected_signal_details = selected_signal_details,
    get_signal = get_signal
  ))

}




read_edf_signal2 <- function(path, signal_numbers,
                            convert_volt = c('NA', 'V', 'mV', 'uV')){
  header <- read_edf_header(path)
  if(missing(signal_numbers)) {
    signal_numbers <- seq_len(header$nSignals)
  }
  if(length(signal_numbers)) {
    signal_numbers <- signal_numbers[!is.na(signal_numbers)]
  }
  signals <-
    edfReader::readEdfSignals(header,
                              signals = signal_numbers,
                              simplify = FALSE,
                              fragments = FALSE, physical = TRUE)

  signal_names <- names(signals)

  signal_order <- sapply(signal_names, function(nm){
    signals[[nm]]$signalNumber
  }, simplify = TRUE, USE.NAMES = TRUE)

  elec_names <- sapply(signal_numbers, function(e){
    sel <- signal_order == e
    if(!any(sel)) { return(NA) }
    signal_names[sel][[1]]
  }, simplify = TRUE, USE.NAMES = TRUE)

  if(any(isTRUE(is.na(convert_volt)))){
    convert_volt <- NA
  } else {
    convert_volt <- match.arg(convert_volt)
    if(convert_volt == 'NA'){
      convert_volt <- NA
    }
  }


  if(!is.na(convert_volt)){
    cv <- c(1, 1e-3, 1e-6)[c('V', 'mV', 'uV') == convert_volt]
  } else {
    cv <- NA
  }


  get_signal <- function(number){
    sel <- signal_order == number
    if(!any(sel)){ stop('Cannot find channel/electrode number') }
    nm <- signal_names[sel][[1]]
    unit <- header$unit2[sel][[1]]
    s <- signals[[nm]]$signal
    if(length(cv) && !is.na(cv)){
      cv2 <- c(1, 1e-3, 1e-6)[c('V', 'mV', 'uV') == unit]
      if(length(cv2) == 1){
        s <- (cv2 / cv) * s
        unit <- convert_volt
      }
    }
    list(
      signal = s,
      unit = unit
    )
  }

  selected_signal_details <- lapply(signal_numbers, get_signal)

  return(list(
    header = header,
    signals = signals,
    all_signal_names = signal_names,
    all_signal_numbers = signal_order,
    selected_signal_names = elec_names,
    selected_signal_numbers = signal_numbers,
    selected_signal_details = selected_signal_details,
    get_signal = get_signal
  ))

}

