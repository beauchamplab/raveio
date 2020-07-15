# readin edf file format

check_edf <- function(){
  if(!dipsaus::package_installed('edfReader')){
    stop("Package ", sQuote('edfReader'), ' is not installed. ',
         'Please run \n  install.packages("edfReader")')
  }
}

# path <- '~/Downloads/eeg_recording/ma0844az_1-1+.edf'

#' @export
read_edf_header <- function(path){
  check_edf()

  header <- edfReader::readEdfHeader(path)

  # annotation
  is_annot <- header$sHeaders$isAnnotation

  # sample rate
  srates <- header$sHeaders$sRate

  # is continuous
  is_cont <- header$isContinuous

  list(
    header = header,
    is_annot = is_annot,
    srates = srates,
    is_cont = is_cont
  )

}

#' @export
read_edf_signal <- function(path, electrodes = NULL){
  check_edf()
  header <- edfReader::readEdfHeader(path)

  signals <-
    edfReader::readEdfSignals(header,
                              signals = 'All',
                              simplify = FALSE,
                              fragments = FALSE)

  electrode_names <- names(signals)

  signal_number <- sapply(electrode_names, function(nm){
    signals[[nm]]$RSignalNumber
  }, simplify = TRUE, USE.NAMES = TRUE)

  elec_names <- sapply(electrodes, function(e){
    sel <- signal_number == e
    if(!any(sel)) { return() }
    electrode_names[sel][[1]]
  }, simplify = FALSE, USE.NAMES = TRUE)

  return(list(
    header = header,
    signals = signals,
    signal_names = electrode_names,
    signal_number = signal_number,
    elec_names = elec_names
  ))

}

