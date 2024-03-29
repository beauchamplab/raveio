#' @title Generate common average reference signal for 'RAVE' subjects
#' @description To properly run this function, please install \code{ravetools}
#' package.
#' @param subject subject ID or \code{\link{RAVESubject}} instance
#' @param electrodes electrodes to calculate the common average; these
#' electrodes must run through 'Wavelet' first
#' @returns A reference instance returned by \code{\link{new_reference}} with
#' signal type determined automatically.
#' @details
#' The goal of generating common average signals is to capture the common
#' movement from all the channels and remove them out from electrode
#' signals.
#'
#' The common average signals will be stored at subject reference
#' directories. Two exact same copies will be stored: one in 'HDF5'
#' format such that the data can be read universally by other programming
#' languages; one in \code{\link[filearray]{filearray}} format that can be
#' read in R with super fast speed.
#' @export
generate_reference <- function(subject, electrodes) {

  # DIPSAUS DEBUG START
  # subject <- "YAEL/PAV020"
  # electrodes <- c(1,2,3,4)

  subject <- restore_subject_instance(subject, strict = FALSE)

  electrodes <- dipsaus::parse_svec(electrodes)

  # check if any electrodes fall out of declaration
  missing_e <- electrodes[!electrodes %in% subject$electrodes]
  if(length(missing_e)) {
    stop("Electrodes ", dipsaus::deparse_svec(missing_e), " is missing or not defined. Please remove it from the reference")
  }
  if(!length(electrodes)) {
    stop("The reference does not contain any electrode channels")
  }

  has_wavelet <- TRUE
  if(!all(subject$preprocess_settings$has_wavelet[subject$electrodes %in% electrodes])) {
    has_wavelet <- FALSE
    # stop("Wavelet has not been applied to one or more electrodes. Please run the 'Wavelet' module first.")
  }

  # generate reference from channels
  blocks <- subject$blocks

  # ncores <- raveio_getopt("max_worker")
  # ncols <- ceiling(length(electrodes) / ncores)
  # scheduler_mat <- matrix(c(electrodes, rep(NA, ncores * ncols - length(electrodes))),
  #                         nrow = ncores)

  nchans <- length(electrodes)
  subject_id <- subject$subject_id

  wavelet_params <- subject$preprocess_settings$wavelet_params
  srate <- subject$raw_sample_rates[subject$electrodes %in% electrodes][[1]]

  electrode_text <- dipsaus::deparse_svec(electrodes)

  ref_cache_path <- file.path(subject$reference_path,
                              sprintf("ref_%s", electrode_text))

  ref_signals <- lapply_async(blocks, function(block){

    ref_signal <- 0
    subject_inst <- restore_subject_instance(subject_id, strict = FALSE)

    for(e in electrodes) {
      inst <- LFP_electrode$new(subject = subject_inst, number = e, quiet = TRUE)
      if( has_wavelet ) {
        s <- load_h5(inst$voltage_file, sprintf("raw/voltage/%s", block))
      } else {
        s <- load_h5(inst$preprocess_file, sprintf("notch/%s", block))
      }
      ref_signal <- ref_signal + (s[] / nchans)
    }

    sarray_path <- file.path(ref_cache_path,
                             block, 'voltage')
    if(dir.exists(sarray_path)) {
      unlink(sarray_path, recursive = TRUE)
    }

    dir_create2(dirname(sarray_path))
    sarray <- filearray::filearray_create(filebase = sarray_path, dimension = c(length(ref_signal), 1L), type = "double")
    sarray[] <- ref_signal
    sarray$set_header("staged", TRUE)
    sarray$.mode <- "readonly"

    # perform wavelet
    if(is.list(wavelet_params)) {

      # wavelet
      coef <- 0
      for(e in electrodes) {
        inst <- LFP_electrode$new(subject = subject_inst, number = e)
        power <- load_h5(inst$power_file, sprintf("raw/power/%s", block), ram = TRUE)
        phase <- load_h5(inst$phase_file, sprintf("raw/phase/%s", block), ram = TRUE)
        coef <- coef + sqrt(power) * exp(1i * phase)
      }
      coef <- coef / nchans
      if(!is.matrix(coef)) {
        dim(coef) <- c(length(coef), 1L)
      } else {
        coef <- t(coef)
      }

      warray_path <- file.path(subject_inst$reference_path,
                               sprintf("ref_%s", electrode_text),
                               block, 'wavelet')
      if(dir.exists(warray_path)) {
        unlink(warray_path, recursive = TRUE)
      }
      dir_create2(dirname(warray_path))
      warray <- filearray::filearray_create(filebase = warray_path,
                                            dimension = dim(coef),
                                            type = "complex")
      # write coef to reference path
      warray[] <- coef
      rm(coef)
      warray$set_header("staged", TRUE)
      warray$.mode <- "readonly"
    } else {
      warray <- NULL
    }

    list(
      voltage = sarray,
      wavelet = warray
    )

  }, callback = function(block){
    sprintf("Collecting data|Block %s", block)
  })
  names(ref_signals) <- blocks

  target_file <- file.path(subject$reference_path, sprintf("ref_%s.h5", electrode_text))
  if(file.exists(target_file)) {
    unlink(target_file)
  }

  if(is.list(wavelet_params)) {
    chunk <- c(length(wavelet_params$frequencies), 1, 2L)
    chunk[[2]] <- 2^ceiling(log2(1024 / prod(chunk)))
  } else {
    chunk <- NULL
  }

  conf_data <- list(
    subject_id = subject$subject_id,
    reference_created_from = electrodes,
    has_wavelet = has_wavelet,
    blocks = blocks
  )

  for(block in blocks){

    item <- ref_signals[[block]]

    # voltage
    save_h5(as.vector(item$voltage[]),
                    file = target_file,
                    name = sprintf("/voltage/%s", block),
                    chunk = 1024, level = 7, replace = TRUE)

    # wavelet
    if(!is.null(item$wavelet)) {
      coef <- t(item$wavelet[drop=FALSE])
      coef_data <- c(Mod(coef), Arg(coef))
      dim(coef_data) <- c(dim(coef), 2L)

      save_h5(coef_data, file = target_file,
                      name = sprintf("/wavelet/coef/%s", block),
                      chunk = chunk, level = 7, replace = TRUE)

      rm(coef_data)
      rm(coef)
    }
  }

  ref_param_path <- file.path(ref_cache_path, "config.yaml")
  save_yaml(conf_data, ref_param_path)

  # Important: always clear subject's cached data
  clear_cached_files(
    subject_code = subject$subject_code,
    quiet = TRUE
  )

  new_reference(subject = subject, number = electrode_text)
}
