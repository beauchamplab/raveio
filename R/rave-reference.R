#' @title Generate common average reference signal for 'RAVE' subjects
#' @description To properly run this function, please install \code{ravetools}
#' package.
#' @param subject subject ID or \code{\link{RAVESubject}} instance
#' @param electrodes electrodes to calculate the common average; these
#' electrodes must run through 'Wavelet' first
#' @return A reference instance returned by \code{\link{new_reference}} with
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

  subject <- as_rave_subject(subject, strict = FALSE)

  electrodes <- dipsaus::parse_svec(electrodes)

  # check if any electrodes fall out of declaration
  missing_e <- electrodes[!electrodes %in% subject$electrodes]
  if(length(missing_e)) {
    stop("Electrodes ", dipsaus::deparse_svec(missing_e), " is missing or not defined. Please remove it from the reference")
  }
  if(!length(electrodes)) {
    stop("The reference does not contain any electrode channels")
  }

  if(!all(subject$preprocess_settings$has_wavelet[subject$electrodes %in% electrodes])) {
    stop("Wavelet has not been applied to one or more electrodes. Please run the 'Wavelet' module first.")
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

  ref_signals <- dipsaus::lapply_async2(blocks, function(block){

    ref_signal <- 0
    subject_inst <- as_rave_subject(subject_id, strict = FALSE)

    for(e in electrodes) {
      inst <- LFP_electrode$new(subject = subject_inst, number = e)
      s <- load_h5(inst$voltage_file, sprintf("raw/voltage/%s", block))
      ref_signal <- ref_signal + (s[] / nchans)
    }

    sarray_path <- file.path(subject_inst$reference_path,
                             sprintf("ref_%s", electrode_text),
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
      compress_rate <- srate / wavelet_params$downsample_to
      s <- ref_signal
      if(wavelet_params$pre_downsample > 1) {
        s <- ravetools::decimate(s, wavelet_params$pre_downsample, ftype = "fir")
        compress_rate <- compress_rate / wavelet_params$pre_downsample
      }
      wavelet <- ravetools::morlet_wavelet(
        data = s,
        freqs = wavelet_params$frequencies,
        srate = srate,
        wave_num = wavelet_params$cycle,
        precision = wavelet_params$precision
      )

      ind <- floor(seq(1, length(s), by = compress_rate))

      if(wavelet_params$precision == 'double') {
        coef <- wavelet$real[ind, ] + 1i * wavelet$imag[ind, ]
        wavelet$real$.mode <- "readwrite"
        wavelet$real$delete(force = TRUE)
        wavelet$imag$.mode <- "readwrite"
        wavelet$imag$delete(force = TRUE)
      } else {
        coef <- wavelet[ind, ]
        wavelet$.mode <- "readwrite"
        wavelet$delete(force = TRUE)
      }

      # write coef to reference path
      warray_path <- file.path(subject_inst$reference_path,
                               sprintf("ref_%s", electrode_text),
                               block, 'wavelet')
      if(dir.exists(warray_path)) {
        unlink(warray_path, recursive = TRUE)
      }
      dir_create2(dirname(warray_path))
      warray <- filearray::filearray_create(filebase = warray_path, dimension = dim(coef), type = "complex")
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

  }, plan = FALSE, callback = function(block){
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

  # Important: always clear subject's cached data
  clear_cached_files(
    subject_code = subject$subject_code,
    quiet = TRUE
  )

  new_reference(subject = subject, number = electrode_text)
}
