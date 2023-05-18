
#' @rdname rave-prepare
#' @export
prepare_subject_wavelet <- function(subject, electrodes, reference_name, epoch_name, time_windows, signal_type = c("LFP"), env = parent.frame(), verbose = TRUE, ...) {
  call <- match.call()
  call[[1]] <- as.call(list(quote(`::`), quote(raveio), quote(prepare_subject_with_epoch)))

  if(length(signal_type) > 1) {
    stop("`prepare_subject_wavelet`: you can only load one signal type each time")
  }

  re <- eval(call, envir = env)

  # DIPSAUS DEBUG START
  # subject = "demo/YAB"
  # re = prepare_subject_with_epoch(subject)
  # signal_type <- "LFP"
  # verbose <- TRUE
  re$signal_type <- signal_type


  frequency_table <- re$subject$get_frequency(simplify = FALSE)
  frequency <- frequency_table$Frequency
  re$frequency <- frequency

  match_signal_types <- re$electrode_signal_types %in% signal_type
  re$electrode_list <- re$electrode_list[match_signal_types]
  re$electrode_instances <- re$electrode_instances[match_signal_types]
  re$electrode_signal_types <- re$electrode_signal_types[match_signal_types]

  electrode_signal_types <- re$electrode_signal_types

  # load references first
  ref_mat <- unique(sprintf("%s_%s", re$reference_table[re$reference_table$Electrode %in% re$electrode_list, "Reference"], electrode_signal_types))

  ref_instances <- dipsaus::drop_nulls(re$reference_instances[ref_mat])
  if(verbose) {
    refs <- lapply_async(ref_instances, function(ref){
      ref$load_data(type = "wavelet-coefficient")
    }, callback = function(ref){
      sprintf("Loading Reference (wavelet-coefficient) | %s", ref$number)
    })
  } else {
    refs <- lapply_async(ref_instances, function(ref){
      ref$load_data(type = "wavelet-coefficient")
    }, callback = NULL)
  }


  # load actual wavelet-coefficient, reference on the fly
  if(verbose) {
    wavelet_list <- lapply_async(re$electrode_instances, function(el){
      el$load_data(type = "wavelet-coefficient")
    }, callback = function(el){
      sprintf("Loading Electrode (wavelet-coefficient) | %s", el$number)
    })
  } else {
    wavelet_list <- lapply_async(re$electrode_instances, function(el){
      el$load_data(type = "wavelet-coefficient")
    }, callback = NULL)
  }


  # re$wavelet_list <- wavelet_list
  wavelet_dimnames <- dimnames(wavelet_list[[1]])
  wavelet_dimnames$Electrode <- re$electrode_list
  re$time_points <- wavelet_dimnames$Time
  # re$wavelet_dimnames <- wavelet_dimnames
  wavelet_dim <- vapply(wavelet_dimnames, length, 0L)
  # re$wavelet_dim <- wavelet_dim

  digest_key <- list(
    subject_id = re$subject$subject_id,
    epoch_table = re$epoch$table[c("Block", "Time", "Trial")],
    reference_table = re$reference_table,
    time_points = wavelet_dimnames$Time,
    electrode_list = re$electrode_list,
    frequency_table = frequency_table,
    electrode_signal_types = re$electrode_signal_types,
    signal_data_type = "wavelet-coefficient"
  )
  digest_string <- dipsaus::digest(digest_key)

  re$signature <- structure(digest_string, contents = names(digest_key))

  re$wavelet <- dipsaus::fastmap2()
  re$wavelet$dimnames <- wavelet_dimnames
  re$wavelet$dim <- wavelet_dim
  re$wavelet$data_list <- wavelet_list
  re$wavelet$signature <- dipsaus::digest(c(digest_string, re$electrode_list))

  class(re) <- c(
    sprintf("rave_prepare_wavelet-%s", signal_type),
    "rave_prepare_wavelet", class(re)
  )
  re

}

# re <- prepare_subject_phase('demo/YAB')
# re$electrode_instances$e_14$load_data('power')
