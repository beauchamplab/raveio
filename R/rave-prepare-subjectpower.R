#' @rdname rave-prepare
#' @export
prepare_subject_power <- function(subject, electrodes, reference_name, epoch_name, time_windows, signal_type = c("LFP"), env = parent.frame(), verbose = TRUE, ...) {
  call <- match.call()
  call[[1]] <- as.call(list(quote(`::`), quote(raveio), quote(prepare_subject_with_epoch)))

  if(length(signal_type) > 1) {
    stop("`prepare_subject_power`: you can only load one signal type each time")
  }

  re <- eval(call, envir = env)

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
      ref$load_data(type = "power")
    }, callback = function(ref){
      sprintf("Loading Reference (power) | %s", ref$number)
    })
  } else {
    refs <- lapply_async(ref_instances, function(ref){
      ref$load_data(type = "power")
    }, callback = NULL)
  }


  # load actual power, reference on the fly
  if(verbose) {
    power_list <- lapply_async(re$electrode_instances, function(el){
      el$load_data(type = "power")
    }, callback = function(el){
      sprintf("Loading Electrode (power) | %s", el$number)
    })
  } else {
    power_list <- lapply_async(re$electrode_instances, function(el){
      el$load_data(type = "power")
    }, callback = NULL)
  }


  # re$power_list <- power_list
  power_dimnames <- dimnames(power_list[[1]])
  power_dimnames$Electrode <- re$electrode_list
  re$time_points <- power_dimnames$Time
  # re$power_dimnames <- power_dimnames
  power_dim <- vapply(power_dimnames, length, 0L)
  # re$power_dim <- power_dim

  digest_key <- list(
    subject_id = re$subject$subject_id,
    epoch_table = re$epoch$table[c("Block", "Time", "Trial")],
    reference_table = re$reference_table,
    time_points = power_dimnames$Time,
    electrode_list = re$electrode_list,
    frequency_table = frequency_table,
    electrode_signal_types = re$electrode_signal_types
  )
  digest_string <- dipsaus::digest(digest_key)

  re$signature <- structure(digest_string, contents = names(digest_key))

  re$power <- dipsaus::fastmap2()
  re$power$dimnames <- power_dimnames
  re$power$dim <- power_dim
  re$power$data_list <- power_list
  re$power$signature <- dipsaus::digest(c(digest_string, re$electrode_list))

  class(re) <- c(
    sprintf("rave_prepare_power-%s", signal_type),
    "rave_prepare_power", class(re)
  )
  re

}

# re <- prepare_subject_power('demo/DemoSubject')
# re$electrode_instances$e_14$load_data('power')
