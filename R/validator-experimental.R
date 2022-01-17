#' @export
prepare_power <- function(subject, electrodes,
                          epoch_name, reference_name,
                          time_windows = c(-1,2)) {

  if(!is.list(time_windows)){
    time_windows <- unlist(time_windows)
    if(length(time_windows) %% 2 != 0){
      stop("`time_windows` must be a list of time intervals (length 2)")
    }
    time_windows <- matrix(time_windows, nrow = 2, byrow = FALSE)
    time_windows <- as.list(as.data.frame(time_windows))
    time_windows <- unname(time_windows)
  }
  lapply(time_windows, function(x){
    if(length(x) != 2){
      stop("`time_windows` must be a list of time intervals (length 2)")
    }
    if(!is.numeric(x)){
      stop("`time_windows` must be a list of 'numerical' time intervals")
    }
    if(anyNA(x)){
      stop("`time_windows` cannot contain NAs")
    }
    if(x[[1]] > x[[2]]){
      stop("`time_windows` time intervals must be in ascending order")
    }
  })

  re <- dipsaus::fastmap2()

  # Subject instance
  subject <- as_rave_subject(subject, strict = TRUE)
  re$subject <- subject

  # Epoch
  if(missing(epoch_name)){
    if(!length(subject$epoch_names)){
      stop("No epoch file found in this subject. Please check meta folder.")
    }
    epoch_name <- subject$epoch_names[[1]]
    message("No epoch_name specified, using epoch `", epoch_name, "`.")
  }
  epoch <- subject$get_epoch(epoch_name = epoch_name,
                             trial_starts = min(unlist(time_windows)))
  re$epoch_name <- epoch_name
  re$epoch <- epoch

  if(missing(reference_name)){
    if(!length(subject$reference_names)){
      stop("No reference file found in this subject. Please check meta folder.")
    }
    reference_name <- subject$reference_names[[1]]
    message("No reference_name specified, using reference `", reference_name, "`.")
  }
  reference_table <-
    subject$get_reference(reference_name = reference_name)
  re$reference_name <- reference_name
  re$reference_table <- reference_table

  if(missing(electrodes)){
    electrodes <- subject$electrodes
    message("No electrodes specified, loading all electrodes.")
  }
  electrode_table <- subject$get_electrode_table(
    electrodes = electrodes,
    reference_name = reference_name)
  re$electrode_table <- electrode_table

  frequency_table <- subject$get_frequency(simplify = FALSE)
  re$frequency_table <- frequency_table

  loading <- subset(electrode_table, subset = electrode_table$isLoaded)
  electrode_list <- unique(loading$Electrode)
  re$electrode_list <- electrode_list

  ref_table <- subset(reference_table, Electrode %in%
                        electrode_list)
  references_list <- unique(ref_table$Reference)
  re$references_list <- references_list

  # load references
  ncores <- raveio_getopt("max_worker", default = 1L)
  dipsaus::lapply_callr(references_list, function(ref_name){
    ns <- asNamespace("raveio")
    ref <- ns$LFP_electrode$new(
      subject = subject,
      ref_name, is_reference = TRUE)
    ref$set_epoch(epoch)
    ref$trial_intervals <- time_windows
    reference_data <- ref$load_data(type = "power")
    NULL
  }, .callback = function(ref_name){
    sprintf("Loading Reference | %s", ref_name)
  }, .globals = list(
    subject = subject,
    epoch = epoch,
    time_windows = time_windows
  ), .ncores = ncores, .packages = "raveio")

  # load actual power, reference on the fly
  power_list <- dipsaus::lapply_callr(electrode_list, function(e){
    ns <- asNamespace('raveio')
    ref_name <- reference_table$Reference[reference_table$Electrode == e]
    el <- ns$LFP_electrode$new(subject = subject, e, is_reference = FALSE)
    ref <- ns$LFP_electrode$new(subject = subject, ref_name, is_reference = TRUE)
    el$set_reference(ref)
    el$set_epoch(epoch)
    el$trial_intervals <- time_windows
    el$load_data(type = "power")
  }, .globals = list(
    reference_table = reference_table,
    subject = subject,
    epoch = epoch,
    time_windows = time_windows
  ), .callback = function(e){
    sprintf("Loading Electrode | %s", e)
  }, .ncores = ncores, .packages = "raveio")
  re$power_list <- power_list

  power_dimnames <- dimnames(power_list[[1]])
  power_dimnames$Electrode <- electrode_list
  re$power_dimnames <- power_dimnames

  re
}
