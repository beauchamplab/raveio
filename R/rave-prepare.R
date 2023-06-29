#' @title Prepare 'RAVE' single-subject data
#' @param subject character of project and subject, such as \code{"demo/YAB"},
#' or \code{\link{RAVESubject}} instance
#' @param electrodes integer vector of electrodes, or a character that can be
#' parsed by \code{\link[dipsaus]{parse_svec}}
#' @param epoch_name epoch name to be loaded, or a
#' \code{\link{RAVEEpoch}} instance
#' @param reference_name reference name to be loaded
#' @param time_windows a list of time windows that are relative to epoch onset
#' time; need to pass the validation \code{\link{validate_time_window}}
#' @param signal_type electrode signal type (length of one) to be considered;
#' default is 'LFP'. This option rarely needs to change unless you really want
#' to check the power data from other types. For other signal types, check
#' \code{\link{SIGNAL_TYPES}}
#' @param blocks one or more session blocks to load
#' @param time_frequency whether to load time-frequency data when preparing
#' block data
#' @param env environment to evaluate
#' @param repository_id used internally
#' @param verbose whether to show progress
#' @param quiet whether to quietly load the data
#' @param ... ignored
#' @returns A \code{\link[dipsaus]{fastmap2}} (basically a list) of objects.
#' Depending on the functions called, the following items may exist in the list:
#' \describe{
#' \item{\code{subject}}{A \code{\link{RAVESubject}} instance}
#' \item{\code{epoch_name}}{Same as input \code{epoch_name}}
#' \item{\code{epoch}}{A \code{\link{RAVEEpoch}} instance}
#' \item{\code{reference_name}}{Same as input \code{reference_name}}
#' \item{\code{reference_table}}{A data frame of reference}
#' \item{\code{electrode_table}}{A data frame of electrode information}
#' \item{\code{frequency}}{A vector of frequencies}
#' \item{\code{time_points}}{A vector of time-points}
#' \item{\code{power_list}}{A list of power data of the electrodes}
#' \item{\code{power_dimnames}}{A list of trial indices, frequencies, time
#' points, and electrodes that are loaded}
#' }
#' @name rave-prepare
NULL

#
# prepare_power <- function(subject, electrodes,
#                           epoch_name, reference_name,
#                           time_windows, signal_types = c("LFP")) {
#   unsupported_signal_types <- signal_types[!signal_types %in% SIGNAL_TYPES]
#   if(length(unsupported_signal_types)){
#     stop("Unsupported signal type(s): ", paste(unsupported_signal_types, collapse = ", "))
#   }
#
#   re <- dipsaus::fastmap2()
#
#   # Subject instance
#   subject <- as_rave_subject(subject, strict = TRUE)
#   re$subject <- subject
#
#   if(missing(time_windows)){
#     time_windows <- subject$get_default("time_windows", default_if_missing = list(c(0, 2)))
#     message("No time_windows specified, using default: ", deparse(time_windows))
#   }
#
#   if(!is.list(time_windows)){
#     time_windows <- unlist(time_windows)
#     if(length(time_windows) %% 2 != 0){
#       stop("`time_windows` must be a list of time intervals (length 2)")
#     }
#     time_windows <- matrix(time_windows, nrow = 2, byrow = FALSE)
#     time_windows <- as.list(as.data.frame(time_windows))
#     time_windows <- unname(time_windows)
#   }
#   lapply(time_windows, function(x){
#     if(length(x) != 2){
#       stop("`time_windows` must be a list of time intervals (length 2)")
#     }
#     if(!is.numeric(x)){
#       stop("`time_windows` must be a list of 'numerical' time intervals")
#     }
#     if(anyNA(x)){
#       stop("`time_windows` cannot contain NAs")
#     }
#     if(x[[1]] > x[[2]]){
#       stop("`time_windows` time intervals must be in ascending order")
#     }
#   })
#
#   # Epoch
#   if(missing(epoch_name)){
#     if(!length(subject$epoch_names)){
#       stop("No epoch file found in this subject. Please check meta folder.")
#     }
#     epoch_name <- subject$get_default('epoch_name', default_if_missing = subject$epoch_names[[1]])
#     if(!epoch_name %in% subject$epoch_names){
#       epoch_name <- subject$epoch_names[[1]]
#     }
#     message("No epoch_name specified, using epoch `", epoch_name, "`.")
#     epoch <- subject$get_epoch(epoch_name,
#                                trial_starts = min(unlist(time_windows)))
#   } else {
#     if(inherits(epoch_name, "RAVEEpoch")){
#       epoch <- epoch_name
#       epoch_name <- epoch$name
#     } else {
#       epoch <- subject$get_epoch(epoch_name,
#                                  trial_starts = min(unlist(time_windows)))
#     }
#   }
#   re$epoch_name <- epoch_name
#   re$epoch <- epoch
#
#   if(missing(reference_name)){
#     if(!length(subject$reference_names)){
#       stop("No reference file found in this subject. Please check meta folder.")
#     }
#     reference_name <- subject$get_default('reference_name', default_if_missing = subject$reference_names[[1]])
#     if(!reference_name %in% subject$reference_names){
#       reference_name <- subject$reference_names[[1]]
#     }
#     message("No reference_name specified, using reference `", reference_name, "`.")
#   }
#   reference_table <-
#     subject$get_reference(reference_name = reference_name)
#   re$reference_name <- reference_name
#   re$reference_table <- reference_table
#
#   valid_electrodes <- subject$valid_electrodes(reference_name)
#   if(missing(electrodes)){
#     electrodes <- dipsaus::parse_svec(subject$get_default(
#       "electrodes", default_if_missing = valid_electrodes))
#     message("No electrodes specified, loading default electrodes: ", dipsaus::deparse_svec(electrodes))
#   } else {
#     electrodes <- dipsaus::parse_svec(electrodes)
#   }
#   electrodes <- sort(electrodes, decreasing = FALSE)
#   electrode_types <- subject$electrode_types
#   available_electrodes <- subject$electrodes[electrode_types %in% signal_types & subject$has_wavelet]
#   electrodes <- electrodes[electrodes %in% available_electrodes]
#
#   if(!length(electrodes)){
#     stop("No electrode is available to load. Please check if you have run wavelet, or adjust the `signal_types`.")
#   }
#
#   electrode_table <- subject$get_electrode_table(
#     electrodes = electrodes,
#     reference_name = reference_name)
#
#   re$electrode_table <- electrode_table
#
#   frequency_table <- subject$get_frequency(simplify = FALSE)
#   re$frequency <- frequency_table$Frequency
#
#   loading <- subset(electrode_table, subset = electrode_table$isLoaded)
#   electrode_list <- unique(loading$Electrode)
#   re$electrode_list <- electrode_list
#
#   electrode_signal_types <- subject$electrode_types[subject$electrodes %in% electrode_list]
#   re$electrode_signal_types <- electrode_signal_types
#
#   ref_table <- reference_table[reference_table$Electrode %in% electrode_list, ]
#   references_list <- unique(ref_table$Reference)
#   re$references_list <- references_list
#
#   # load references
#   dipsaus::lapply_async2(references_list, function(ref_name){
#     ns <- asNamespace("raveio")
#     ref <- ns$LFP_electrode$new(
#       subject = subject,
#       ref_name, is_reference = TRUE)
#     ref$set_epoch(epoch)
#     ref$trial_intervals <- time_windows
#     reference_data <- ref$load_data(type = "power")
#     NULL
#   }, callback = function(ref_name){
#     sprintf("Loading Reference | %s", ref_name)
#   }, plan = FALSE)
#
#   # load actual power, reference on the fly
#   power_list <- dipsaus::lapply_async2(electrode_list, function(e){
#     ns <- asNamespace('raveio')
#     ref_name <- reference_table$Reference[reference_table$Electrode == e]
#     el <- ns$LFP_electrode$new(subject = subject, e, is_reference = FALSE)
#     ref <- ns$LFP_electrode$new(subject = subject, ref_name, is_reference = TRUE)
#     el$set_reference(ref)
#     el$set_epoch(epoch)
#     el$trial_intervals <- time_windows
#     el$load_data(type = "power")
#   }, callback = function(e){
#     sprintf("Loading Electrode | %s", e)
#   }, plan = FALSE)
#   re$power_list <- power_list
#
#   # create electrode instances
#   electrode_instances <- lapply(electrode_list, function(e){
#     ref_name <- reference_table$Reference[reference_table$Electrode == e]
#     el <- LFP_electrode$new(subject = subject, e, is_reference = FALSE)
#     ref <- LFP_electrode$new(subject = subject, ref_name, is_reference = TRUE)
#     el
#   })
#   names(electrode_instances) <- electrode_list
#
#   re$electrode_instances <- electrode_instances
#   power_dimnames <- dimnames(power_list[[1]])
#   power_dimnames$Electrode <- electrode_list
#   re$power_dimnames <- power_dimnames
#   re$power_dim <- sapply(power_dimnames, length)
#   re$time_points <- power_dimnames$Time
#
#   # generate a list that should be used to calculate digest
#   digest_key <- list(
#     subject_id = subject$subject_id,
#     epoch_table = epoch$table[c("Block", "Time", "Trial")],
#     reference_table = reference_table,
#     time_points = power_dimnames$Time,
#     electrodes = electrode_list,
#     frequency_table = frequency_table,
#     electrode_signal_types = electrode_signal_types
#   )
#   digest_string <- dipsaus::digest(digest_key)
#   # cache_root <- file.path( cache_root(), "_binded_arrays_", digest_string, "power")
#   # dir_create2(cache_root)
#
#   re$power <- dipsaus::fastmap2()
#   for(signal_type in unique(electrode_signal_types)){
#     # find electrodes
#     # fbase <- file.path(cache_root, signal_type)
#     sel <- electrode_signal_types == signal_type
#     elecs <- electrode_list[sel]
#     tmp_list <- power_list[sel]
#     arr_dnames <- power_dimnames
#     arr_dnames$Electrode <- elecs
#
#     re$power[[signal_type]] <- list(
#       electrodes = elecs,
#       data_list = tmp_list,
#       signature = dipsaus::digest(c(digest_string, elecs))
#     )
#
#     # if(length(tmp_list)){
#     #
#     #   arr <- tryCatch({
#     #     filearray::filearray_checkload(
#     #       fbase, mode = "readonly", symlink_ok = TRUE,
#     #       rave_signature = digest_string,
#     #       signal_type = signal_type,
#     #       rave_data_type = "power"
#     #     )
#     #   }, error = function(e){
#     #     if(file.exists(fbase)){
#     #       unlink(fbase, recursive = TRUE, force = TRUE)
#     #     }
#     #     symlink <- getOption("filearray.symlink_enabled", symlink_enabled())
#     #     arr <- filearray::filearray_bind(
#     #         .list = tmp_list,
#     #         filebase = fbase,
#     #         symlink = symlink,
#     #         overwrite = TRUE,
#     #         cache_ok = TRUE
#     #       )
#     #     arr$.mode <- "readwrite"
#     #     arr$.header$rave_signature <- digest_string
#     #     arr$.header$signal_type <- signal_type
#     #     arr$.header$rave_data_type <- "power"
#     #     dimnames(arr) <- arr_dnames
#     #     arr$.mode <- "readonly"
#     #     arr
#     #   })
#     #
#     #   re$power[[signal_type]] <- arr
#     # }
#
#   }
#
#   class(re) <- c("rave_prepare_power", "rave_repository", "fastmap2", "list")
#   re
# }

