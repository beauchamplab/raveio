#' @rdname rave-prepare
#' @export
prepare_subject_bare0 <- function(subject, electrodes, reference_name, ..., quiet = TRUE, repository_id = NULL) {
  re <- dipsaus::fastmap2()
  subject <- as_rave_subject(subject)

  # ----- project -----
  re$project <- subject$project

  # ----- subject -----
  re$subject <- subject

  if(missing(electrodes)){
    electrodes <- subject$get_default(
      "electrodes", default_if_missing = subject$electrodes)
    message("No electrodes specified, loading all electrodes: ", dipsaus::deparse_svec(electrodes))
  }
  if(length(electrodes) == 1 && is.character(electrodes)) {
    electrodes <- sort(dipsaus::parse_svec(electrodes))
  }

  if(missing(reference_name) || !length(reference_name) || !all(reference_name %in% subject$reference_names)){
    if(!length(subject$reference_names)){
      warning("No reference file found in this subject. Please check meta folder! Preparing table with no reference.")
      safe_write_csv(
        data.frame(
          Electrode = subject$electrodes,
          Group = "default",
          Reference = "noref",
          Type = "No Reference"
        ), file = file.path(subject$meta_path, "reference_noref.csv"),
        row.names = FALSE
      )
      reference_name <- "noref"
    } else {
      reference_name <- subject$get_default('reference_name', default_if_missing = subject$reference_names[[1]])
      if(!reference_name %in% subject$reference_names){
        reference_name <- subject$reference_names[[1]]
      }
      if(reference_name != "noref") {
        message("No reference_name specified, using reference `", reference_name, "`.")
      }
    }
  } else {
    reference_name <- reference_name[reference_name %in% subject$reference_names]
    reference_name <- reference_name[[1]]
  }
  reference_table <- subject$get_reference(reference_name)

  if("Reference" %in% names(reference_table)){
    old_electrodes <- electrodes
    electrodes <- as.integer(reference_table$Electrode[reference_table$Reference != ''])
    electrodes <- old_electrodes[old_electrodes %in% electrodes]
    if(!setequal(electrodes, old_electrodes)){
      old_electrodes <- dipsaus::deparse_svec(old_electrodes[!old_electrodes %in% electrodes])
      message("The following electrodes are removed because they are either missing or marked as `excluded`: ", old_electrodes)
    }
  }

  # ----- reference_name -----
  re$reference_name <- reference_name

  # ----- reference_table -----
  re$reference_table <- reference_table

  # ----- references_list -----
  ref_table <- reference_table[reference_table$Electrode %in% electrodes, ]
  references_list <- unique(ref_table$Reference)
  re$references_list <- references_list

  # ----- electrode_list -----
  electrode_list <- electrodes
  re$electrode_list <- electrode_list

  # ----- electrode_table -----
  electrode_table <- subject$get_electrode_table()
  re$electrode_table <- electrode_table

  # ----- electrode_signal_types -----
  sel <- subject$electrodes %in% electrodes
  electrode_signal_types <- subject$electrode_types[sel]
  re$electrode_signal_types <- electrode_signal_types


  # ----- reference_instances -----
  # load reference electrodes
  ref_mat <- unique(cbind(
    ref_table$Reference,
    electrode_signal_types
  ))
  reference_instances <- structure(
    lapply(seq_len(nrow(ref_mat)), function(ii){
      y <- ref_mat[ii, ]
      new_reference(subject = subject, number = y[[1]], signal_type = y[[2]], quiet = quiet)
    }),
    names = sprintf("%s_%s", ref_mat[, 1], ref_mat[, 2])
  )
  re$reference_instances <- dipsaus::drop_nulls(reference_instances)

  # ----- electrode_instances -----
  electrode_instances <- structure(lapply(seq_along(electrode_list), function(ii){
    e <- electrode_list[[ii]]
    signal_type <- electrode_signal_types[[ii]]
    ref_name <- reference_table$Reference[reference_table$Electrode == e][[1]]
    ref_name <- sprintf("%s_%s", ref_name, signal_type)
    ref <- reference_instances[[ref_name]]

    el <- new_electrode(subject = subject, number = e, signal_type = signal_type, quiet = quiet)
    el$set_reference(ref)
    el
  }), names = sprintf("e_%d", electrode_list))
  re$electrode_instances <- electrode_instances

  digest_key <- list(
    subject_id = re$subject$subject_id,
    reference_table = re$reference_table,
    electrodes = re$electrode_list,
    electrode_signal_types = re$electrode_signal_types
  )
  digest_string <- dipsaus::digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  class(re) <- c("rave_prepare_subject_bare0", "rave_repository", "fastmap2", "list")
  re
}


#' @rdname rave-prepare
#' @export
prepare_subject_bare <- function(subject, electrodes, reference_name, ...,
                                 repository_id = NULL) {

  # electrode_list, reference_name, reference_table, electrode_table, subject, references_list, electrode_signal_types, electrode_instances
  re <- dipsaus::fastmap2()
  subject <- as_rave_subject(subject)

  # ----- project -----
  re$project <- subject$project

  # ----- subject -----
  re$subject <- subject

  if(missing(electrodes)){
    electrodes <- subject$get_default(
      "electrodes", default_if_missing = subject$electrodes)
    message("No electrodes specified, loading all electrodes: ", dipsaus::deparse_svec(electrodes))
  }
  if(length(electrodes) == 1 && is.character(electrodes)) {
    electrodes <- sort(dipsaus::parse_svec(electrodes))
  }


  if(missing(reference_name) || !length(reference_name) || !all(reference_name %in% subject$reference_names)){
    if(!length(subject$reference_names)){
      warning("IMPORTANT: No reference file found in this subject. Please check meta folder! Preparing fake reference table with no reference.")
      safe_write_csv(
        data.frame(
          Electrode = subject$electrodes,
          Group = "default",
          Reference = "noref",
          Type = "No Reference"
        ), file = file.path(subject$meta_path, "reference_noref.csv"),
        row.names = FALSE
      )
      reference_name <- "noref"
    } else {
      reference_name <- subject$get_default('reference_name', default_if_missing = subject$reference_names[[1]])
      if(!reference_name %in% subject$reference_names){
        reference_name <- subject$reference_names[[1]]
      }
      message("No reference_name specified, using reference `", reference_name, "`.")
    }
  } else {
    reference_name <- reference_name[reference_name %in% subject$reference_names]
    reference_name <- reference_name[[1]]
  }
  reference_table <- subject$get_reference(reference_name)

  if("Reference" %in% names(reference_table)){
    old_electrodes <- electrodes
    electrodes <- as.integer(reference_table$Electrode[reference_table$Reference != ''])
    electrodes <- old_electrodes[old_electrodes %in% electrodes]
    if(!setequal(electrodes, old_electrodes)){
      old_electrodes <- dipsaus::deparse_svec(old_electrodes[!old_electrodes %in% electrodes])
      message("The following electrodes are removed because they are either missing or marked as `excluded`: ", old_electrodes)
    }
  }

  # ----- reference_name -----
  re$reference_name <- reference_name

  # ----- reference_table -----
  re$reference_table <- reference_table

  # ----- references_list -----
  ref_table <- reference_table[reference_table$Electrode %in% electrodes, ]
  references_list <- unique(ref_table$Reference)
  re$references_list <- references_list

  # ----- electrode_list -----
  electrode_list <- electrodes
  re$electrode_list <- electrode_list

  # ----- electrode_table -----
  electrode_table <- subject$get_electrode_table(
    electrodes = electrodes,
    reference_name = reference_name,
    subset = FALSE,
    simplify = FALSE)
  re$electrode_table <- electrode_table

  # ----- electrode_signal_types -----
  sel <- subject$electrodes %in% electrodes
  electrode_signal_types <- subject$electrode_types[sel]
  re$electrode_signal_types <- electrode_signal_types


  # ----- reference_instances -----
  # load reference electrodes
  ref_mat <- unique(cbind(
    ref_table$Reference,
    electrode_signal_types
  ))
  reference_instances <- structure(
    lapply(seq_len(nrow(ref_mat)), function(ii){
      y <- ref_mat[ii, ]
      new_reference(subject = subject, number = y[[1]], signal_type = y[[2]])
    }),
    names = sprintf("%s_%s", ref_mat[, 1], ref_mat[, 2])
  )
  re$reference_instances <- dipsaus::drop_nulls(reference_instances)

  # ----- electrode_instances -----
  electrode_instances <- structure(lapply(seq_along(electrode_list), function(ii){
    e <- electrode_list[[ii]]
    signal_type <- electrode_signal_types[[ii]]
    ref_name <- reference_table$Reference[reference_table$Electrode == e][[1]]
    ref_name <- sprintf("%s_%s", ref_name, signal_type)
    ref <- reference_instances[[ref_name]]

    el <- new_electrode(subject = subject, number = e, signal_type = signal_type)
    el$set_reference(ref)
    el
  }), names = sprintf("e_%d", electrode_list))
  re$electrode_instances <- electrode_instances

  digest_key <- list(
    subject_id = re$subject$subject_id,
    reference_table = re$reference_table,
    electrodes = re$electrode_list,
    electrode_signal_types = re$electrode_signal_types
  )
  digest_string <- dipsaus::digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  class(re) <- c("rave_prepare_subject", "rave_repository", "fastmap2", "list")
  re

}

#' @rdname rave-prepare
#' @export
prepare_subject_with_epoch <- function(subject, electrodes, reference_name, epoch_name, time_windows, env = parent.frame(), ...){

  call <- as.list(match.call())
  call[["env"]] <- NULL
  call[["time_windows"]] <- NULL
  call[["epoch_name"]] <- NULL
  call[[1]] <- as.call(list(quote(`::`), quote(raveio), quote(prepare_subject_bare)))
  call <- as.call(call)
  re <- eval(call, envir = env)

  if(missing(time_windows)){
    time_windows <- re$subject$get_default("time_windows", default_if_missing = list(c(0, 2)))
    message("No time_windows specified, using default: ", deparse(time_windows))
    missing_time_windows <- TRUE
  }
  time_windows <- validate_time_window(time_windows)
  re$time_windows <- time_windows

  if(missing(epoch_name)){
    if(!length(re$subject$epoch_names)){
      stop("No epoch file found in this subject. Please check meta folder.")
    }
    epoch_name <- re$subject$get_default('epoch_name', default_if_missing = re$subject$epoch_names[[1]])
    if(!epoch_name %in% re$subject$epoch_names){
      epoch_name <- re$subject$epoch_names[[1]]
    }
    message("No epoch_name specified, using epoch `", epoch_name, "`.")
    epoch <- re$subject$get_epoch(
      epoch_name = epoch_name,
      trial_starts = min(unlist(time_windows)),
      as_table = FALSE
    )
  } else {
    if(inherits(epoch_name, "RAVEEpoch")){
      epoch <- epoch_name
      epoch_name <- epoch$name
    } else {
      epoch <- re$subject$get_epoch(
        epoch_name = epoch_name,
        trial_starts = min(unlist(time_windows)),
        as_table = FALSE
      )
    }
  }
  re$epoch_name <- epoch_name
  re$epoch <- epoch

  # set epoch and time_windows
  lapply(re$reference_instances, function(e){
    e$set_epoch(epoch)
    e$trial_intervals <- time_windows
    NULL
  })
  lapply(re$electrode_instances, function(e){
    e$set_epoch(epoch)
    e$trial_intervals <- time_windows
    NULL
  })



  digest_key <- list(
    subject_id = re$subject$subject_id,
    epoch_table = re$epoch$table,
    reference_table = re$reference_table,
    electrode_list = re$electrode_list,
    electrode_signal_types = re$electrode_signal_types,
    time_windows = re$time_windows
  )
  digest_string <- dipsaus::digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))

  class(re) <- c(
    "rave_prepare_with_epoch",
    "rave_prepare_subject", "rave_repository",
    "fastmap2", "list"
  )
  re
}

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
    refs <- dipsaus::lapply_async2(ref_instances, function(ref){
      ref$load_data(type = "power")
    }, callback = function(ref){
      sprintf("Loading Electrode | %s", ref$number)
    }, plan = FALSE)
  } else {
    refs <- dipsaus::lapply_async2(ref_instances, function(ref){
      ref$load_data(type = "power")
    }, callback = NULL, plan = FALSE)
  }


  # load actual power, reference on the fly
  if(verbose) {
    power_list <- dipsaus::lapply_async2(re$electrode_instances, function(el){
      el$load_data(type = "power")
    }, callback = function(el){
      sprintf("Loading Electrode | %s", el$number)
    }, plan = FALSE)
  } else {
    power_list <- dipsaus::lapply_async2(re$electrode_instances, function(el){
      el$load_data(type = "power")
    }, callback = NULL, plan = FALSE)
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
