#' @rdname rave-prepare
#' @export
prepare_subject_raw_voltage_with_epoch <- function(subject, electrodes, epoch_name, time_windows, ..., quiet = TRUE, repository_id = NULL) {
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

  # ----- epoch -----
  if(missing(time_windows)){
    time_windows <- subject$get_default("time_windows", default_if_missing = list(c(0, 2)))
    message("No time_windows specified, using default: ", deparse(time_windows))
    missing_time_windows <- TRUE
  }
  time_windows <- validate_time_window(time_windows)
  re$time_windows <- time_windows

  if(missing(epoch_name)){
    epoch_names <- subject$epoch_names
    if(!length(epoch_names)){
      stop("No epoch file found in this subject. Please check meta folder.")
    }
    epoch_name <- subject$get_default('epoch_name') %OF% epoch_names
    message("No epoch_name specified, using epoch `", epoch_name, "`.")
    epoch <- subject$get_epoch(
      epoch_name = epoch_name,
      trial_starts = min(unlist(time_windows)),
      as_table = FALSE
    )
  } else {
    if(inherits(epoch_name, "RAVEEpoch")){
      epoch <- epoch_name
      epoch_name <- epoch$name
    } else {
      epoch <- subject$get_epoch(
        epoch_name = epoch_name,
        trial_starts = min(unlist(time_windows)),
        as_table = FALSE
      )
    }
  }
  re$epoch <- epoch
  re$epoch_name <- epoch_name
  epoch_table <- epoch$table
  epoch_table <- epoch_table[order(epoch_table$Trial), ]
  re$epoch_table <- epoch_table

  # ----- electrode_list -----
  re$electrode_list <- electrodes

  # ----- electrode_table -----
  electrode_table <- subject$get_electrode_table()
  re$electrode_table <- electrode_table

  # ----- electrode_signal_types -----
  sel <- subject$electrodes %in% electrodes
  electrode_signal_types <- subject$electrode_types[sel]
  re$electrode_signal_types <- electrode_signal_types

  # ----- sample_rate -----
  sample_rate <- unique(subject$raw_sample_rates[sel])
  if(length(sample_rate) > 1) {
    stop(sprintf("Found more than different sample rates from the requested electrode channels [%s]. Please choose electrodes with the same sample rate.", paste(sprintf("%.0fHz", sample_rate), collapse = ", ")))
  }
  re$sample_rate <- sample_rate

  # ----- electrode_instances -----
  electrode_instances <- structure(lapply(seq_along(electrodes), function(ii){
    e <- electrodes[[ii]]
    signal_type <- electrode_signal_types[[ii]]

    el <- new_electrode(subject = subject, number = e, signal_type = signal_type, quiet = quiet)
    el$set_epoch(epoch)
    el$trial_intervals <- time_windows
    el
  }), names = sprintf("e_%d", electrodes))
  re$electrode_instances <- electrode_instances

  # ----- load_data -----
  data_list <- lapply_async(
    re$electrode_instances,
    function(inst) {
      inst$load_data(type = "raw-voltage")
    },
    callback = function(inst) {
      sprintf("Loading raw-voltage|Electrode %s", inst$number)
    }
  )
  names(data_list) <- names(electrode_instances)
  dim <- dim(data_list[[1]])
  dim[[3]] <- length(data_list)

  dimnames <- dimnames(data_list[[1]])
  dimnames[[3]] <- electrodes

  digest_key <- list(
    subject_id = subject$subject_id,
    epoch_table = epoch_table,
    electrodes = electrodes,
    rave_data_type = "raw-voltage",
    electrode_signal_types = electrode_signal_types,
    sample_rate = sample_rate,
    time_windows = time_windows
  )
  digest_string <- dipsaus::digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  re$raw_voltage <- dipsaus::list_to_fastmap2(list(
    dim = dim,
    data_list = data_list,
    dimnames = dimnames,
    signature = re$signature
  ))

  class(re) <- c("rave_prepare_subject_raw_voltage_with_epoch", "rave_repository", "fastmap2", "list")
  re
}



#' @rdname rave-prepare
#' @export
prepare_subject_voltage_with_epoch <- function(subject, electrodes, epoch_name, time_windows, reference_name, ..., quiet = TRUE, repository_id = NULL) {

  # ----- DIPSAUS: DEBUG START--------
  # devtools::load_all()
  # subject <- "devel/PAV007"
  # electrodes <- c(14,15)
  # epoch_name <- "stimulation"
  # time_windows <- c(-1,2)
  # quiet = TRUE
  # repository_id <- NULL
  # re <- dipsaus::fastmap2()
  # subject <- as_rave_subject(subject)
  # re$project <- subject$project
  # re$subject <- subject

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

  # ----- reference -----
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
  re$reference_table <- reference_table
  re$reference_name <- reference_name

  # ----- epoch -----
  if(missing(time_windows)){
    time_windows <- subject$get_default("time_windows", default_if_missing = list(c(0, 2)))
    message("No time_windows specified, using default: ", deparse(time_windows))
    missing_time_windows <- TRUE
  }
  time_windows <- validate_time_window(time_windows)
  re$time_windows <- time_windows

  if(missing(epoch_name)){
    epoch_names <- subject$epoch_names
    if(!length(epoch_names)){
      stop("No epoch file found in this subject. Please check meta folder.")
    }
    epoch_name <- subject$get_default('epoch_name') %OF% epoch_names
    message("No epoch_name specified, using epoch `", epoch_name, "`.")
    epoch <- subject$get_epoch(
      epoch_name = epoch_name,
      trial_starts = min(unlist(time_windows)),
      as_table = FALSE
    )
  } else {
    if(inherits(epoch_name, "RAVEEpoch")){
      epoch <- epoch_name
      epoch_name <- epoch$name
    } else {
      epoch <- subject$get_epoch(
        epoch_name = epoch_name,
        trial_starts = min(unlist(time_windows)),
        as_table = FALSE
      )
    }
  }
  re$epoch <- epoch
  re$epoch_name <- epoch_name
  epoch_table <- epoch$table
  epoch_table <- epoch_table[order(epoch_table$Trial), ]
  re$epoch_table <- epoch_table

  # ----- electrode_list -----
  re$electrode_list <- electrodes

  # ----- electrode_table -----
  electrode_table <- subject$get_electrode_table()
  re$electrode_table <- electrode_table

  # ----- electrode_signal_types -----
  sel <- subject$electrodes %in% electrodes
  electrode_signal_types <- subject$electrode_types[sel]
  re$electrode_signal_types <- electrode_signal_types

  # ----- sample_rate -----
  sample_rate <- unique(subject$raw_sample_rates[sel])
  if(length(sample_rate) > 1) {
    stop(sprintf("Found more than different sample rates from the requested electrode channels [%s]. Please choose electrodes with the same sample rate.", paste(sprintf("%.0fHz", sample_rate), collapse = ", ")))
  }
  re$sample_rate <- sample_rate

  # ----- reference_instances -----
  ref_table <- reference_table[reference_table$Electrode %in% electrodes, ]
  references_list <- unique(ref_table$Reference)
  re$references_list <- references_list
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
  electrode_instances <- structure(lapply(seq_along(electrodes), function(ii){
    e <- electrodes[[ii]]
    signal_type <- electrode_signal_types[[ii]]

    el <- new_electrode(subject = subject, number = e, signal_type = signal_type, quiet = quiet)

    # set reference
    ref_name <- reference_table$Reference[reference_table$Electrode == e][[1]]
    ref_name <- sprintf("%s_%s", ref_name, signal_type)
    ref <- reference_instances[[ref_name]]
    el$set_reference(ref)

    el$set_epoch(epoch)
    el$trial_intervals <- time_windows

    el
  }), names = sprintf("e_%d", electrodes))
  re$electrode_instances <- electrode_instances

  # ----- load_data -----
  ref_mat <- unique(sprintf("%s_%s", re$reference_table[re$reference_table$Electrode %in% re$electrode_list, "Reference"], electrode_signal_types))
  ref_instances <- dipsaus::drop_nulls(re$reference_instances[ref_mat])
  if(length(ref_instances) < 4) {
    refs <- lapply_async(ref_instances, function(ref){
      ref$load_data(type = "voltage")
    }, callback = NULL)
  } else {
    refs <- lapply_async(ref_instances, function(ref){
      ref$load_data(type = "voltage")
    }, callback = function(ref){
      sprintf("Loading Electrode | %s", ref$number)
    })
  }

  data_list <- lapply_async(
    re$electrode_instances,
    function(inst) {
      inst$load_data(type = "voltage")
    },
    callback = function(inst) {
      sprintf("Loading voltage|Electrode %s", inst$number)
    }
  )
  names(data_list) <- names(electrode_instances)
  dim <- dim(data_list[[1]])
  dim[[3]] <- length(data_list)

  dimnames <- dimnames(data_list[[1]])
  dimnames[[3]] <- electrodes

  digest_key <- list(
    subject_id = subject$subject_id,
    epoch_table = epoch_table,
    electrodes = electrodes,
    rave_data_type = "voltage",
    electrode_signal_types = electrode_signal_types,
    sample_rate = sample_rate,
    time_windows = time_windows
  )
  digest_string <- dipsaus::digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  re$voltage <- dipsaus::list_to_fastmap2(list(
    dim = dim,
    data_list = data_list,
    dimnames = dimnames,
    signature = re$signature
  ))

  class(re) <- c("rave_prepare_subject_voltage_with_epoch", "rave_repository", "fastmap2", "list")
  re
}

