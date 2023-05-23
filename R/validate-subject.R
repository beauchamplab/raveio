#' @name validate_subject
#' @title Validate subject data integrity
#' @description Check against existence, validity, and consistency
#' @param subject subject ID (character), or \code{\link{RAVESubject}} instance
#' @param method validation method, choices are \code{'normal'} (default) or
#' \code{'basic'} for fast checks; if set to \code{'normal'}, four additional
#' validation parts will be tested (see parts with \code{*} in Section 'Value').
#' @param verbose whether to print out the validation messages
#' @param version data version, choices are \code{1} for 'RAVE' 1.0 data format,
#' and \code{2} ('RAVE' 2.0 data format); default is \code{2}
#' @returns A list of nested validation results. The validation process
#' consists of the following parts in order:
#' \describe{
#' \item{\strong{Data paths} (\code{paths})}{}
#' \item{\code{path}}{the subject's root folder}
#' \item{\code{path}}{the subject's 'RAVE' folder (the \code{'rave'} folder under the root directory)}
#' \item{\code{raw_path}}{the subject's raw data folder}
#' \item{\code{data_path}}{a directory storing all the voltage, power, phase data (before reference)}
#' \item{\code{meta_path}}{meta directory containing all the electrode coordinates, reference table, epoch information, etc.}
#' \item{\code{reference_path}}{a directory storing calculated reference signals}
#' \item{\code{preprocess_path}}{a directory storing all the preprocessing information}
#' \item{\code{cache_path} (low priority)}{data caching path}
#' \item{\code{freesurfer_path} (low priority)}{subject's 'FreeSurfer' directory}
#' \item{\code{note_path} (low priority)}{subject's notes}
#' \item{\code{pipeline_path} (low priority)}{a folder containing all saved pipelines for this subject}
#'
#' \item{\strong{Preprocessing information} (\code{preprocess})}{}
#' \item{\code{electrodes_set}}{whether the subject has a non-empty electrode set}
#' \item{\code{blocks_set}}{whether the session block length is non-zero}
#' \item{\code{sample_rate_set}}{whether the raw sampling frequency is set to a valid, proper positive number}
#' \item{\code{data_imported}}{whether all the assigning electrodes have been imported}
#' \item{\code{notch_filtered}}{whether all the 'LFP' and 'EKG' signals have been 'Notch' filtered}
#' \item{\code{has_wavelet}}{whether all the 'LFP' signals are wavelet-transformed}
#' \item{\code{has_reference}}{at least one reference has been generated in the meta folder}
#' \item{\code{has_epoch}}{at least one epoch file has been generated in the meta folder}
#' \item{\code{has_electrode_file}}{meta folder has \code{electrodes.csv} file}
#'
#' \item{\strong{Meta information} (\code{meta})}{}
#' \item{\code{meta_data_valid}}{this item only exists when the previous preprocess validation is failed or incomplete}
#' \item{\code{meta_electrode_table}}{the \code{electrodes.csv} file in the meta folder has correct format and consistent electrodes numbers to the preprocess information}
#' \item{\code{meta_reference_xxx}}{(\code{xxx} will be replaced with actual reference names) checks whether the reference table contains all electrodes and whether each reference data exists}
#' \item{\code{meta_epoch_xxx}}{(\code{xxx} will be replaced with actual epoch names) checks whether the epoch table has the correct formats and whether there are missing blocks indicated in the epoch files}
#'
#' \item{\strong{Voltage data} (\code{voltage_data*})}{}
#' \item{\code{voltage_preprocessing}}{whether the raw preprocessing voltage data are valid. This includes data lengths are the same within the same blocks for each signal type}
#' \item{\code{voltage_data}}{whether the voltage data (after 'Notch' filters) exist and readable. Besides, the lengths of the data must be consistent with the raw signals}
#'
#' \item{\strong{Spectral power and phase} (\code{power_phase_data*})}{}
#' \item{\code{power_data}}{whether the power data exists for all 'LFP' signals. Besides, to pass the validation process, the frequency and time-point lengths must be consistent with the preprocess record}
#' \item{\code{power_data}}{same as \code{power_data} but for the phase data}
#'
#' \item{\strong{Epoch table} (\code{epoch_tables*})}{
#' One or more sub-items depending on the number of epoch tables. To pass the validation, the event time for each session block must not exceed the actual signal duration. For example, if one session lasts for 200 seconds, it will invalidate the result if a trial onset time is later than 200 seconds.
#' }
#'
#' \item{\strong{Reference table} (\code{reference_tables*})}{
#' One or more sub-items depending on the number of reference tables. To pass the validation, the reference data must be valid. The inconsistencies, for example,
#' missing file, wrong frequency size, invalid time-point lengths will result
#' in failure
#' }
#' }
#'
#'
NULL
# A validate result is a list consisting of the following elements:
# 1. name
# 2. description
# 3. valid (TRUE or FALSE)
# 4. value (value if valid, otherwise characters or simpleError if invalid)

validate_result_new <- function(
    name, valid, value = NULL, description = name, message = NULL,
    ..., .verbose = FALSE, severity = c("major", "minor")) {

  severity <- match.arg(severity)
  valid <- as.logical(valid)
  stopifnot(length(valid) == 1)

  if(inherits(message, "condition")) {
    message <- message$message
  }

  if(isFALSE(valid)) {
    if(!length(message)) {
      message <- sprintf("`%s` is invalid", name)
    }

    re <- structure(
      list(
        name = name,
        valid = FALSE,
        message = message,
        value = value,
        description = description,
        severity = severity,
        ...
      ),
      class = c("RAVEValidationError", "RAVEValidation", "error", "condition")
    )
  } else if(is.na(valid)) {
    if(!length(message)) {
      message <- sprintf("`%s` is skipped", name)
    }
    re <- structure(
      list(
        name = name,
        valid = NA,
        message = message,
        value = value,
        description = description,
        severity = severity,
        ...
      ),
      class = c("RAVEValidationSkipped", "RAVEValidation", "condition")
    )
  } else {
    re <- structure(
      list(
        name = name,
        valid = TRUE,
        value = value,
        message = message,
        description = description,
        severity = severity,
        ...
      ),
      class = c("RAVEValidationSuccess", "RAVEValidation", "condition")
    )
  }
  if(.verbose) {
    print(re)
  }
  return(re)
}

validate_from_expression <- function(
    name, expr, env = parent.frame(), quoted = FALSE,
    description = name, ..., .verbose = FALSE) {

  if(!quoted) {
    expr <- substitute(expr)
  }
  valid <- FALSE

  value <- tryCatch({
    re <- eval(expr, envir = env)
    valid <- TRUE
    re
  }, error = function(e) {
    e
  })

  if(valid) {
    message <- NULL
  } else {
    message <- value
  }
  validate_result_new(
    name = name, description = description, .verbose = .verbose,
    valid = valid, value = value, message = message, ...
  )

}

#' @export
print.RAVEValidation <- function(x, use_logger = TRUE, ...) {
  valid <- x$valid
  if(isTRUE(valid)) {
    valid <- "yes"
  } else if (is.na(valid)) {
    valid <- "N/A (skipped)"
  } else {
    valid <- "no"
  }

  msg <- sprintf("%s... valid: %s%s", x$description, valid, ifelse(
    isTRUE(x$valid), "", sprintf("\n  reason: %s", x$message)
  ))
  if(use_logger) {
    if(isTRUE(x$valid)) {
      level <- "DEFAULT"
    } else if (is.na(x$valid)) {
      level <- "INFO"
    } else if(identical(x$severity, "minor")) {
      level <- "WARNING"
    } else {
      level <- "ERROR"
    }
    catgl("{msg}\n", level = level)
  } else {
    cat(msg, "\n", sep = "")
  }
  invisible(x)
}

validate_subject_paths <- function(subject, verbose = TRUE, other_checks = NULL) {

  subject <- restore_subject_instance(subject, strict = FALSE)

  # check paths
  path_names <- c("reference_path", "note_path", "pipeline_path", "cache_path",
                  "data_path", "preprocess_path", "freesurfer_path", "meta_path",
                  "rave_path", "path")

  low_priority <- c('freesurfer_path', 'note_path', 'cache_path', 'pipeline_path')

  check_path <- function(path, descr, path_name) {
    severity <- ifelse(path_name %in% low_priority, "minor", "major")
    if(length(path) != 1 || is.na(path) || path %in% c(".", "/", "") ||
       !dir.exists(path)) {
      re <- validate_result_new(
        name = path_name,
        valid = FALSE,
        value = path,
        message = sprintf("%s is missing", descr),
        severity = severity,
        description = sprintf("Subject [%s] %s", subject$subject_id, descr),
        .verbose = verbose
      )
    } else {
      re <- validate_result_new(
        name = path_name,
        valid = TRUE,
        value = path,
        message = sprintf("%s found!", descr),
        severity = severity,
        description = sprintf("Subject [%s] %s", subject$subject_id, descr),
        .verbose = verbose
      )
    }
    re
  }

  path_valid <- sapply(path_names, function(path_name) {
    path <- subject[[path_name]]
    descr <- gsub("_", " ", path_name)
    descr <- gsub("rave", "RAVE", descr, ignore.case = TRUE)
    check_path(path, descr, path_name)
  }, USE.NAMES = TRUE, simplify = FALSE)

  path_valid$raw_path <- check_path(
    subject$preprocess_settings$raw_path,
    "raw data path",
    path_name = "raw_path"
  )

  path_valid <- dipsaus::list_to_fastmap2(path_valid)
  if(inherits(other_checks, "fastmap2")) {
    other_checks$paths <- path_valid
  }

  return(invisible(path_valid))

}

validate_subject_preprocess <- function(subject, verbose = TRUE, other_checks = NULL) {
  subject <- restore_subject_instance(subject, strict = FALSE)
  preproc <- subject$preprocess_settings

  re <- dipsaus::fastmap2()

  if(inherits(other_checks, "fastmap2")) {
    other_checks$preprocess <- re
  }


  if(!isTRUE(preproc$valid())) {
    re$preprocess_validity <-
      validate_result_new(
        name = "preprocess_validity",
        valid = FALSE,
        description = sprintf("Subject [%s] preprocess log", subject$subject_id),
        message = sprintf("Invalid preprocessing log file: %s", preproc$path),
        .verbose = verbose
      )
    return(invisible(re))
  }

  # Whether electrode channels are set
  has_electrode <- TRUE
  electrodes <- preproc$electrodes
  if(!length(electrodes)) {
    re$electrodes_set <- validate_result_new(
      name = "electrodes_set",
      valid = FALSE,
      description = sprintf("Subject [%s] whether electrode channels are set",
                            subject$subject_id),
      message = "No electrode is set",
      .verbose = verbose
    )
    has_electrode <- FALSE
  } else {
    re$electrodes_set <- validate_result_new(
      name = "electrodes_set",
      valid = TRUE,
      description = sprintf("Subject [%s] whether electrode channels are set",
                            subject$subject_id),
      value = preproc$electrodes,
      .verbose = verbose
    )
  }

  # subject blocks
  if(!length(preproc$blocks)) {
    re$blocks_set <- validate_result_new(
      name = "blocks_set",
      valid = FALSE,
      description = sprintf("Subject [%s] whether session blocks are set",
                            subject$subject_id),
      message = "No block is set",
      .verbose = verbose
    )
  } else {
    re$blocks_set <- validate_result_new(
      name = "blocks_set",
      valid = TRUE,
      description = sprintf("Subject [%s] whether session blocks are set",
                            subject$subject_id),
      value = preproc$blocks,
      .verbose = verbose
    )
  }

  # sample rates
  srates <- preproc$sample_rates
  if(!isTRUE(all(srates > 1))) {
    invalids <- preproc$electrodes[!(srates > 1)]
    re$sample_rate_set <- validate_result_new(
      name = "sample_rate_set",
      valid = FALSE,
      description = sprintf("Subject [%s] whether sampling frequencies are valid",
                            subject$subject_id),
      message = sprintf("electrodes %s have invalid sample rates: %s",
                        dipsaus::deparse_svec(invalids), deparse1(srates[invalids])),
      .verbose = verbose
    )
  } else {
    re$sample_rate_set <- validate_result_new(
      name = "sample_rate_set",
      valid = TRUE,
      description = sprintf("Subject [%s] whether sampling frequencies are valid",
                            subject$subject_id),
      value = srates,
      .verbose = verbose
    )
  }

  if(!has_electrode) {
    return(invisible(re))
  }

  # Whether data has been imported yet
  if(!all(preproc$data_imported)) {
    invalids <- electrodes[!preproc$data_imported]
    re$data_imported <- validate_result_new(
      name = "data_imported",
      valid = FALSE,
      description = sprintf("Subject [%s] whether all electrode data are imported",
                            subject$subject_id),
      message = sprintf("the following electrode data are missing, please import them",
                        dipsaus::deparse_svec(invalids)),
      .verbose = verbose
    )
  } else {
    re$data_imported <- validate_result_new(
      name = "data_imported",
      valid = TRUE,
      description = sprintf("Subject [%s] whether all electrode data are imported",
                            subject$subject_id),
      .verbose = verbose
    )
  }

  # Notch filtered: only LFP, EKG channels need to be filtered
  notch_sel <- preproc$electrode_types %in% c("LFP", "EKG")
  notch_electrodes <- electrodes[notch_sel]
  notch_filtered <- preproc$notch_filtered[notch_sel]
  if(!length(notch_electrodes)) {
    re$notch_filtered <- validate_result_new(
      name = "notch_filtered",
      valid = NA,
      description = sprintf(
        "Subject [%s] whether Notch filters are applied to all LFP, EKG channels",
        subject$subject_id),
      message = "no LFP, EKG channels found",
      .verbose = verbose
    )
  } else if(!all(notch_filtered)) {
    invalids <- electrodes[!notch_filtered]
    re$notch_filtered <- validate_result_new(
      name = "notch_filtered",
      valid = FALSE,
      description = sprintf(
        "Subject [%s] whether Notch filters are applied to all LFP, EKG channels",
        subject$subject_id),
      message = sprintf(
        "please apply Notch filters to the following electrode channels %s",
        dipsaus::deparse_svec(invalids)),
      .verbose = verbose
    )
  } else {
    re$notch_filtered <- validate_result_new(
      name = "notch_filtered",
      valid = TRUE,
      value = preproc$notch_params,
      description = sprintf(
        "Subject [%s] whether Notch filters are applied to all LFP, EKG channels",
        subject$subject_id),
      .verbose = verbose
    )
  }

  # Wavelet: only LFP channels needs it
  wavelet_sel <- preproc$electrode_types %in% c("LFP")
  wavelet_electrodes <- electrodes[wavelet_sel]
  has_wavelet <- preproc$has_wavelet[wavelet_sel]
  if(!length(wavelet_electrodes)) {
    re$has_wavelet <- validate_result_new(
      name = "has_wavelet",
      valid = NA,
      description = sprintf(
        "Subject [%s] whether time-frequency decomposition has been applied to all LFP channels", subject$subject_id),
      message = "no LFP channels found",
      .verbose = verbose
    )
  } else if(!all(has_wavelet)) {
    invalids <- electrodes[!has_wavelet]
    re$has_wavelet <- validate_result_new(
      name = "has_wavelet",
      valid = FALSE,
      description = sprintf(
        "Subject [%s] whether time-frequency decomposition has been applied to all LFP channels", subject$subject_id),
      message = sprintf(
        "please apply time-frequency decomposition (e.g. Wavelet) to the following electrode channels %s", dipsaus::deparse_svec(invalids)),
      .verbose = verbose
    )
  } else {
    re$has_wavelet <- validate_result_new(
      name = "has_wavelet",
      valid = TRUE,
      description = sprintf(
        "Subject [%s] whether time-frequency decomposition has been applied to all LFP channels", subject$subject_id),
      value = preproc$wavelet_params,
      .verbose = verbose
    )
  }

  # Check if epoch files exist
  if(length(subject$epoch_names)) {
    re$has_epoch <- validate_result_new(
      name = "file existence: epoch_xxx.csv",
      valid = TRUE,
      description = sprintf(
        "Subject [%s] epoch files exist", subject$subject_id),
      value = subject$epoch_names,
      .verbose = verbose
    )
  } else {
    re$has_epoch <- validate_result_new(
      name = "file existence: epoch_xxx.csv",
      valid = FALSE,
      description = sprintf(
        "Subject [%s] epoch files exist", subject$subject_id),
      message = "Cannot find any epoch file matching [epoch_xxx.csv] in the [meta] folder",
      .verbose = verbose
    )
  }

  # Check if reference files exist
  if(length(subject$reference_names)) {
    re$has_reference <- validate_result_new(
      name = "file existence: reference_xxx.csv",
      valid = TRUE,
      description = sprintf(
        "Subject [%s] reference tables exist", subject$subject_id),
      value = subject$reference_names,
      .verbose = verbose
    )
  } else {
    re$has_reference <- validate_result_new(
      name = "file existence: reference_xxx.csv",
      valid = FALSE,
      description = sprintf(
        "Subject [%s] reference tables exist", subject$subject_id),
      message = "Cannot find any reference file matching [reference_xxx.csv] in the [meta] folder",
      .verbose = verbose
    )
  }

  # check if electrodes.csv exists
  electrode_file <- file.path(subject$meta_path, "electrodes.csv")
  if(file.exists(electrode_file)) {
    re$has_electrode_file <- validate_result_new(
      name = "file existence: electrodes.csv",
      valid = TRUE,
      description = sprintf(
        "Subject [%s] electrodes.csv", subject$subject_id),
      .verbose = verbose
    )
  } else {
    re$has_electrode_file <- validate_result_new(
      name = "file existence: electrodes.csv",
      valid = FALSE,
      message = "cannot find electrodes.csv in [meta] folder",
      description = sprintf(
        "Subject [%s] electrodes.csv", subject$subject_id),
      .verbose = verbose
    )
  }

  return(invisible(re))
}

validate_subject_meta <- function(subject, verbose = TRUE, other_checks = NULL) {

  subject <- restore_subject_instance(subject, strict = FALSE)

  re <- dipsaus::fastmap2()
  valid_preproc <- NULL

  if(inherits(other_checks, "fastmap2")) {
    other_checks$meta <- re
    valid_preproc <- other_checks$preprocess
  }
  if(is.null(valid_preproc)) {
    valid_preproc <- validate_subject_preprocess(subject, verbose = FALSE, other_checks = other_checks)
  }

  if(!isTRUE(valid_preproc$electrodes_set$valid)) {
    # no electrode
    re$meta_data_valid <- validate_result_new(
      name = "meta_data_valid",
      valid = NA,
      description = sprintf(
        "Subject [%s] has valid meta data", subject$subject_id),
      message = "no electrode is set: please finish the preprocessing steps",
      .verbose = verbose
    )
    return(invisible(re))
  }

  electrodes <- valid_preproc$electrodes_set$value

  # electrodes.csv
  if(isTRUE(valid_preproc$has_electrode_file$valid)) {

    re$meta_electrode_table <- validate_from_expression(
      name = "electrodes.csv",
      .verbose = verbose,
      description = sprintf(
        "Subject [%s] has valid electrode table (electrodes.csv)",
        subject$subject_id),
      expr = {
        electrode_path <- file.path(subject$meta_path, "electrodes.csv")
        tbl <- safe_read_csv(electrode_path)

        cols <- c("Electrode", "Coord_x", "Coord_y", "Coord_z", "Label")
        cols <- cols[!cols %in% names(tbl)]

        if(length(cols)) {
          stop("electrodes.csv is mising the following column(s): ",
               paste(cols, collapse = ", "))
        }

        miss_e <- electrodes[!electrodes %in% tbl$Electrode]
        if(length(miss_e)) {
          stop("The following electrodes are missing from the electrode table (column: Electrode): ", dipsaus::deparse_svec(miss_e))
        }
        tbl$Electrode <- as.integer(tbl$Electrode)

        if(any(is.na(tbl$Electrode))) {
          stop("electrode channel number must be an integer")
        }

        if(any(duplicated(tbl$Electrode))) {
          stop("duplicated electrode number is detected")
        }

        tbl

      }
    )
  }

  # epoch files (only check csv, not time-stamp: leave it later)
  if(isTRUE(valid_preproc$has_epoch$valid)) {

    epoch_names <- subject$epoch_names
    for(epoch_name in epoch_names) {
      vname <- sprintf("meta_epoch_%s", epoch_name)
      re[[vname]] <- validate_from_expression(
        name = sprintf("epoch_%s.csv", epoch_name),
        .verbose = verbose,
        description = sprintf(
          "Subject [%s] has valid epoch table (epoch_%s.csv)",
          subject$subject_id, epoch_name),
        expr = {
          tbl <- subject$meta_data(meta_type = "epoch", meta_name = epoch_name)

          if(!nrow(tbl)) {
            stop("this epoch contains no trial")
          }

          nms <- c("Block", "Time", "Trial", "Condition")
          nms <- nms[!nms %in% names(tbl)]
          if(length(nms)) {
            stop("epoch table is missing the following columns: ",
                 paste(nms, collapse = ", "))
          }

          # check if block is valid
          tbl_blocks <- unique(tbl$Block)
          tbl_blocks <- tbl_blocks[!tbl_blocks %in% subject$blocks]
          if(length(tbl_blocks)) {
            stop("the following blocks exist in the epoch table but invalid or not imported: ", paste(tbl_blocks, collapse = ', '))
          }

          if(!is.numeric(tbl$Time)) {
            stop("epoch table onset time (colume: Time) contains non-numeric")
          }
          if(!all(as.integer(tbl$Trial) == tbl$Trial)) {
            stop("epoch table trial index (colume: Trial) must be all positive integers")
          }

          if(any(duplicated(tbl$Trial))) {
            stop("duplicated trial number is detected in the epoch file")
          }

          # check event column
          nms <- names(tbl)
          sel <- grepl("^Event_[a-zA-Z0-9_]", nms, ignore.case = FALSE)
          if(any(sel)) {
            nms <- nms[sel]
            events <- gsub("^Event_", "", nms)
            for(ii in seq_along(nms)) {
              if(!is.numeric(tbl[[nms[[ii]]]])) {
                stop(sprintf("detected event [%s] in the epoch table (colume: [%s]) containing non-numerical values", events[[ii]], nms[[ii]]))
              }
            }
          }

          tbl
        }
      )
    }

  }

  # reference files (only check against electrodes.csv, no actual data validation)
  if(valid_preproc$has_reference$valid) {

    reference_names <- subject$reference_names
    for(reference_name in reference_names) {
      vname <- sprintf("meta_reference_%s", reference_name)
      re[[vname]] <- validate_from_expression(
        name = sprintf("reference_%s.csv", reference_name),
        .verbose = verbose,
        description = sprintf(
          "Subject [%s] has valid reference table (reference_%s.csv)",
          subject$subject_id, reference_name),
        expr = {
          tbl <- subject$meta_data(meta_type = "reference", meta_name = reference_name)

          if(!nrow(tbl)) {
            stop("this reference table is empty")
          }

          nms <- c("Electrode", "Group", "Reference", "Type")
          nms <- nms[!nms %in% names(tbl)]
          if(length(nms)) {
            stop("reference table is missing the following columns: ",
                 paste(nms, collapse = ", "))
          }

          # check electrodes
          miss_e <- electrodes[!electrodes %in% tbl$Electrode]
          if(length(miss_e)) {
            stop("the reference parameters are missing for the following electrode channels: ", dipsaus::deparse_svec(miss_e))
          }

          if(any(duplicated(tbl$Electrode))) {
            stop("duplicated electrode number detected in the reference file")
          }

          ref_names <- unique(tbl$Reference)
          for(ref_name in ref_names) {
            ref_name <- trimws(ref_name)
            if(!ref_name %in% c("noref", "")) {
              e <- new_reference(subject = subject, number = ref_name)
              if(!isTRUE(e$exists)) {
                stop("Cannot find reference data file for ", ref_name)
              }
            }
          }
        }
      )
    }
  }

  return(invisible(re))

}

validate_subject_voltage <- function(subject, version = 2, verbose = TRUE, other_checks = NULL) {

  subject <- restore_subject_instance(subject, strict = FALSE)

  re <- dipsaus::fastmap2()
  valid_preproc <- NULL
  if(inherits(other_checks, "fastmap2")) {
    other_checks$voltage_data <- re
    valid_preproc <- other_checks$preprocess
  }
  if(is.null(valid_preproc)) {
    valid_preproc <- validate_subject_preprocess(subject, verbose = FALSE,
                                                 other_checks = other_checks)
  }
  if(!isTRUE(valid_preproc$data_imported$valid)) {
    re$voltage_data <- validate_result_new(
      name = "voltage_data",
      valid = FALSE,
      description = sprintf("Subject [%s] voltage data (analog traces)", subject$subject_id),
      message = "Please import all the electrode data first",
      .verbose = verbose
    )
    return(invisible(re))
  }

  electrodes <- subject$preprocess_settings$electrodes
  electrode_types <- subject$electrode_types
  blocks <- subject$blocks
  notch_filtered <- subject$notch_filtered


  # preprocessing voltage
  re$voltage_preprocessing <- validate_from_expression(
    name = 'voltage_preprocessing',
    .verbose = verbose,
    description = sprintf(
      "Subject [%s] preprocessing voltage data",
      subject$subject_id),
    expr = {
      preprocess_path <- subject$preprocess_path
      pre_elecs <- file.path(preprocess_path, 'voltage',
                             sprintf('electrode_%d.h5', electrodes))
      fe <- file.exists(pre_elecs)
      if(!all(fe)) {
        miss_e <- electrodes[!fe]
        stop("the following electrode channels are set but cannot find data files; please import the channel data: ", dipsaus::deparse_svec(miss_e))
      }

      # go into each channel files and check the length

      signal_lengths <- with_future_parallel({
        lapply_async(seq_along(electrodes), function(ii){
          e <- electrodes[[ii]]
          pre_elec <- file.path(preprocess_path, 'voltage',
                                sprintf('electrode_%d.h5', e))

          electrode_type <- electrode_types[[ii]]
          has_notch <- notch_filtered[[ii]]

          h5names <- tryCatch({
            gsub("^/", "", h5_names(pre_elec))
          }, error = function(e) {
            NULL
          })
          if(!length(h5names)) {
            return(rep(-1, length(blocks)))
          }

          # check length of raw (unreferenced)
          signal_lengths <- sapply(blocks, function(b){
            raw_len <- tryCatch({
              name <- sprintf('raw/%s', b)
              stopifnot(name %in% h5names)
              length(load_h5(pre_elec, name = name, ram = FALSE))
            }, error = function(e){-1})

            if( raw_len <= 0 ){
              return(raw_len)
            }

            if(has_notch && electrode_type %in% c("EKG", "LFP")){
              notch_len <- tryCatch({
                name <- sprintf('notch/%s', b)
                stopifnot(name %in% h5names)
                length(load_h5(pre_elec, name = name, ram = FALSE))
              }, error = function(e){-1})
            }else{
              notch_len <- raw_len
            }
            if( raw_len != notch_len ){
              return(0)
            }
            return(raw_len)
          })
          signal_lengths
        }, callback = function(ii) {
          sprintf('Checking preprocess data|electrode %d', electrodes[[ii]])
        })
      })

      signal_lengths <- do.call('rbind', signal_lengths)
      # check corrupted file - raw preprocess
      corrupted <- rowSums(signal_lengths < 0) > 0
      unequallen <- rowSums(signal_lengths == 0) > 0


      if(any(corrupted)){
        stop(
          "Corrupted preprocess files found in these electrodes ",
          dipsaus::deparse_svec(electrodes[corrupted]))
      }else if(any(unequallen)){
        stop(
          "Notch filter produces different lengths compared to original signals in these electrodes ",
          dipsaus::deparse_svec(electrodes[unequallen]))
      }

      signal_lengths <- as.data.frame(signal_lengths)
      names(signal_lengths) <- blocks
      signal_lengths$Electrode <- electrodes
      signal_lengths$Type <- electrode_types
      signal_lengths$SampleRate <- subject$raw_sample_rates
      signal_lengths
    }
  )

  if(!isTRUE(re$voltage_preprocessing$valid)) {
    re$voltage_data <- validate_result_new(
      name = "voltage_data",
      .verbose = TRUE,
      description = sprintf(
        "Subject [%s] voltage data exists after preprocessing",
        subject$subject_id),
      valid = NA,
      message = "errors found in preprocessing data, skipping.."
    )
    return(invisible(re))
  }

  preproc_tbl <- re$voltage_preprocessing$value

  # actual voltage files
  volt_path <- file.path(subject$data_path, "voltage")

  re$voltage_data <- validate_from_expression(
    name = 'voltage_data',
    .verbose = verbose,
    description = sprintf(
      "Subject [%s] voltage data exists after preprocessing",
      subject$subject_id),
    expr = {
      volt_files <- file.path(volt_path, sprintf('%d.h5', electrodes))
      fe <- file.exists(volt_files)
      if(!all(fe)) {
        stop("cannot find voltage data under [data/voltage/] for the following electrode channels: ", dipsaus::deparse_svec(electrodes[!fe]))
      }

      # check signal lengths
      with_future_parallel({
        length_valid <- lapply_async(seq_len(nrow(preproc_tbl)), function(ii) {
          e <- preproc_tbl$Electrode[[ii]]
          f <- file.path(volt_path, sprintf('%d.h5', e))
          signal_length <- data.matrix(preproc_tbl[ii, blocks, drop = FALSE])

          if(!file.exists(f)){
            return(c(FALSE, FALSE, FALSE))
          }
          h5names <- tryCatch({
            gsub("^/", "", h5_names(f))
          }, error = function(e) {
            NULL
          })
          if(!length(h5names)) {
            return(c(FALSE, FALSE, FALSE))
          }

          # check length of raw (unreferenced)
          raw_lens <- sapply(blocks, function(b){
            tryCatch({
              name <- sprintf('raw/voltage/%s', b)
              stopifnot(name %in% h5names)
              raw <- load_h5(f, name, ram = FALSE)
              length(raw)
            }, error = function(e){
              0
            })
          })

          ref_lens <- sapply(blocks, function(b){
            tryCatch({
              name <- sprintf('ref/voltage/%s', b)
              stopifnot(name %in% h5names)
              raw <- load_h5(f, name, ram = FALSE)
              length(raw)
            }, error = function(e){
              0
            })
          })

          c(
            isTRUE(all(raw_lens > 0)),
            isTRUE(all(raw_lens == signal_length)),
            isTRUE(all(ref_lens == signal_length))
          )

        }, callback = function(ii) {
          sprintf('Checking voltage data|electrode %d', electrodes[[ii]])
        })
        length_valid <- do.call('rbind', length_valid)
      })

      if(!all(length_valid[,1])) {
        stop("voltage data under [data/voltage/] might be corrupted or preprocess unfinished: ", dipsaus::deparse_svec(electrodes[!length_valid[,1]]))
      }
      if(!all(length_valid[,2])) {
        stop("the following electrodes have inconsistent signal lengths [data/voltage/] vs [raw]: ", dipsaus::deparse_svec(electrodes[!length_valid[,2]]))
      }

      if(version < 2 && !all(length_valid[,3])) {
        stop("the following electrodes have inconsistent cache lengths [data/voltage/]: ", dipsaus::deparse_svec(electrodes[!length_valid[,3]]))
      }

    }
  )

  return(invisible(re))
}

validate_subject_power_phase <- function(subject, version = 2, verbose = TRUE, other_checks = NULL) {
  subject <- restore_subject_instance(subject, strict = FALSE)

  re <- dipsaus::fastmap2()
  valid_preproc <- NULL
  if(inherits(other_checks, "fastmap2")) {
    other_checks$power_phase_data <- re
    valid_preproc <- other_checks$voltage_data
  }
  if(is.null(valid_preproc)) {
    valid_preproc <- validate_subject_voltage(
      subject, verbose = FALSE, version = version,
      other_checks = other_checks)
  }

  if(!isTRUE(valid_preproc$voltage_preprocessing$valid)) {
    re$power_phase <- validate_result_new(
      name = "power_phase",
      valid = NA,
      description = sprintf(
        "Subject [%s] has valid power/phase data", subject$subject_id),
      message = "preprocessing data is invalid, skipping",
      .verbose = verbose
    )
    return(invisible(re))
  }

  # only the LFP electrodes are checked at the moment
  tbl <- valid_preproc$voltage_preprocessing$value
  tbl$PowerSrate <- subject$power_sample_rate
  sel <- tbl$Type %in% c("LFP")

  if(!any(sel) || any(is.na(tbl$PowerSrate[sel]))) {
    re$power_phase <- validate_result_new(
      name = "power_phase",
      valid = NA,
      description = sprintf(
        "Subject [%s] has valid power/phase data", subject$subject_id),
      message = "no LFP channel detected or the power/phase haven't been calculated yet; please finish the preprocessing steps... skipping",
      .verbose = verbose
    )
    return(invisible(re))
  }

  # sample rates must be consistent for all LFP electrodes
  if(length(unique(tbl$PowerSrate[sel])) != 1) {
    re$power_phase <- validate_result_new(
      name = "power_phase",
      valid = FALSE,
      description = sprintf(
        "Subject [%s] has valid power/phase data", subject$subject_id),
      message = "all LFP power/phase must share the sample rates after time-frequency decomposition",
      .verbose = verbose
    )
    return(invisible(re))
  }


  # go into files
  check_data <- function(dtype) {
    wavelet_params <- subject$preprocess_settings$wavelet_params
    if(!is.list(wavelet_params)) {
      stop("cannot retrieve frequency information")
    }
    frequencies <- wavelet_params$frequencies
    n_freq <- length(frequencies)
    if(!n_freq) {
      stop("zero frequency detected")
    }
    blocks <- subject$blocks
    signal_lengths <- data.matrix(tbl[, blocks, drop = FALSE])
    expected_length <- floor((signal_lengths - 1) / subject$raw_sample_rates *
                               subject$power_sample_rate) + 1
    # power/phase directories
    dpath <- file.path(subject$data_path, dtype)
    fs <- file.path(dpath, sprintf("%d.h5", tbl$Electrode[sel]))
    fe <- file.exists(fs)
    if(!all(fe)) {
      miss_e <- tbl$Electrode[sel][!fe]
      stop(sprintf("Cannot find %s data for the following electrode channels under [data/%s] directory: %s", dtype, dtype, dipsaus::deparse_svec(miss_e)))
    }

    dtype_checks <- with_future_parallel({
      dtype_checks <- lapply_async(which(sel), function(ii) {
        e <- tbl$Electrode[[ii]]
        f <- file.path(dpath, sprintf('%d.h5', e))

        el <- expected_length[ii, ]

        if(!file.exists(f)){
          return(c(FALSE, FALSE, FALSE, FALSE, FALSE))
        }
        h5names <- tryCatch({
          gsub("^/", "", h5_names(f))
        }, error = function(e) {
          NULL
        })
        if(!length(h5names)) {
          return(c(FALSE, FALSE, FALSE, FALSE, FALSE))
        }

        # check length
        raw_lens <- sapply(blocks, function(b){
          tryCatch({
            name <- sprintf('raw/%s/%s', dtype, b)
            stopifnot(name %in% h5names)
            raw <- load_h5(f, name, ram = FALSE)
            raw <- dim(raw)
            if(length(raw) != 2) { return(c(0, 0)) }
            raw
          }, error = function(e){
            c(0, 0)
          })
        })
        ref_lens <- sapply(blocks, function(b){
          tryCatch({
            name <- sprintf('ref/%s/%s', dtype, b)
            stopifnot(name %in% h5names)
            raw <- load_h5(f, name, ram = FALSE)
            raw <- dim(raw)
            if(length(raw) != 2) { return(c(0, 0)) }
            raw
          }, error = function(e){
            c(0, 0)
          })
        })

        c(
          isTRUE(all(raw_lens > 0)),
          isTRUE(all(raw_lens[1,] == n_freq)),
          isTRUE(all(raw_lens[1,] == n_freq)),

          # not strict, but works in 99.999% cases
          isTRUE(all(abs(raw_lens[2, ] - el) < 10)),
          isTRUE(all(ref_lens[2, ] == raw_lens[2, ]))
        )
      }, callback = function(ii) {
        sprintf('Checking %s data|electrode %d', dtype, tbl$Electrode[[ii]])
      })
      do.call("rbind", dtype_checks)
    })

    lfp_s <- tbl$Electrode[sel]
    if(!all(dtype_checks[,1])) {
      stop(sprintf("The following electrode channels may have corrupted %s data files ([data/%s:unreferenced]): %s", dtype, dtype, dipsaus::deparse_svec(lfp_s[!dtype_checks[,1]])))
    }
    if(!all(dtype_checks[,2])) {
      stop(sprintf("The following electrode channels have inconsistent number of frequencies to expected ([data/%s:unreferenced]): %s", dtype, dipsaus::deparse_svec(lfp_s[!dtype_checks[,2]])))
    }
    if(!all(dtype_checks[,4])) {
      stop(sprintf("The following electrode channels have inconsistent number of time-points to expected ([data/%s:unreferenced]): %s", dtype, dipsaus::deparse_svec(lfp_s[!dtype_checks[,4]])))
    }
    if(version < 2) {

      if(!all(dtype_checks[,3])) {
        stop(sprintf("The following electrode channels have inconsistent number of frequencies to expected ([data/%s:referenced]): %s", dtype, dipsaus::deparse_svec(lfp_s[!dtype_checks[,3]])))
      }
      if(!all(dtype_checks[,5])) {
        stop(sprintf("The following electrode channels have inconsistent number of time-points to expected ([data/%s:referenced]): %s", dtype, dipsaus::deparse_svec(lfp_s[!dtype_checks[,5]])))
      }

    }

  }
  re$power_data <- validate_from_expression(
    name = 'power_data',
    .verbose = verbose,
    description = sprintf(
      "Subject [%s] has valid power data", subject$subject_id),
    expr = {
      check_data("power")
    }
  )
  re$phase_data <- validate_from_expression(
    name = 'phase_data',
    .verbose = verbose,
    description = sprintf(
      "Subject [%s] has valid phase data", subject$subject_id),
    expr = {
      check_data("phase")
    }
  )

  re$power_phase <- validate_result_new(
    name = "power_phase",
    valid = re$power_data$valid && re$phase_data$valid,
    description = sprintf(
      "Subject [%s] has valid power/phase data", subject$subject_id),
    message = "all LFP power/phase must share the sample rates after time-frequency decomposition",
    .verbose = FALSE
  )
  return(invisible(re))
}

# epoch length
validate_subject_epoch <- function(subject, verbose = TRUE, other_checks = NULL) {
  subject <- restore_subject_instance(subject, strict = FALSE)

  re <- dipsaus::fastmap2()
  valid_preproc <- NULL
  if(inherits(other_checks, "fastmap2")) {
    other_checks$epoch_tables <- re
    valid_preproc <- other_checks$voltage_data
  }
  if(is.null(valid_preproc)) {
    valid_preproc <- validate_subject_voltage(
      subject, verbose = FALSE,
      other_checks = other_checks)
  }

  if(!isTRUE(valid_preproc$voltage_preprocessing$valid)) {
    re$epoch <- validate_result_new(
      name = "epoch",
      valid = NA,
      description = sprintf(
        "Subject [%s] has valid time-stamps in epoch table", subject$subject_id),
      message = "preprocessing data is invalid, skipping",
      .verbose = verbose
    )
    return(invisible(re))
  }

  tbl <- valid_preproc$voltage_preprocessing$value
  blocks <- subject$blocks
  tbl_names <- names(tbl)
  max_time <- tbl[, blocks, drop = FALSE] / tbl$SampleRate
  session_maxtime <- sapply(blocks, function(b) {
    if(!b %in% tbl_names) { return(NULL) }
    x <- max_time[[b]]
    x <- x[!is.na(x)]
    if(length(x)) { max(x) } else { NULL }
  }, simplify = FALSE, USE.NAMES = TRUE)
  session_maxtime <- dipsaus::drop_nulls(session_maxtime)
  session_maxtstr <- paste(
    sprintf("%s (%.2fs)", names(session_maxtime), as.vector(session_maxtime)),
    collapse = ", "
  )

  # for each epoch
  check_epoch <- function(epoch) {

    epoch_tbl <- subject$meta_data(meta_type = "epoch", meta_name = epoch)
    subs <- split(epoch_tbl, epoch_tbl$Block)
    cols <- names(epoch_tbl)
    cols <- cols[grepl("(^Event_[a-zA-Z0-9_]+$)|(^Time$)", cols)]

    for(col in cols) {
      invalid_trials <- lapply(subs, function(sub) {
        block <- sub$Block[[1]]
        max_time <- session_maxtime[[block]]
        if(length(max_time) != 1 || max_time <= 0) {
          stop(sprintf(
            "cannot obtain the duration of session block [%s] from epoch [%s]",
            block, epoch))
        }
        x <- sub[[col]]
        sub$Trial[is.na(x) | x > max_time | x <= 0 ]
      })
      invalid_trials <- unlist(invalid_trials)
      if(length(invalid_trials)) {
        stop(sprintf(
          "found invalid time in column [%s] from epoch [%s]; please make sure the onset/event time does not exceed the maximum duration of that session: %s",
          col, epoch, session_maxtstr
        ))
      }
    }
    return()

  }
  for(epoch in subject$epoch_names){
    re[[epoch]] <- validate_from_expression(
      name = sprintf("epoch_%s.csv", epoch),
      .verbose = verbose,
      description = sprintf(
        "Subject [%s] has valid epoch table [meta/epoch_%s.csv]",
        subject$subject_id, epoch),
      expr = {
        check_epoch(epoch)
      }
    )
  }

  return(invisible(re))

}

# reference
validate_subject_reference <- function(subject, verbose = TRUE, other_checks = NULL) {
  subject <- restore_subject_instance(subject, strict = FALSE)

  re <- dipsaus::fastmap2()
  valid_preproc <- NULL
  if(inherits(other_checks, "fastmap2")) {
    other_checks$reference_tables <- re
    valid_preproc <- other_checks$voltage_data
  }
  if(is.null(valid_preproc)) {
    valid_preproc <- validate_subject_voltage(
      subject, verbose = FALSE,
      other_checks = other_checks)
  }

  if(!isTRUE(valid_preproc$voltage_preprocessing$valid)) {
    re$reference <- validate_result_new(
      name = "reference",
      valid = NA,
      description = sprintf(
        "Subject [%s] has valid reference data", subject$subject_id),
      message = "preprocessing data is invalid, skipping",
      .verbose = verbose
    )
    return(invisible(re))
  }

  tbl <- valid_preproc$voltage_preprocessing$value
  blocks <- subject$blocks
  reference_names <- subject$reference_names
  n_freq <- length(subject$preprocess_settings$wavelet_params$frequencies)
  power_srate <- subject$power_sample_rate

  check_reference <- function(reference) {
    ref_tbl <- subject$meta_data(meta_type = 'references', meta_name = reference)
    ref_names <- unique(ref_tbl$Reference)
    ref_names <- trimws(ref_names)
    ref_names <- ref_names[!ref_names %in% c("", "noref")]
    if(!length(ref_names)) {
      return()
    }
    lapply(ref_names, function(ref_name){
      e <- new_reference(subject, ref_name)
      if(!isTRUE(e$valid)) {
        stop("reference data [data/reference/", ref_name, ".h5] is missing")
      }
      if(!is.character(e$number)) {
        # reference to single electrode, redundant as it has been checked
        # in power_phase
        return()
      }
      # check HDF5 file
      ref_file <- e$voltage_file
      h5names <- gsub("^/", "", h5_names(ref_file))
      if(!length(h5names)) {
        stop("reference data [data/reference/", ref_name, ".h5] is corrupted")
      }
      # voltage
      volt_names <- sprintf("voltage/%s", blocks)
      sel <- volt_names %in% h5names
      if(!all(sel)) {
        stop("reference data [data/reference/", ref_name, ".h5] is does not contain voltage data for the following blocks: ", paste(blocks[!sel], collapse = ", "))
      }
      # get signal length for the type
      signal_lengths <- tbl[tbl$Type %in% e$type, blocks, drop = FALSE]
      if(!nrow(signal_lengths)) {
        stop(sprintf("reference %s has signal type [%s], but no electrode channel of such type is found: this reference might be obsolete", ref_name, e$type))
      }
      signal_lengths <- signal_lengths[1, , drop = FALSE]

      for(block in blocks) {
        explen <- signal_lengths[[block]]
        actlen <- length(load_h5(
          file = ref_file,
          name = sprintf("voltage/%s", block),
          read_only = TRUE,
          ram = FALSE
        ))
        if(explen != actlen) {
          stop(sprintf("reference %s has inconsistent voltage length in block [%s]: expected: %.0f vs. actual: %.0f", ref_name, block, explen, actlen))
        }
      }

      if(e$type %in% c("LFP", "EKG")) {

        if(!n_freq) {
          stop("cannot obtain frequency information from preprocessing log files")
        }
        if(!isTRUE(power_srate > 1)) {
          stop("cannot obtain power/phase sample rates from preprocessing log files")
        }

        # wavelet coefficients
        wave_names <- sprintf("wavelet/coef/%s", blocks)
        sel <- wave_names %in% h5names
        if(!all(sel)) {
          stop("reference data [data/reference/", ref_name, ".h5] is does not contain time-frequency decomposition data for the following blocks: ", paste(blocks[!sel], collapse = ", "))
        }
        # get signal length for the type
        signal_lengths <- (tbl[, blocks, drop = FALSE] - 1) * (power_srate / tbl$SampleRate)
        signal_lengths <- signal_lengths[tbl$Type %in% e$type, , drop = FALSE]
        if(!nrow(signal_lengths)) {
          stop(sprintf("reference %s has signal type [%s], but no electrode channel of such type is found: this reference might be obsolete", ref_name, e$type))
        }
        signal_lengths <- signal_lengths[1, , drop = FALSE]
        for(block in blocks) {
          expdim <- c(n_freq, signal_lengths[[block]], 2)
          actdim <- dim(load_h5(
            file = ref_file,
            name = sprintf("wavelet/coef/%s", block),
            read_only = TRUE,
            ram = FALSE
          ))
          if(length(actdim) != 3 || actdim[[3]] != 2) {
            stop(sprintf("reference %s has corrupted wavelet data in block [%s]", ref_name, block))
          }
          if(expdim[[1]] != actdim[[1]]) {
            stop(sprintf("reference %s has inconsistent wavelet frequencies in block [%s]: expected: %.0f vs. actual: %.0f", ref_name, block, expdim[[1]], actdim[[1]]))
          }
          if(abs(expdim[[2]] - actdim[[2]]) > 10) {
            stop(sprintf("reference %s has inconsistent wavelet time-points in block [%s]: expected: %.0f vs. actual: %.0f", ref_name, block, floor(expdim[[2]]) + 1, actdim[[2]]))
          }
        }
      }

      return()
    })

  }

  for(reference in subject$reference_names){
    re[[reference]] <- validate_from_expression(
      name = sprintf("reference_%s.csv", reference),
      .verbose = verbose,
      description = sprintf(
        "Subject [%s] has valid reference table [meta/reference_%s.csv]",
        subject$subject_id, reference),
      expr = {
        check_reference(reference)
      }
    )
  }

  return(invisible(re))
}

# cache

#' @export
validate_subject <- function(
    subject, method = c("normal", "basic", "all"), verbose = TRUE, version = 2) {

  method <- match.arg(method)

  subject <- as_rave_subject(subject, strict = FALSE)
  results <- dipsaus::fastmap2()

  validate_subject_paths(subject = subject, verbose = verbose, other_checks = results)
  validate_subject_preprocess(subject = subject, verbose = verbose, other_checks = results)
  validate_subject_meta(subject = subject, verbose = verbose, other_checks = results)

  if(method %in% c("normal", "all")) {
    validate_subject_voltage(subject = subject, verbose = verbose, other_checks = results, version = version)
    validate_subject_power_phase(subject = subject, verbose = verbose, other_checks = results, version = version)

    validate_subject_epoch(subject = subject, verbose = verbose, other_checks = results)
    validate_subject_reference(subject = subject, verbose = verbose, other_checks = results)
  }

  return(results)
}
