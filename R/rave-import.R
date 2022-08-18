is_physical_unit <- function(unit, choices){
  if(is_valid_ish(unit, max_len = 1, mode = 'character') && unit %in% choices){
    return(TRUE)
  }
  return(FALSE)
}

volc_units <- c('V', 'mV', 'uV')

#' Returns a list of 'RAVE' directories
#' @description This function is internally used and should not be called
#' directly.
#' @param subject_code 'RAVE' subject code
#' @param project_name 'RAVE' project name
#' @param blocks session or block names, optional
#' @param .force_format format of the data, default is automatically detected.
#' @return A list of directories
#' @export
rave_directories <- function(subject_code, project_name, blocks = NULL, .force_format = c('', 'native', 'BIDS')){
  .force_format <- match.arg(.force_format)
  re <- dipsaus::fastmap2()

  subject_code <- stringr::str_remove(subject_code, '^sub-')

  re$root_data <- normalizePath(raveio_getopt('data_dir'), mustWork = FALSE)

  # check file structure mode
  fstruct <- raveio_getopt('file_structure')

  bids_raw <- normalizePath(raveio_getopt('bids_data_dir'), mustWork = FALSE)
  # raw path
  re$root_raw <- normalizePath(raveio_getopt('raw_data_dir'), mustWork = FALSE)
  re$raw_path <- file.path(re$root_raw, subject_code)
  re$.raw_path_type <- "native"
  if(!dir.exists(re$raw_path)){
    raw_path <- file.path(bids_raw, project_name, sprintf('sub-%s', subject_code))
    if(dir.exists(raw_path)){
      re$root_raw <- bids_raw
      re$raw_path <- raw_path
      re$.raw_path_type <- "bids"
    }
  }


  # TODO: in RAVE 2.0, BIDS should be supported and native path should be
  # supported.
  if(fstruct == 'BIDS' || .force_format == 'BIDS'){
    re$bids_project_path <- file.path(bids_raw, project_name)
    re$bids_subject_path <- file.path(re$bids_project_path, sprintf('sub-%s', subject_code))
    re$project_path <- file.path(re$root_data, project_name, 'derivatives', 'rave', project_name)
  } else {
    re$project_path <- file.path(re$root_data, project_name)
  }
  re$group_data_path <- file.path(re$project_path, '_project_data')
  re$subject_path <- file.path(re$project_path, subject_code)
  re$rave_path <- file.path(re$subject_path, 'rave')
  re$note_path <- file.path(re$subject_path, 'notes', 'rave_notes')
  re$proprocess_path <- file.path(re$rave_path, 'preprocess')
  re$meta_path <- file.path(re$rave_path, 'meta')
  re$data_path <- file.path(re$rave_path, 'data')
  re$reference_path <- file.path(re$data_path, 'reference')

  re$pipeline_path <- file.path(re$rave_path, 'pipeline')



  return(re)
}

rave_import_lfp <- function(
  project_name, subject_code, blocks, electrodes,
  sample_rate, conversion = NA, add = FALSE,
  data_type = 'LFP', ...
) {
  pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))

  if(!add && isTRUE(pretools$`@freeze_lfp_ecog`)){
    # LFP has been imported, just stop
    stop(catgl('Subject {project_name}/{subject_code} has been imported previously. Please proceed to next step.'))
  }

  method <- class(project_name)[[1]]

  if(!isTRUE(list(...)[["skip_validation"]])){
    # perform validation
    res <- do.call(
      sprintf('validate_raw_file_lfp.%s', method), list(
        subject_code = subject_code,
        blocks = blocks,
        electrodes = electrodes,
        check_content = TRUE,
        project_name = project_name
      )
    )

    if(!res){
      reasons <- attr(res, 'reason')
      if(!is.list(reasons) || !length(reasons)){ stop('rave_import error: unknown reason.') }
      msg <- sapply(seq_along(reasons), function(ii){
        nm <- names(reasons)[[ii]]
        items <- reasons[[ii]]
        paste0(ii, ' - ', nm, '\n', paste0('    ', items, collapse = '\n'))
      })
      stop('The following issues found when importing subject ',
           sQuote(subject_code), ' into project ', sQuote(project_name),
           '.\n', msg, call. = match.call())
    }
  }

  # Not imported, import
  re <- UseMethod('rave_import_lfp')

  subject <- RAVESubject$new(project_name = project_name,
                             subject_code = subject_code,
                             strict = FALSE)
  electrodes <- dipsaus::parse_svec(electrodes)
  tryCatch({
    # Save to electrodes.csv
    has_fs <- !is.null(rave_brain(subject))

    # check if electrodes.csv exists
    orig <- subject$get_electrode_table(reference_name = ".fake", simplify = FALSE)


    if(!setequal(orig$Electrode, electrodes)) {
      stop("Electrode set is wrong")
    }

    if(has_fs) {
      # Try to import
      raveio::import_electrode_table(
        path = file.path(subject$meta_path, "electrodes.csv"),
        subject = subject, use_fs = has_fs)
    }

  }, error = function(e){
    catgl("Cannot import from existing electrodes.csv, creating a new one", level = "INFO")
    tbl <- data.frame(
      Electrode = subject$electrodes,
      Coord_x = 0, Coord_y = 0, Coord_z = 0,
      Label = "NoLabel",
      SignalType = subject$electrode_types
    )
    raveio::save_meta2(
      data = tbl,
      meta_type = "electrodes",
      project_name = project_name,
      subject_code = subject_code
    )
  })

  return(re)
}

rave_import_lfp.native_matlab <- function(project_name, subject_code, blocks,
                                          electrodes, sample_rate, add = FALSE,
                                          conversion = NA, data_type = 'LFP', ...){
  .fs_struct <- raveio_getopt('file_structure')
  on.exit({
    raveio_setopt('file_structure', .fs_struct, .save = TRUE)
  }, add = TRUE)
  raveio_setopt('file_structure', 'native', .save = FALSE)
  # direct import data, no check (already checked)
  pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))

  if(!add){
    pretools$set_blocks(blocks = blocks)
  }
  pretools$set_electrodes(electrodes, type = data_type, add = add)
  pretools$set_sample_rates(sample_rate, type = data_type)
  pretools$subject$initialize_paths(include_freesurfer = FALSE)
  pretools$save()

  # Now import data
  res <- validate_raw_file_lfp.native_matlab(
    subject_code = subject_code,
    blocks = blocks,
    electrodes = electrodes,
    check_content = FALSE,
    project_name = project_name
  )

  save_path <- file.path(pretools$subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)

  if(isTRUE(conversion %in% volc_units)){
    unit <- volc_units
  } else {
    unit <- 'NA'
  }

  file_info <- attr(res, 'info')

  dipsaus::lapply_async2(
    electrodes, function(e) {
      # Allow both ch1.mat and ch001.mat to pass
      regexp <- stringr::regex(sprintf('(^|[^0-9])[0]{0,}%d\\.(mat|h5)$', e), ignore_case = TRUE)
      cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
      for(b in blocks){
        info <- file_info[[b]]
        sel <- stringr::str_detect(info$files, regexp)
        src <- file.path(info$path, info$files[sel][[1]])
        dat <- read_mat(src, ram = FALSE)
        nm <- guess_raw_trace(dat, is_vector = TRUE)[[1]]
        s <- as.numeric(dat[[nm]])
        s[is.na(s)] <- 0
        # save to HDF5
        save_h5(x = s, file = cfile, name = sprintf('raw/%s', b),
                chunk = 1024, replace = TRUE, quiet = TRUE)
        save_h5(x = unit, file = cfile, name = sprintf('/units/%s', b),
                chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
      }
      invisible()
    }, callback = function(e) {
      sprintf("Importing %s/%s | electrode %s", project_name, subject_code, e)
    }, plan = FALSE
  )

  # progress <-
  #   dipsaus::progress2(
  #     catgl('Importing {project_name}/{subject_code}', .capture = TRUE),
  #     max = length(electrodes),
  #     shiny_auto_close = TRUE
  #   )
  #
  # lapply(electrodes, function(e){
  #   progress$inc(sprintf('Importing electrode %d', e))
  #   regexp <- stringr::regex(sprintf('(^|[^0-9])%d\\.(mat|h5)$', e), ignore_case = TRUE)
  #   cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
  #   for(b in blocks){
  #     info <- file_info[[b]]
  #     sel <- stringr::str_detect(info$files, regexp)
  #     src <- file.path(info$path, info$files[sel][[1]])
  #     dat <- read_mat(src, ram = FALSE)
  #     nm <- guess_raw_trace(dat, is_vector = TRUE)[[1]]
  #     s <- as.numeric(dat[[nm]])
  #     s[is.na(s)] <- 0
  #     # save to HDF5
  #     save_h5(x = s, file = cfile, name = sprintf('raw/%s', b),
  #             chunk = 1024, replace = TRUE, quiet = TRUE)
  #     save_h5(x = unit, file = cfile, name = sprintf('/units/%s', b),
  #             chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
  #   }
  #   invisible()
  # })

  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "native_matlab")
  pretools$save()

}


rave_import_lfp.native_matlab2 <- function(project_name, subject_code, blocks,
                                           electrodes, sample_rate, conversion = NA,
                                           add = FALSE, data_type = 'LFP', ...){
  .fs_struct <- raveio_getopt('file_structure')
  on.exit({
    raveio_setopt('file_structure', .fs_struct, .save = TRUE)
  }, add = TRUE)
  raveio_setopt('file_structure', 'native', .save = FALSE)
  # direct import data, no check (already checked)
  pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))
  if(!add){
    pretools$set_blocks(blocks = blocks)
  }
  pretools$set_electrodes(electrodes, type = data_type, add = add)
  pretools$set_sample_rates(sample_rate, type = data_type)
  pretools$subject$initialize_paths(include_freesurfer = FALSE)
  pretools$save()

  # Now import data
  res <- validate_raw_file_lfp.native_matlab2(
    subject_code = subject_code,
    blocks = blocks,
    electrodes = electrodes,
    check_content = FALSE,
    project_name = project_name
  )

  save_path <- file.path(pretools$subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)

  if(isTRUE(conversion %in% volc_units)){
    unit <- volc_units
  } else {
    unit <- 'NA'
  }

  progress <-
    dipsaus::progress2(
      catgl('Importing {project_name}/{subject_code}', .capture = TRUE),
      max = (length(electrodes) + 1) * length(blocks),
      shiny_auto_close = TRUE
    )
  file_info <- attr(res, 'info')
  for(b in blocks){
    info <- file_info[[b]]
    progress$inc(paste('Reading block', b))
    dat <- read_mat(file.path(info$path, info$files))
    nm <- guess_raw_trace(dat, electrodes = electrodes, is_vector = FALSE)[[1]]
    dat <- dat[[nm]]
    if(which.min(dim(dat)) == 1){
      dat <- t(dat)
    }
    lapply(electrodes, function(e){
      progress$inc(paste('Writing', b, '- electrode', e))
      cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
      s <- dat[,as.integer(e)]
      s[is.na(s)] <- 0
      save_h5(x = as.numeric(s), file = cfile, name = sprintf('raw/%s', b),
              chunk = 1024, replace = TRUE, quiet = TRUE)
      save_h5(x = unit, file = cfile, name = sprintf('/units/%s', b),
              chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
      invisible()
    })
  }

  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "native_matlab2")
  pretools$save()

}


rave_import_lfp.native_edf <- function(project_name, subject_code, blocks,
                                       electrodes, sample_rate, conversion = NA,
                                       add = FALSE, data_type = 'LFP', ...){
  .fs_struct <- raveio_getopt('file_structure')
  on.exit({
    raveio_setopt('file_structure', .fs_struct, .save = TRUE)
  }, add = TRUE)
  raveio_setopt('file_structure', 'native', .save = FALSE)
  # direct import data, no check (already checked)
  pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))
  if(!add){
    pretools$set_blocks(blocks = blocks)
  }
  pretools$set_electrodes(electrodes, type = data_type, add = add)
  pretools$set_sample_rates(sample_rate, type = data_type)
  pretools$subject$initialize_paths(include_freesurfer = FALSE)
  pretools$save()

  # Now import data
  res <- validate_raw_file_lfp.native_edf(
    subject_code = subject_code,
    blocks = blocks,
    electrodes = electrodes,
    check_content = FALSE,
    project_name = project_name
  )

  save_path <- file.path(pretools$subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)

  progress <-
    dipsaus::progress2(
      catgl('Importing {project_name}/{subject_code}', .capture = TRUE),
      max = length(blocks),
      shiny_auto_close = TRUE
    )
  file_info <- attr(res, 'info')

  ncores <- raveio_getopt("max_worker", 1)
  schedule_mat <- matrix(
    rep(NA, ceiling(length(electrodes) / ncores) * ncores),
    nrow = ncores
  )
  schedule_mat[seq_along(electrodes)] <- electrodes


  for(b in blocks){
    info <- file_info[[b]]
    progress$inc(paste('Processing block', b))
    edf_file <- file.path(info$path, info$files)

    dipsaus::lapply_async2(seq_len(ncores), function(margin) {
      sub_es <- schedule_mat[margin, ]
      sub_es <- sub_es[!is.na(sub_es)]
      dat <- read_edf_signal2(path = edf_file, signal_numbers = sub_es, convert_volt = conversion)
      lapply(sub_es, function(e){
        # progress$inc(paste('Writing', b, '- electrode', e))
        cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
        signal <- dat$get_signal(number = e)
        s <- as.numeric(signal$signal)
        s[is.na(s)] <- 0
        save_h5(x = as.vector(s), file = cfile, name = sprintf('raw/%s', b),
                chunk = 1024, replace = TRUE, quiet = TRUE)
        save_h5(x = signal$unit, file = cfile, name = sprintf('/units/%s', b),
                chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
        invisible()
      })
    }, plan = FALSE, callback = function(margin) {
      sprintf("Importing %s/%s | Block %s - chunk %s",
              project_name, subject_code, b, margin)
    })

    # dat <- read_edf_signal(path = edf_file, signal_numbers = electrodes, convert_volt = conversion)
    # lapply(electrodes, function(e){
    #   progress$inc(paste('Writing', b, '- electrode', e))
    #   cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
    #   signal <- dat$get_signal(number = e)
    #   s <- as.numeric(signal$signal)
    #   s[is.na(s)] <- 0
    #   save_h5(x = as.vector(s), file = cfile, name = sprintf('raw/%s', b),
    #           chunk = 1024, replace = TRUE, quiet = TRUE)
    #   save_h5(x = signal$unit, file = cfile, name = sprintf('/units/%s', b),
    #           chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
    #   invisible()
    # })
  }

  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "native_edf")
  pretools$save()
}

rave_import_lfp.native_brainvis <- function(project_name, subject_code, blocks,
                                            electrodes, sample_rate, conversion = NA,
                                            add = FALSE, data_type = 'LFP', ...){
  .fs_struct <- raveio_getopt('file_structure')
  on.exit({
    raveio_setopt('file_structure', .fs_struct, .save = TRUE)
  }, add = TRUE)
  raveio_setopt('file_structure', 'native', .save = FALSE)
  # direct import data, no check (already checked)
  pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))
  if(!add){
    pretools$set_blocks(blocks = blocks)
  }
  pretools$set_electrodes(electrodes, type = data_type, add = add)
  pretools$set_sample_rates(sample_rate, type = data_type)
  pretools$subject$initialize_paths(include_freesurfer = FALSE)
  pretools$save()

  # Now import data
  res <- validate_raw_file_lfp.native_brainvis(
    subject_code = subject_code,
    blocks = blocks,
    electrodes = electrodes,
    check_content = FALSE,
    project_name = project_name
  )

  save_path <- file.path(pretools$subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)

  progress <-
    dipsaus::progress2(
      catgl('Importing {project_name}/{subject_code}', .capture = TRUE),
      max = (length(electrodes) + 1) * length(blocks),
      shiny_auto_close = TRUE
    )
  file_info <- attr(res, 'info')
  for(b in blocks){
    info <- file_info[[b]]
    progress$inc(paste('Reading block', b))
    eeg_file <- file.path(info$path, info$files)
    header <- read_eeg_header(eeg_file)
    eeg_df <- file.path(info$path, header$common$DataFile)
    if(!file.exists(eeg_df)){
      # check eeg file
      df <- list.files(info$path, pattern = '\\.(eeg|dat)$', ignore.case = TRUE)
      if(length(df)){
        eeg_df <- file.path(info$path, df[[1]])
      }
    }
    dat <- read_eeg_data(header, eeg_df)
    # dim(dat$data)

    volc_units <- c('V', 'mV', 'uV')
    volc_factr <- c(1e6, 1e3, 1)

    lapply(electrodes, function(e){
      progress$inc(paste('Writing', b, '- electrode', e))
      cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
      s <- dat$data[e, ]
      s[is.na(s)] <- 0
      # TODO: convert unit
      unit <- header$channels$unit[[e]]
      if(
        is_physical_unit(conversion, volc_units) &&
        is_physical_unit(unit, volc_units)
      ) {
        # translate
        s <- s * volc_factr[volc_units == unit] / volc_factr[volc_units == conversion]
        unit <- conversion
      }
      save_h5(x = as.numeric(s), file = cfile, name = sprintf('raw/%s', b),
              chunk = 1024, replace = TRUE, quiet = TRUE)
      save_h5(x = unit, file = cfile, name = sprintf('/units/%s', b),
              chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
      invisible()
    })
  }

  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "native_brainvis")
  pretools$save()
}

rave_import_lfp.native_blackrock <- function(project_name, subject_code, blocks,
                                          electrodes, sample_rate, add = FALSE,
                                          conversion = NA, data_type = 'LFP', ...){

  # list2env(list(project_name = 'devel', subject_code = "PAV004",
  #               blocks = c("001", "002"),
  #               electrodes = "1-230,254-257", sample_rate = 2000, add = FALSE,
  #               conversion = NA, data_type = 'LFP'), envir=.GlobalEnv)

  .fs_struct <- raveio_getopt('file_structure')
  on.exit({
    raveio_setopt('file_structure', .fs_struct, .save = TRUE)
  }, add = TRUE)
  raveio_setopt('file_structure', 'native', .save = FALSE)
  # direct import data, no check (already checked)
  pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))

  if(!add){
    pretools$set_blocks(blocks = blocks)
  }
  electrodes <- dipsaus::parse_svec(electrodes)
  pretools$set_electrodes(electrodes, type = data_type, add = add)
  pretools$set_sample_rates(sample_rate, type = data_type)
  pretools$subject$initialize_paths(include_freesurfer = FALSE)
  pretools$save()

  # Now create instance, always check content as loading is cached
  res <- validate_raw_file_lfp.native_blackrock(
    subject_code = subject_code,
    blocks = blocks,
    electrodes = electrodes,
    check_content = TRUE,
    project_name = project_name
  )

  save_path <- file.path(pretools$subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)

  if(isTRUE(conversion %in% volc_units)){
    unit <- volc_units
  } else {
    unit <- 'uV'
  }
  factor <- c(1e-6, 1e-3, 1)[c("V", "mV", "uV") == unit]

  file_info <- attr(res, 'info')

  blackrock_files <- sapply(blocks, function(b) {
    brfile <- BlackrockFile$new(
      path = file.path(file_info[[b]]$path, file_info[[b]]$files[[1]]),
      block = b)

    # check sampling frequency
    actual_srates <- brfile$electrode_table$SampleRate[brfile$electrode_table$Electrode %in% electrodes]
    actual_srates <- unique(actual_srates)
    if(any(actual_srates < sample_rate)) {
      actual_srates <- actual_srates[actual_srates < sample_rate]
      stop(sprintf("Cannot import the data: the requested sample rate is %.0f Hz. However, some electrode channels have less sampling frequencies: %s", sample_rate, paste(actual_srates, collapse = ", ")))
    }

    brfile$refresh_data(verbose = FALSE)
    brfile
  }, simplify = FALSE, USE.NAMES = TRUE)

  dipsaus::lapply_async2(
    electrodes, function(e) {
      cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
      for(b in blocks){
        brfile <- blackrock_files[[b]]
        s <- brfile$get_electrode(e) * factor
        asrate <- attr(s, "meta")$SampleRate
        if(asrate > sample_rate) {
          # decimate
          decimate_rate <- asrate / sample_rate
          if(decimate_rate - round(decimate_rate) == 0) {
            s <- ravetools::decimate(s, decimate_rate)
          } else {
            # Cannot decimate...
            catgl(sprintf("Cannot decimate channel %d from %.1fHz to %.1fHz. Using nearest interpolation. This is highly discouraged. Please consider changing the sampling frequency.", e, asrate, sample_rate), level = "WARNING")
            tidx <- round(seq(1, length(s), by = decimate_rate))
            s <- s[tidx]
          }
        }
        # save to HDF5
        save_h5(x = s, file = cfile, name = sprintf('raw/%s', b),
                chunk = 1024, replace = TRUE, quiet = TRUE)
        save_h5(x = unit, file = cfile, name = sprintf('/units/%s', b),
                chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
      }
      invisible()
    }, callback = function(e) {
      sprintf("Importing %s/%s | electrode %s", project_name, subject_code, e)
    }, plan = FALSE
  )


  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "native_blackrock")
  pretools$save()

  # generate epoch files as well
  epoch <- lapply(blackrock_files, function(brfile) {
    tbl <- brfile$get_epoch()
    if(is.data.frame(tbl) && nrow(tbl)) {
      return(tbl)
    } else {
      return(NULL)
    }
  })
  epoch <- do.call('rbind', unname(epoch))
  if(is.data.frame(epoch) && nrow(epoch)) {
    epoch$Trial <- seq_len(nrow(epoch))
    # save epoch
    path <- file.path(pretools$subject$meta_path, "epoch_nev_exports.csv")
    safe_write_csv(x = epoch[, c("Block", "Time", "Trial", "Condition")],
                   file = path, row.names = FALSE)
  }

  invisible()

}


rave_import_lfp.bids_edf <- function(project_name, subject_code, blocks,
                                     electrodes, sample_rate, task_runs, conversion = NA,
                                     add = FALSE, data_type = 'LFP', ...){
  # direct import data, no check (already checked)
  pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))
  # file exists?
  if(!dir.exists(pretools$raw_path)){
    # locate in BIDS
    .fs_struct <- raveio_getopt('file_structure')
    on.exit({
      raveio_setopt('file_structure', .fs_struct, .save = TRUE)
    }, add = TRUE)
    raveio_setopt('file_structure', 'BIDS', .save = FALSE)

    pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))
    if(!dir.exists(pretools$raw_path)){
      stop('Cannot find subject folder int BIDS directory tree nor rave native raw path. Please make sure the subject files exist')
    }
  }

  # Now import data
  res <- validate_raw_file_lfp.bids_edf(
    subject_code = subject_code,
    blocks = blocks,
    electrodes = electrodes,
    check_content = FALSE,
    project_name = project_name
  )
  valid_run_names <- attr(res, "valid_run_names")
  invalid_runs <- task_runs[!task_runs %in% valid_run_names]
  if(length(invalid_runs)){
    stop('Invalid task_runs found, possible runs:\n', paste(' ', utils::head(valid_run_names), collapse = '\n'), '\n  ...')
  }

  # Do not check. block here does not mean session, should be session+task+run
  if(!add){
    pretools$set_blocks(blocks = task_runs, force = TRUE)
  }
  pretools$set_electrodes(electrodes, type = data_type, add = add)

  pretools$set_sample_rates(sample_rate, type = data_type)
  pretools$subject$initialize_paths(include_freesurfer = FALSE)
  pretools$save()



  save_path <- file.path(pretools$subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)

  progress <-
    dipsaus::progress2(
      catgl('Importing {project_name}/{subject_code}', .capture = TRUE),
      max = (length(electrodes) + 1) * length(task_runs),
      shiny_auto_close = TRUE
    )
  file_info <- attr(res, 'info')
  for(b in task_runs){
    info <- file_info[[b]]
    progress$inc(paste('Reading:', b))
    edf_file <- file.path(info$path, info$files)
    dat <- read_edf_signal(path = edf_file, signal_numbers = electrodes, convert_volt = conversion)
    lapply(electrodes, function(e){
      progress$inc(paste('Writing', b, '- electrode', e))
      cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
      signal <- dat$get_signal(number = e)
      s <- signal$signal
      s[is.na(s)] <- 0
      save_h5(x = as.numeric(s), file = cfile, name = sprintf('raw/%s', b),
              chunk = 1024, replace = TRUE, quiet = TRUE)
      save_h5(x = signal$unit, file = cfile, name = sprintf('/units/%s', b),
              chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
      invisible()
    })
  }

  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "bids_edf")
  pretools$save()
}



rave_import_lfp.bids_brainvis <- function(project_name, subject_code, blocks,
                                          electrodes, sample_rate, task_runs, conversion = NA,
                                          add = FALSE, data_type = 'LFP', ...){
  # direct import data, no check (already checked)
  pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))
  # file exists?
  if(!dir.exists(pretools$raw_path)){
    # locate in BIDS
    .fs_struct <- raveio_getopt('file_structure')
    on.exit({
      raveio_setopt('file_structure', .fs_struct, .save = TRUE)
    }, add = TRUE)
    raveio_setopt('file_structure', 'BIDS', .save = FALSE)

    pretools <- RAVEPreprocessSettings$new(subject = sprintf('%s/%s', project_name, subject_code))
    if(!dir.exists(pretools$raw_path)){
      stop('Cannot find subject folder int BIDS directory tree nor rave native raw path. Please make sure the subject files exist')
    }
  }
  blocks <- sprintf('ses-%s', stringr::str_remove(blocks, 'ses-'))

  # Now import data
  res <- validate_raw_file_lfp.bids_brainvis(
    subject_code = subject_code,
    blocks = blocks,
    electrodes = electrodes,
    check_content = FALSE,
    project_name = project_name
  )
  valid_run_names <- attr(res, "valid_run_names")
  invalid_runs <- task_runs[!task_runs %in% valid_run_names]
  if(length(invalid_runs)){
    stop('Invalid task_runs found, possible runs:\n', paste(' ', utils::head(valid_run_names), collapse = '\n'), '\n  ...')
  }

  # Do not check. block here does not mean session, should be session+task+run
  if(!add){
    pretools$set_blocks(blocks = task_runs, force = TRUE)
  }
  pretools$set_electrodes(electrodes, type = data_type, add = add)
  pretools$set_sample_rates(sample_rate, type = data_type)
  pretools$subject$initialize_paths(include_freesurfer = FALSE)
  pretools$save()

  eeg_header <- local({
    raw_root <- raveio_getopt('bids_data_dir')
    if(is.na(raw_root) || !dir.exists(raw_root)){
      raw_root <- raveio_getopt('raw_data_dir')
    }
    load_bids_ieeg_header(raw_root, project_name, subject_code)
  })


  save_path <- file.path(pretools$subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)

  progress <-
    dipsaus::progress2(
      catgl('Importing {project_name}/{subject_code}', .capture = TRUE),
      max = (length(electrodes) + 1) * length(task_runs),
      shiny_auto_close = TRUE
    )
  file_info <- attr(res, 'info')
  for(b in task_runs){
    info <- file_info[[b]]
    progress$inc(paste('Reading:', b))
    eeg_file <- file.path(info$path, info$files)
    header <- read_eeg_header(eeg_file)
    eeg_df <- file.path(info$path, header$common$DataFile)
    if(!file.exists(eeg_df)){
      # eeg file might be changed to BIDS file name
      ext <- stringr::str_extract(header$common$DataFile, '\\.[^.]+$')
      eeg_df <- stringr::str_replace(eeg_file, '\\.[^.]+$', ext)
    }
    dat <- read_eeg_data(header, eeg_df)
    volc_units <- c('V', 'mV', 'uV')
    volc_factr <- c(1e6, 1e3, 1)

    sess_name <- stringr::str_extract(b, '^[^-_]+')
    channel_table <- eeg_header$sessions[[sess_name]]$tasks[[b]]$channels
    snames <- eeg_header$sessions[[sess_name]]$space_names
    if(length(snames)){
      electrode_table <- eeg_header$sessions[[sess_name]]$spaces[[snames[[1]]]]$table
      electrode_names <- electrode_table$name[electrodes]
      channel_idx <- sapply(electrode_names, function(ename){
        which(channel_table$name == ename)
      })
    } else {
      channel_idx <- electrodes
    }

    lapply(seq_along(electrodes), function(ii){
      e <- electrodes[[ii]]
      chidx <- channel_idx[[ii]]
      progress$inc(paste('Writing', b, '- electrode', e))
      cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
      s <- dat$data[chidx, ]
      s[is.na(s)] <- 0

      unit <- header$channels$unit[[chidx]]
      if(
        is_physical_unit(conversion, volc_units) &&
        is_physical_unit(unit, volc_units)
      ) {
        # translate
        s <- s * volc_factr[volc_units == unit] / volc_factr[volc_units == conversion]
        unit <- conversion
      }

      save_h5(x = as.numeric(s), file = cfile, name = sprintf('raw/%s', b),
              chunk = 1024, replace = TRUE, quiet = TRUE)
      save_h5(x = unit, file = cfile, name = sprintf('/units/%s', b),
              chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
      invisible()
    })
  }

  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "bids_brainvis")
  pretools$save()
}







#' Import data into 'rave' projects
#' @description Import files with predefined structures. Supported file
#' formats include 'Matlab', 'HDF5', 'EDF(+)', 'BrainVision'
#' (\code{'.eeg/.dat/.vhdr'}). Supported file structures include 'rave' native
#' structure and 'BIDS' (very limited) format. Please see
#' \url{https://openwetware.org/wiki/RAVE:ravepreprocess} for tutorials.
#' @param project_name project name, for 'rave' native structure, this can be
#' any character; for 'BIDS' format, this must be consistent with 'BIDS'
#' project name. For subjects with multiple tasks, see Section "'RAVE' Project"
#' @param subject_code subject code in character. For 'rave' native structure,
#' this is a folder name under raw directory. For 'BIDS', this is subject
#' label without \code{"sub-"} prefix
#' @param blocks characters, for 'rave' native format, this is the folder names
#' subject directory; for 'BIDS', this is session name with \code{"ses-"}.
#' Section "Block vs. Session" for different meaning of "blocks" in 'rave'
#' and 'BIDS'
#' @param electrodes integers electrode numbers
#' @param format integer from 1 to 6, or character. For characters, you can get
#' options by running \code{names(IMPORT_FORMATS)}
#' @param data_type electrode signal type; see \code{\link{SIGNAL_TYPES}}
#' @param sample_rate sample frequency, must be positive
#' @param conversion physical unit conversion, choices are \code{NA},
#' \code{V}, \code{mV}, \code{uV}
#' @param task_runs for 'BIDS' formats only, see Section "Block vs. Session"
#' @param ... other parameters
#' @param add whether to add electrodes. If set to true, then only new
#' electrodes are allowed to be imported, blocks will be ignored and trying to
#' import electrodes that have been imported will still result in error.
#' @return None
#' @section 'RAVE' Project:
#' A 'rave' project can be very flexible. A project can refer to a task, a
#' research objective, or "arbitrarily" as long as you find common research
#' interests among subjects. One subject can appear in multiple projects with
#' different blocks, hence \code{project_name} should be
#' objective-based. There is no concept of "project" in 'rave' raw directory.
#' When importing data, you choose subset of blocks from subjects forming
#' a project.
#'
#' When importing 'BIDS' data into 'rave', \code{project_name} must be
#' consistent with 'BIDS' project name as a compromise. Once imported,
#' you may change the project folder name in imported rave data
#' directory to other names. Because once raw traces are imported,
#' 'rave' data will become self-contained
#' and 'BIDS' data are no longer required for analysis.
#' This naming inconsistency will also be ignored.
#'
#'
#' @section Block vs. Session:
#' 'rave' and 'BIDS' have different definitions for a "chunk" of signals.
#' In 'rave', we use "block". it means
#' combination of session (days), task, and run, i.e. a block of continuous
#' signals captured. Raw data files are supposed to be stored in file
#' hierarchy of \code{<raw-root>/<subject_code>/<block>/<datafiles>}.
#' In 'BIDS', sessions, tasks, and runs are separated, and only session names
#' are indicated under subject folder. Because some previous compatibility
#' issues, argument \code{'block'} refers to direct folder names under
#' subject directories.
#' This means when importing data from 'BIDS' format, \code{block} argument
#' needs to be session names to comply with \code{'subject/block'} structure,
#' and there is an additional mandatory argument \code{task_runs}
#' especially designed for 'BIDS' format.
#'
#' For 'rave' native raw data format, \code{block} will be as-is once imported.
#' \cr
#' For 'BIDS' format, \code{task_runs} will be treated as blocks once imported.
#'
#' @section File Formats:
#' Following file structure. Here use project \code{"demo"} and subject
#' \code{"YAB"} and block \code{"008")}, electrode \code{14} as an example.
#' \describe{
#' \item{\code{format=1}, or \code{".mat/.h5 file per electrode per block"}}{
#' folder \code{<raw>/YAB/008} contains 'Matlab' or 'HDF5' files per electrode.
#' Data file name should look like \code{"xxx_14.mat"}}
#' \item{\code{format=2}, or \code{"Single .mat/.h5 file per block"}}{
#' \code{<raw>/YAB/008} contains only one 'Matlab' or 'HDF5' file. Data within
#' the file should be a 2-dimensional matrix, where the column 14 is
#' signal recorded from electrode 14}
#' \item{\code{format=3}, or \code{"Single EDF(+) file per block"}}{
#' \code{<raw>/YAB/008} contains only one \code{'edf'} file}
#' \item{\code{format=4}, or \code{
#' "Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block"}}{
#' \code{<raw>/YAB/008} contains only one \code{'vhdr'} file, and
#' the data file must be inferred from the header file
#' }
#' \item{\code{format=5}, or \code{"BIDS & EDF(+)"}}{
#' \code{<bids>/demo/sub-YAB/ses-008/} must contains \code{*_electrodes.tsv},
#' each run must have channel file. The channel files and electrode file
#' must be consistent in names.
#' \cr
#' Argument \code{task_runs} is mandatory, characters, combination of session,
#' task name, and run number. For example, a task header file in BIDS with name
#' \code{'sub-YAB_ses-008_task-visual_run-01_ieeg.edf'} has \code{task_runs}
#' name as \code{'008-visual-01'}, where the first \code{'008'} refers
#' to session, \code{'visual'} is task name, and the second \code{'01'} is
#' run number.
#' }
#' \item{\code{format=6}, or \code{
#' "BIDS & BrainVision (.vhdr+.eeg, .vhdr+.dat)"}}{
#' Same as previous format \code{"BIDS & EDF(+)"}, but data files have
#' 'BrainVision' formats.
#' }
#' }
#'
#'
#' @export
rave_import <- function(project_name, subject_code, blocks, electrodes, format,
                        sample_rate, conversion = NA, data_type = 'LFP',
                        task_runs = NULL, add = FALSE, ...){

  stopifnot2(isTRUE(data_type %in% SIGNAL_TYPES), msg = paste(
    "Unsupported electrode signal type:", data_type
  ))

  switch (
    data_type,

    # Continuous iEEG/EEG signals, will be wavelet and perform spectral analysis
    'LFP' = {
      generic_name <- IMPORT_FORMATS[[format]]

      if(!is_valid_ish(generic_name, max_len = 1L, mode = 'character', blank = TRUE)){
        stop('rave_import: format ', sQuote(format), ' must be integer from 1-',
             length(IMPORT_FORMATS), ' or the following characters:\n',
             paste0(seq_along(IMPORT_FORMATS), ': ', sQuote(names(IMPORT_FORMATS)),
                    collapse = ',\n'))
      }

      if(generic_name %in% unlist(IMPORT_FORMATS[c(5,6)])){
        # BIDS format, blocks must be ses-xxx
        blocks <- stringr::str_remove(blocks, '^ses-')
        blocks <- sprintf('ses-%s', blocks)
        if(!length(task_runs)){
          stop('rave_import: BIDS format must specify task_runs as session+task+run')
        }
      }

      rave_import_lfp(
        project_name = structure(project_name, class = generic_name),
        subject_code = subject_code, blocks = blocks, electrodes = electrodes,
        sample_rate = sample_rate, conversion = conversion, task_runs = task_runs,
        add = add, data_type = data_type,
        ...
      )
    },
    {
      stop("Electrode with signal type: ", data_type, " has not been implemented yet")
    }
  )

}

