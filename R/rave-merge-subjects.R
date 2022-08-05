
merge_rave_subject <- function(from_subjects, to_subject, ignore_duplicated_blocks = TRUE) {

  # Will merge all subject and create a new subject. The original data from new subject
  # will be overwritten
  # from_subjects <- c('demo/KC', 'demo/KC')
  # to_subject <- 'test4/KC'
  # ignore_duplicated_blocks <- FALSE


  if(length(from_subjects) < 1) {
    stop("`merge_rave_subject`: No subject to merge or migrate")
  }

  to_subject <- as_rave_subject(to_subject, strict = FALSE)

  from_subjects <- lapply(from_subjects, function(from_subject) {
    from_subject <- as_rave_subject(from_subject, strict = FALSE)
    from_valid <- validate_subject(from_subject, method = "basic", verbose = FALSE, version = "2")

    if(!isTRUE(from_valid$preprocess$has_wavelet$valid)) {
      stop(sprintf("merge_rave_subject: please apply wavelet tranforms to all subjects. Please make sure the subject [%s] has finished this preprocessing step", from_subject$subject_id))
    }
    from_subject
  })

  sample_subject <- from_subjects[[1]]
  o <- order(sample_subject$electrodes)
  electrodes <- sample_subject$electrodes[o]

  if(!length(electrodes)) {
    stop(sprintf("Subject [%s] has no electrodes", sample_subject$subject_id))
  }

  electrode_types <- sample_subject$electrode_types[o]
  sample_rates <- sample_subject$raw_sample_rates[o]
  notch_params <- sample_subject$preprocess_settings$notch_params
  wavelet_params <- sample_subject$preprocess_settings$wavelet_params

  block_list <- dipsaus::fastmap2()

  lapply(from_subjects, function(from_subject) {

    if(!setequal(from_subject$electrodes, electrodes)) {
      stop(sprintf("Subject [%s] has different electrode set (%s) vs. subject [%s] (%s)",
                   from_subject$subject_id, dipsaus::deparse_svec(from_subject$electrodes),
                   sample_subject$subject_id, dipsaus::deparse_svec(electrodes)))
    }

    o <- order(from_subject$electrodes)
    etypes <- from_subject$electrode_types[o]
    sel <- etypes != electrode_types
    if(any(sel)) {
      s <- sprintf("  - Electrode %.0f: %s vs. %s\n",
                   electrodes[sel], etypes[sel], electrode_types[sel])
      stop(sprintf(
        "Subjects have inconsistent electrode types: (%s vs %s)\n",
        from_subject$subject_id, sample_subject$subject_id
      ), s)
    }

    if(any(from_subject$raw_sample_rates[o] != sample_rates)) {
      stop(sprintf(
        "The raw sampling frequency is inconsistent between [%s] and [%s]",
        from_subject$subject_id, sample_subject$subject_id
      ))
    }

    wparams <- from_subject$preprocess_settings$wavelet_params

    if(wparams$downsample_to != wavelet_params$downsample_to) {
      stop(sprintf(
        "In wavelet transform, the subject [%s] down-sampled power to [%.1f Hz] while [%s] has [%.1f Hz]",
        from_subject$subject_id, wparams$downsample_to,
        sample_subject$subject_id, wavelet_params$downsample_to
      ))
    }

    if(
      length(wparams$frequencies) != length(wavelet_params$frequencies) ||
      !all(wparams$frequencies == wavelet_params$frequencies)
    ) {
      stop(sprintf(
        "In wavelet transform, the subject [%s] has frequencies [%s] while [%s] has [%s]",
        from_subject$subject_id, dipsaus::deparse_svec(wparams$frequencies),
        sample_subject$subject_id, dipsaus::deparse_svec(wavelet_params$frequencies)
      ))
    }

    for(b in from_subject$blocks) {
      item <- list(
        subject = from_subject,
        from_block = b
      )
      if(startsWith(b, "0")) {
        b <- sprintf("block%s", b)
      }
      if(!block_list[["@has"]](b)) {
        block_list[[b]] <- item
      } else if (!ignore_duplicated_blocks) {
        b <- sprintf("%s_dup%s", b, rand_string(4))
        block_list[[b]] <- item
      }
    }

  })


  # 1. initialize `to_subject`, make sure directories are ready
  to_subject$initialize_paths(include_freesurfer = TRUE)

  # 2. set preprocessing data and unlock subject if locked
  preproc <- to_subject$preprocess_settings
  preproc$data$checklevel <- 0L

  existing <- preproc$electrodes
  existing <- existing[!existing %in% electrodes]
  preproc$data$electrodes <- NULL
  preproc$data$`@remove`(as.character(electrodes))

  blocks <- names(block_list)
  if(!setequal(preproc$blocks, blocks)) {
    lapply(existing, function(e){
      preproc$data[[as.character(e)]]$data_imported <- FALSE
    })
    preproc$set_blocks(blocks, force = TRUE)
  }
  etypes <- unique(electrode_types)
  for(etype in etypes) {
    sel <- electrode_types %in% etype
    preproc$set_electrodes(electrodes[sel], type = etype, add = TRUE)
    srate <- sample_rates[sel]
    preproc$set_sample_rates(srate[[1]], type = etype)
  }

  preproc$save()


  # 2. "import" raw data
  physical_unit <- NA
  # format <- 7L

  dipsaus::lapply_async2(electrodes, function(e) {

    for(b in names(block_list)) {

      item <- block_list[[b]]
      subject <- item$subject
      origin_block <- item$from_block

      # 1. rave/preprocess/voltage/electrode_x.h5
      src_path <- file.path(subject$preprocess_path, "voltage", sprintf("electrode_%d.h5", e))
      dst_path <- file.path(to_subject$preprocess_path, "voltage", sprintf("electrode_%d.h5", e))
      dir_create2(dirname(dst_path))

      src_h5name <- sprintf("raw/%s", origin_block)
      dst_h5name <- sprintf("raw/%s", b)
      s <- load_h5(file = src_path, name = src_h5name, ram = TRUE, quiet = TRUE)
      save_h5(x = s, file = dst_path, name = dst_h5name, quiet = TRUE)

      src_h5name <- sprintf("notch/%s", origin_block)
      dst_h5name <- sprintf("notch/%s", b)
      s <- load_h5(file = src_path, name = src_h5name, ram = TRUE, quiet = TRUE)
      save_h5(x = s, file = dst_path, name = dst_h5name, quiet = TRUE)

      # 2. rave/data/xxx/x.h5
      for(dtype in c("power", "phase", "voltage")) {
        src_path <- file.path(subject$data_path, dtype, sprintf("%d.h5", e))
        dst_path <- file.path(to_subject$data_path, dtype, sprintf("%d.h5", e))
        dir_create2(dirname(dst_path))

        src_h5name <- sprintf("raw/%s/%s", dtype, origin_block)
        dst_h5name <- sprintf("raw/%s/%s", dtype, b)
        s <- load_h5(file = src_path, name = src_h5name, ram = TRUE, quiet = TRUE)
        save_h5(x = s, file = dst_path, name = dst_h5name, quiet = TRUE)

        save_h5(x = "invalid", file = dst_path, name = 'reference', quiet = TRUE, ctype = 'character')
      }
    }

  }, plan = FALSE, callback = function(e) {
    sprintf("Merging subjects|electrode %d", e)
  })

  # 3. migrate notch filter and wavelet parameters
  # notch filter
  preproc$data$notch_params <- notch_params
  apply_notch <- list(
    electrodes = electrodes,
    notch_filter_lowerbound = notch_params$frequencies - notch_params$half_bandwidths / 2,
    notch_filter_upperbound = notch_params$frequencies + notch_params$half_bandwidths / 2,
    timestamp = strftime(Sys.time(), usetz = TRUE)
  )
  to_subject$set_default(
    namespace = "notch_filter",
    key = "parameters",
    value = apply_notch
  )

  # wavelet
  wavelet_params$timestamp <- strftime(Sys.time(), usetz = TRUE)
  wavelet_logs <- list(wavelet_params)
  preproc$data$wavelet_logs <- wavelet_logs
  preproc$data$wavelet_params <- wavelet_params

  to_subject$set_default(
    namespace = "wavelet_module",
    key = "parameters",
    wavelet_params
  )

  for(e in electrodes) {
    preproc$data[[as.character(e)]]$data_imported <- TRUE
    preproc$data[[as.character(e)]]$notch_filtered <- TRUE
    preproc$data[[as.character(e)]]$has_wavelet <- TRUE
  }
  preproc$data$preprocess_version <- preproc$current_version
  preproc$save()

  # Step 3: copy meta data
  # generate frequencies.csv
  utils::write.csv(
    file = file.path(to_subject$meta_path, "frequencies.csv"),
    row.names = FALSE,
    data.frame(
      Frequency = wavelet_params$frequencies,
      Cycle = wavelet_params$cycle,
      Method = "Wavelet"
    )
  )
  # also remove the meta/time_points.csv
  tpfile <- file.path(to_subject$meta_path, "time_points.csv")
  if(file.exists(tpfile)) { unlink(tpfile) }

  # migrate reference files
  utils::write.csv(
    data.frame(
      Electrode = to_subject$electrodes,
      Group = "default",
      Reference = "noref",
      Type = "No Reference"
    ),
    file.path(to_subject$meta_path, "reference_noref.csv")
  )

  # merge epoch_nev_export.csv
  epoch_tbl <- lapply(names(block_list), function(b) {
    item <- block_list[[b]]
    epoch <- item$subject$meta_data(meta_type = 'epoch', meta_name = 'nev_exports')
    if(!is.data.frame(epoch)) { return() }
    tryCatch({
      epoch <- epoch[, c("Block", "Time", "Trial", "Condition")]
      sel <- epoch$Block %in% item$from_block
      if(!any(sel)) {
        return(NULL)
      }
      epoch <- epoch[sel, ]
      epoch$Block <- b
      return(epoch)
    }, error = function(e) {
      NULL
    })
  })
  epoch_tbl <- dipsaus::drop_nulls(epoch_tbl)
  if(length(epoch_tbl)) {

    epoch_tbl <- do.call("rbind", unname(epoch_tbl))
    if(nrow(epoch_tbl)) {
      epoch_tbl$Trial <- seq_along(nrow(epoch_tbl))
      safe_write_csv(
        x = epoch_tbl,
        file = file.path(to_subject$meta_path, "epoch_nev_exports.csv"),
        row.names = FALSE
      )
    }


  }

  # get electrode files
  etable <- NULL
  for(from_subject in from_subjects) {
    tbl <- from_subject$get_electrode_table(
      reference_name = ".fake")
    sel <- tbl$Electrode %in% electrodes
    if(sum(sel) == length(electrodes)) {
      tbl <- tbl[sel, ]
      if(is.null(etable)) {
        etable <- tbl
      } else if(all(etable$Coord_x == 0)) {
        etable <- tbl
      } else if(ncol(etable) < ncol(tbl)){
        etable <- tbl
      }
    }
  }
  if(!is.data.frame(etable)) {
    etable <- data.frame(
      Electrode = to_subject$electrodes,
      Coord_x = 0,
      Coord_y = 0,
      Coord_z = 0,
      Label = sprintf("NoLabel%s", seq_along(to_subject$electrodes)),
      LocationType = "iEEG"
    )
  }
  etable$SignalType <- to_subject$electrode_types

  save_meta2(data = etable, meta_type = "electrodes", project_name = to_subject$project_name, subject_code = to_subject$subject_code)

  # clear cache
  clear_cached_files(
    subject_code = to_subject$subject_code,
    quiet = TRUE
  )

  # backward compatible
  rave_subject_format_conversion(subject = to_subject)

  clear_cached_files(
    subject_code = to_subject$subject_code
  )

  catgl("Done merging subjects. PLEASE re-generate epoch and reference files", level = "INFO")
  catgl("Done merging subjects. PLEASE re-generate epoch and reference files", level = "DEFAULT")


}
