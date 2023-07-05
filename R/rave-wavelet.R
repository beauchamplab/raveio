
run_wavelet <- function(
    subject, electrodes, freqs, cycles,
    target_sample_rate = 100,
    kernels_precision = "float", pre_downsample = 1,
    verbose = TRUE
) {
  # DIPSAUS DEBUG START
  # list2env(
  #   list(
  #     subject = "YAEL/PAV020",
  #     electrodes = c(1, 2, 3, 4, 5, 6,
  #                    7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
  #     freqs = c(2L,
  #               22L, 42L, 62L, 82L, 102L, 122L, 142L, 162L, 182L),
  #     cycles = c(3,
  #                8, 11, 12, 14, 15, 16, 17, 18, 19),
  #     target_sample_rate = 100,
  #     kernels_precision = "float",
  #     pre_downsample = 4,
  #     verbose = TRUE
  #   ),
  #   envir = globalenv()
  # )

  subject <- restore_subject_instance(subject, strict = FALSE)

  # clear subject's cached files
  clear_cached_files(
    subject_code = subject$subject_code
  )

  blocks <- subject$preprocess_settings$blocks
  srates <- subject$preprocess_settings$sample_rates
  srates <- sapply(electrodes, function(e){
    re <- srates[subject$electrodes == e]
    if(!length(re)) {
      stop("Electrode ", e, " does not have sample rate. The data might not be imported correctly and some configurations are missing.")
    }
    re[[1]]
  })
  compress_rates <- srates / target_sample_rate
  pre_decimate <- as.integer(pre_downsample)
  if(is.na(pre_decimate) || pre_decimate < 1) {
    pre_decimate <- 1
  }

  overall_progress <- dipsaus::progress2(
    title = "Wavelet overall progress",
    max = 6,
    shiny_auto_close = TRUE,
    quiet = !verbose
  )

  # Create dir
  overall_progress$inc("Creating directories")
  subject$initialize_paths(include_freesurfer = FALSE)
  dir_create2(file.path(subject$data_path, "power"))
  dir_create2(file.path(subject$data_path, "phase"))
  dir_create2(file.path(subject$data_path, "voltage"))

  # set up ravetools temporary path
  tpath <- dir_create2(
    file.path(cache_root(), "ravetools", rand_string(length = 10))
  )
  ravetools_tdir_opt <- getOption("ravetools.tempdir", default = NULL)
  ravetools_tdir_env <- Sys.getenv("RAVETOOLS_TEMPDIR", unset = "")
  options("ravetools.tempdir" = tpath)
  Sys.setenv("RAVETOOLS_TEMPDIR" = tpath)

  on.exit({
    options("ravetools.tempdir" = ravetools_tdir_opt)
    if(identical(ravetools_tdir_env, "")) {
      Sys.unsetenv("RAVETOOLS_TEMPDIR")
    } else {
      Sys.setenv("RAVETOOLS_TEMPDIR" = ravetools_tdir_env)
    }
    unlink(tpath, recursive = TRUE)
  }, add = TRUE, after = FALSE)

  # prepare kernels
  overall_progress$inc("Generating wavelet kernels")
  sample_file <- file.path(subject$preprocess_path, 'voltage',
                          sprintf('electrode_%d.h5', electrodes[[1]]))
  if(!file.exists(sample_file) || !h5_valid(sample_file)){
    stop("Electrode file is missing (preprocess, electrode ", electrodes[[1]], ")")
  }
  sample_names <- gsub("^/", "", h5_names(sample_file))



  ravetools <- asNamespace("ravetools")

  generate_kernel <- ravetools[[sprintf("wavelet_kernels2_%s", kernels_precision)]]

  lapply(unique(srates), function(srate) {
    lapply(blocks, function(block){
      sample_name <- sprintf("notch/%s", block)
      if(!sample_name %in% sample_names) {
        stop(sprintf("I can find the imported signal file for Electrode %s, but cannot find any notch-filtered signal for block %s. The data file might be corrupted.", electrodes[[1]], block))
      }
      sample_data <- load_h5(sample_file, name = sample_name, ram = FALSE, read_only = TRUE)
      data_length <- length(sample_data)

      if(data_length <= 0) {
        stop(sprintf("Electrode %s has zero-length signal (/notch/%s). The data file might be corrupted.", electrodes[[1]], block))
      }

      if(pre_downsample > 1) {
        sample_data <- ravetools::decimate(
          sample_data[], pre_downsample, ftype = "fir")
        data_length <- length(sample_data)
      }

      generate_kernel(freqs = freqs, srate = srate / pre_decimate, wave_num = cycles, data_length = data_length, signature = subject$subject_id)
    })
  })

  # 2. raw channel files and power/phase files
  overall_progress$inc("Removing previously generated wavelet coefficients")
  preproc <- RAVEPreprocessSettings$new(subject = subject$subject_id, read_only = TRUE)

  for(e in electrodes) {
    preproc$data[[as.character(e)]]$has_wavelet <- FALSE
  }
  preproc$save()

  data_root <- subject$data_path
  lapply(electrodes, function(e){
    unlink(file.path(data_root, 'power', sprintf('%d.h5', e)))
    unlink(file.path(data_root, 'phase', sprintf('%d.h5', e)))
    unlink(file.path(data_root, 'voltage', sprintf('%d.h5', e)))
    for(block in blocks){
      unlink(file.path(data_root, 'cache', 'power', 'raw', block, sprintf('%d.fst', e)))
      unlink(file.path(data_root, 'cache', 'phase', 'raw', block, sprintf('%d.fst', e)))
    }
  })



  # load signals
  overall_progress$inc("Applying wavelet (a.k.a. the long step)")
  preprocess_dir <- subject$preprocess_path
  lapply_async(
    seq_along(electrodes), function(ii){

      e <- electrodes[[ii]]
      srate <- srates[[ii]]
      compress_rate <- compress_rates[[ii]]

      if(pre_decimate > 1) {
        compress_rate <- compress_rates[[ii]] / pre_decimate
        srate <- srate / pre_decimate
      }

      for(block in blocks){

        sorig <- load_h5(
          file = file.path(preprocess_dir, 'voltage', sprintf('electrode_%d.h5', e)),
          name = sprintf('/notch/%s', block),
          ram = TRUE
        )

        if(pre_decimate > 1) {
          s <- ravetools::decimate(sorig, pre_decimate,
                                   ftype = "fir")
        } else {
          s <- sorig
        }
        data_length <- length(s)

        re <- ravetools::morlet_wavelet(
          data = s,
          freqs = freqs,
          srate = srate,
          wave_num = cycles,
          precision = kernels_precision,
          signature = subject$subject_id
        )

        # Subset coefficients to save space
        ind <- floor(seq(1, data_length, by = compress_rate))

        if(kernels_precision == "float"){
          # Load all at once and subset is faster, but one signal is around 1-2GB, so...
          coef <- t(re[ind,,drop = FALSE])

          phase <- Arg(coef)
          power <- Mod(coef)^2

          re$.mode <- "readwrite"
          re$delete()
        } else {
          coef <- t(re$real[ind, , drop = FALSE] + 1i * re$imag[ind, , drop = FALSE])

          phase <- Arg(coef)
          power <- Mod(coef)^2
          re$real$.mode <- "readwrite"
          re$real$delete()
          re$imag$.mode <- "readwrite"
          re$imag$delete()
        }

        # Save power, phase, voltage
        fname <- sprintf('%d.h5', e)
        wavelet_h5chunk <- c(length(freqs), 128)

        # power
        save_h5(
          x = power,
          file = file.path(data_root, "power", fname),
          name = sprintf('/raw/power/%s', block),
          chunk = wavelet_h5chunk,
          replace = TRUE
        )
        # save_h5(
        #   x = 'noref',
        #   file = file.path(data_root, "power", fname),
        #   name = '/reference',
        #   chunk = 1,
        #   replace = TRUE, size = 1000
        # )

        # phase
        save_h5(
          x = phase,
          file = file.path(data_root, "phase", fname),
          name = sprintf('/raw/phase/%s', block),
          chunk = wavelet_h5chunk,
          replace = TRUE
        )

        # voltage
        save_h5(
          x = sorig,
          file = file.path(data_root, "voltage", fname),
          name = sprintf('/raw/voltage/%s', block),
          chunk = 1024,
          replace = TRUE
        )
      }
    },
    callback = function(ii){
      sprintf(sprintf("Applying wavelet|Electrode - %s", electrodes[[ii]]))
    }
  )

  # reload preprocess settings in case some changes are not captured
  preproc <- RAVEPreprocessSettings$new(subject = subject$subject_id, read_only = TRUE)


  overall_progress$inc("Saving configurations and update log files")

  for(e in electrodes) {
    preproc$data[[as.character(e)]]$notch_filtered <- TRUE
    preproc$data[[as.character(e)]]$has_wavelet <- TRUE
  }


  wavelet_params <- list(
    channels = electrodes,
    electrodes = electrodes,
    downsample_to = target_sample_rate,
    target_srate = target_sample_rate,
    frequencies = freqs,
    wave_num = cycles,
    cycle = cycles,
    precision = kernels_precision,
    pre_downsample = pre_downsample
  )

  wavelet_logs <- as.list(preproc$data$wavelet_logs)
  wavelet_logs[[length(wavelet_logs) + 1]] <- wavelet_params
  preproc$data$wavelet_logs <- wavelet_logs

  wavelet_params <- wavelet_params[c(
    "electrodes", "downsample_to", "frequencies",
    "cycle", "precision", "pre_downsample"
  )]
  wavelet_params$timestamp <- strftime(Sys.time(), usetz = TRUE)
  preproc$data$wavelet_params <- wavelet_params
  preproc$save()

  subject$set_default(
    namespace = "wavelet_module",
    key = "parameters",
    wavelet_params
  )
  # generate reference table
  safe_write_csv(
    file = file.path(subject$meta_path, "reference_noref.csv"),
    row.names = FALSE,
    data.frame(
      Electrode = subject$electrodes,
      Group = "Default",
      Reference = "noref",
      Type = "No Reference"
    )
  )
  # generate frequencies.csv
  utils::write.csv(
    file = file.path(subject$meta_path, "frequencies.csv"),
    row.names = FALSE,
    data.frame(
      Frequency = freqs,
      Cycle = cycles,
      Method = "Wavelet"
    )
  )
  # Finalizing: clear cache
  clear_cached_files(
    subject_code = subject$subject_code
  )

  # also remove the meta/time_points.csv
  tpfile <- file.path(subject$meta_path, "time_points.csv")
  if(file.exists(tpfile)) {
    unlink(tpfile)
  }

  overall_progress$inc("Make sure reference files are up-to-date")

  subject <- restore_subject_instance(subject$subject_id)
  # check subject references
  refs <- unlist(lapply(subject$reference_names, function(refname) {
    tryCatch({
      refs <- unique(subject$get_reference(refname, simplify = TRUE))
      refs <- refs[startsWith(refs, "ref_")]
      if(length(refs)) {
        refs <- refs[vapply(refs, function(ref) {
          isTRUE(length(dipsaus::parse_svec(gsub("^ref_", "", ref))) > 1)
        }, FALSE)]
      }
      return(refs)
    }, error = function(e) { NULL })
  }))
  refs <- unique(refs)

  if(length(refs)) {
    lapply_async(refs, function(ref, subject_id) {
      try({
        ref_electrodes <- dipsaus::parse_svec(gsub("^ref_", "", ref))
        generate_reference(subject = subject_id, electrodes = ref_electrodes)
      })
      return()
    }, FUN.args = list(subject_id = subject$subject_id), callback = function(ref) {
      sprintf("Re-generate reference|%s", ref)
    })
  }

  return(wavelet_params)
}
