library(targets)
library(dipsaus)

source("common.R")

...targets <- list(
  load_settings = tar_target(
    settings,
    {
      settings <- raveio::load_yaml(settings_path)
      settings$electrodes <- dipsaus::parse_svec(settings$electrodes)
      freq <- settings$wavelet$frequencies
      if(is.character(freq)){
        freq <- sapply(freq, raveio::glue)
      }
      freq <- as.numeric(unlist(freq))
      if(any(is.na(freq) | freq <= 0)){
        stop("Wavelet frequencies contain NA or non-positive numbers")
      }
      settings$wavelet$frequencies <- freq
      cycle <- as.numeric(settings$wavelet$kernel_cycle)
      settings$wavelet$kernel_cycle <- cycle

      raveio::catgl("Wavelet frequency range: {min(freq)} - {max(freq)} Hz ({length(freq)} frequencies)", level = "INFO")
      raveio::catgl("Wavelet kernel cycle range: {min(cycle)} - {max(cycle)}", level = "INFO")

      settings
    }
  ),
  find_electrodes_to_transform = tar_target(
    electrodes,
    {
      es <- preprocess_instance$electrodes
      es <- es[es %in% settings$electrodes]
      blocks <- preprocess_instance$blocks
      if(!length(es)){
        stop("No electrode matching the settings file is found.")
      } else {
        # check source signal
        e <- es[[1]]
        preprocess_path <- preprocess_instance$subject$preprocess_path
        h5_path <- file.path(preprocess_path, 'voltage', sprintf('electrode_%d.h5', e))
        if(!file.exists(h5_path)){
          stop("Cannot find preprocess files. Have you imported data yet?")
        }
        nms <- raveio::h5_names(h5_path)
        for(block in blocks){
          if(!raveio::glue(settings$signal_source) %in% nms){
            stop("Cannot find signal names in the imported files: ", settings$source, " (with block: ", block, ")")
          }
        }

        raveio::catgl("Wavelet filters will be applied to {dipsaus::deparse_svec(es)} at block {paste(blocks, collapse = ', ')}", level = "INFO")
      }

      es
    }
  ),
  apply_morlet_wavelet = tar_target(
    apply_morlet_wavelet,
    {
      wave_params <- settings$wavelet
      signal_source <- settings$signal_source

      if(!length(electrodes)){
        stop("Wavelet - No electrode to transform")
      }

      # Wavelet the first subject, this will set up necessary cache
      electrode <- electrodes[[1]]

      progress <- dipsaus::progress2("Run wavelets", max = length(electrodes), shiny_auto_close = TRUE)
      progress$inc("Test-run the first electrode to set up...")

      raveutils::wavelet_rave_subject(
        subject = preprocess_instance$subject,
        electrode = electrode,
        frequencies = wave_params$frequencies,
        kernel_cycles = wave_params$kernel_cycle,
        downsample_rate = wave_params$downsample_rate,
        demean = wave_params$demean,
        signal_source = signal_source
      )
      progress$close()

      dipsaus::lapply_callr(
        x = electrodes[-1],
        fun = dipsaus::new_function2(
          alist(e=), bquote({
            raveutils::wavelet_rave_subject(
              subject = .(preprocess_instance$subject),
              electrode = e,
              frequencies = .(wave_params$frequencies),
              kernel_cycles = .(wave_params$kernel_cycle),
              downsample_rate = .(wave_params$downsample_rate),
              demean = .(wave_params$demean),
              signal_source = .(signal_source)
            )
          }), quote_type = "quote"
        ),
        .callback = function(e){
          sprintf("Electrode %d", e)
        },
        .ncores = raveio::raveio_getopt("max_worker"),
        .rs = FALSE
      )

      params <- preprocess_instance$data$wavelet_log
      if(!length(params)){
        params <- list()
      }
      p <- list(
        electrodes = electrodes,
        downsample_to = wave_params$downsample_rate,
        frequencies = wave_params$frequencies,
        cycle = wave_params$kernel_cycle
      )
      params[[length(params) + 1]] <- p
      preprocess_instance$data$wavelet_log <- params
      preprocess_instance$data$wavelet_params <- p
      # save subject data
      for(electrode in electrodes){
        preprocess_instance$data[[electrode]]$has_wavelet <- TRUE
      }
      preprocess_instance$save()

      # save frequencies.csv (maybe for future use)
      freq_path <- file.path(preprocess_instance$subject$meta_path, "frequencies.csv")
      write.csv(data.frame(Frequency = wave_params$frequencies), file = freq_path)

      Sys.time()
    }
  )
)



...targets
