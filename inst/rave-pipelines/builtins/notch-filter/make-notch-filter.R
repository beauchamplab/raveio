library(targets)
library(dipsaus)

source("common.R")
# tar_option_set(packages = c("raveio"))

...targets <- list(
  load_settings = tar_target(
    settings,
    {
      settings <- raveio::load_yaml(settings_path)
      settings$lower_bound <- as.numeric(settings$lower_bound)
      settings$upper_bound <- as.numeric(settings$upper_bound)

      if(length(settings$lower_bound) != length(settings$upper_bound)){
        stop("Notch filter lower bound frequency lengths should equal to the upper bound lengths. Lower bounds: ", paste(settings$lower_bound, collapse = ", "), ", upper bounds: ", paste(settings$upper_bound, collapse = ", "))
      }
      ub <- mapply(max, settings$lower_bound, settings$upper_bound)
      lb <- mapply(min, settings$lower_bound, settings$upper_bound)
      settings$lower_bound <- lb
      settings$upper_bound <- ub
      settings
    }
  ),
  find_electrodes_to_be_filtered = tar_target(
    electrodes,
    {
      es <- preprocess_instance$electrodes
      es <- preprocess_instance$electrodes
      et <- preprocess_instance$electrode_types
      es <- es[et %in% settings$electrode_types]
      if(!length(es)){
        warning("No ", paste(settings$electrode_types, collapse = ", "), " electrode is found. Nothing will be notch-filtered")
      } else {
        raveio::catgl("Notch filter will be applied to ", dipsaus::deparse_svec(es), level = "INFO")
      }
      es
    }
  ),
  apply_notch_filter = tar_target(
    apply_notch_filter,
    {
      blocks <- preprocess_instance$blocks
      sample_rates <- preprocess_instance$sample_rates
      preprocess_path <- preprocess_instance$subject$preprocess_path
      voltage_path <- file.path(preprocess_instance$subject$data_path, "voltage")
      raveio::dir_create2(preprocess_path)
      raveio::dir_create2(voltage_path)

      e <- electrodes[[1]]
      h5_path <- file.path(preprocess_path, 'voltage', sprintf('electrode_%d.h5', e))
      if(!file.exists(h5_path)){
        stop("Cannot find preprocess files. Have you imported data yet?")
      }
      nms <- raveio::h5_names(h5_path)
      for(block in blocks){
        if(!raveio::glue(settings$source) %in% nms){
          stop("Cannot find signal names in the imported files: ", settings$source, " (with block: ", block, ")")
        }
      }

      dipsaus::lapply_callr(seq_along(electrodes), dipsaus::new_function2(
        alist(ii=), bquote({
          electrodes <- .(electrodes)
          blocks <- .(blocks)
          settings <- .(settings)
          sample_rates <- .(sample_rates)
          voltage_path <- .(voltage_path)

          e <- electrodes[[ii]]
          h5_path <- file.path(.(preprocess_path), 'voltage', sprintf('electrode_%d.h5', e))
          h5_volt <- file.path(voltage_path, sprintf("%d.h5", e))

          # load all data
          signals <- structure(lapply(blocks, function(block){
            h5_name <- raveio::glue(settings$source)
            raveio::load_h5(h5_path, h5_name, ram = TRUE)
          }), names = blocks)

          lapply(blocks, function(block){
            filtered <- raveutils::notch_filter(
              signals[[block]],
              sample_rate = sample_rates[[ii]],
              lb = settings$lower_bound,
              ub = settings$upper_bound
            )
            filtered <- as.vector(filtered)
            raveio::save_h5(
              x = filtered,
              file = h5_path,
              name = sprintf('/notch/%s', block),
              chunk = c(1024),
              replace = TRUE
            )
            raveio::save_h5(
              x = filtered,
              file = h5_volt,
              name = sprintf('/raw/voltage/%s', block),
              chunk = c(1024),
              replace = TRUE
            )
          })
        }), quote_type = 'quote')
        , .callback = function(el, ii){
          sprintf('Applying Notch filters...|Electrode %d', electrodes[[i]])
        }, .ncores = raveio::raveio_getopt("max_worker"), .rs = FALSE
      )

      # save subject data
      preprocess_instance$data$notch_params$frequencies <- 0.5 * (
        settings$lower_bound + settings$upper_bound
      )
      preprocess_instance$data$notch_params$half_bandwidths <-
        settings$upper_bound - settings$lower_bound
      for(e in electrodes){
        preprocess_instance$data[[e]]$notch_filtered <- TRUE

        # The pipeline requires notch before the wavelet
        preprocess_instance$data[[e]]$has_wavelet <- FALSE
      }
      preprocess_instance$save()

      Sys.time()
    }
  ),
  saving_pwelch_plots = tar_target(
    notch_plots,
    {
      force(apply_notch_filter)
      if(!settings$save_pwelch){
        return(NULL)
      }
      es <- electrodes
      subject <- preprocess_instance$subject
      chntx <- dipsaus::deparse_svec(es)
      if( stringr::str_length(chntx) > 20 ){
        chntx <- sprintf('total %d electrodes')
      }

      fname <- sprintf('[%s][%s] notch inspection [%s].zip',
                       subject$project_name,
                       subject$subject_code, chntx)
      blocks <- preprocess_instance$blocks
      srates <- preprocess_instance$sample_rates

      winlen <- sapply(srates, function(sample_rate){
        as.integer(raveio::glue(as.character(settings$plots$pwelch_window_size)))
      })
      winlen[is.na(winlen)] <- ceiling(srates[is.na(winlen)] * 2)
      freq_lim <- settings$plots$pwelch_max_frequency
      nclass <- settings$plots$histogram_bin_count

      cex <- settings$plots$font_size_level
      cex %?<-% 2
      fore_col <- settings$plots$pwelch_filtered_color
      fore_col %?<-% "black"
      back_col <- settings$plots$pwelch_original_color
      back_col %?<-% "grey80"

      path <- file.path(subject$pipeline_path, "_plots", "notch-inspect")

      raveutils::export_diagnose_voltage(
        subject = subject, electrodes = es, blocks = blocks,
        h5_names = c("/notch/{block}", settings$source),
        winlens = winlen, freq_lims = freq_lim,
        save_dir = path,
        nclass = nclass, cex = cex, onefile = TRUE,
        fore_col = fore_col, back_col = back_col
      )
      Sys.time()
    }
  )
)



...targets
