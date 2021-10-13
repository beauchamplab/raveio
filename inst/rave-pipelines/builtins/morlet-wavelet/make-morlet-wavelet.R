library(targets)
library(dipsaus)

...targets <- local({
  source("common.R", local = TRUE)
  source("viridis.R", local = TRUE)

  list(
    plot_wavelet_kernels = tar_target(
      plot_wavelet_kernels,
      {
        pipeline_path <- preprocess_instance$subject$pipeline_path
        dir <- file.path(pipeline_path, "_plots", "wavelet-kernel")
        raveio::dir_create2(dir)
        fname <- strftime(Sys.time(), "[Wavelet-kernels] %Y_%m_%d-%H_%M_%S.pdf")

        params <- settings$save_plots$wavelet_kernel

        grDevices::pdf(
          file = file.path(dir, fname),
          width = params$width,
          height = params$height,
          useDingbats = isTRUE(params$useDingbats),
          onefile = TRUE
        )

        et <- preprocess_instance$electrode_types
        es <- preprocess_instance$electrodes
        sel <- es %in% electrodes

        for(etype in unique(et[sel])){

          srate <- preprocess_instance$sample_rates[sel & et == etype][1]

          kernels <- raveutils::wavelet_kernels(
            freqs = settings$wavelet$frequencies,
            srate = srate,
            wave_num = settings$wavelet$kernel_cycle
          )

          raveutils:::`plot.raveutils-wavelet-kernels`(kernels, cex = )
          mtext(
            sprintf("(%s electrodes)", etype),
            side = 4,
            line = 1,
            at = max(settings$wavelet$kernel_cycle),
            outer = FALSE,
            padj = 0,
            adj = 0
          )
        }

        grDevices::dev.off()

        Sys.time()
      }
    ),
    plot_wavelet_power = tar_target_raw(
      "plot_wavelet_power",
      bquote({
        force(apply_morlet_wavelet)
        pipeline_path <- preprocess_instance$subject$pipeline_path
        dir <- file.path(
          pipeline_path, "_plots", "wavelet-diagnosis",
          strftime(Sys.time(), "%Y_%m_%d-%H_%M_%S"))
        raveio::dir_create2(dir)


        params <- settings$save_plots$diagnosis
        pal <- .(pal)

        # load array
        blocks <- preprocess_instance$blocks
        array_paths <- file.path(
          preprocess_instance$subject$cache_path,
          "wavelet", blocks)

        arrs <- structure(lapply(array_paths, function(path){
          filearray::filearray_load(path, "readonly")
        }), names = blocks)

        freq <- settings$wavelet$frequencies

        lapply(blocks, function(block){
          raveio::dir_create2(file.path(dir, block))

          arr <- arrs[[block]]

          duration <- params$duration * settings$wavelet$downsample_rate

          len <- nrow(arr)
          nsegs <- ceiling(len / duration)

          lapply(electrodes, function(e){
            fname <- file.path(dir, block, sprintf("%d.pdf", e))

            grDevices::pdf(
              file = fname,
              width = params$width,
              height = params$height,
              useDingbats = isTRUE(params$useDingbats),
              onefile = TRUE
            )
            on.exit({ grDevices::dev.off() }, add = FALSE)

            power <- 20 * log10(Mod(arr[,,e, drop = TRUE]))
            power2 <- t(t(power) - colMeans(power))

            zlim <- ceiling(max(power))
            zlim2 <- ceiling(max(abs(power2)))

            par(mfrow = c(4,2))
            image(as.matrix(seq(0, zlim, length.out = 128)), col = pal, axes = FALSE, main = "Color palette for power")
            axis(1, c(0, 1), labels = paste(c(0, zlim), "dB"))

            image(as.matrix(seq(0, zlim2, length.out = 128)), col = pal, axes = FALSE, main = "Color palette for centered power", sub = "Power is substracted by mean for each frequency")
            axis(1, c(0, 0.5, 1), labels = paste(c(-zlim2, 0, zlim2), "dB"))

            par(mfrow = c(4,2))

            for(i in seq_len(nsegs)){
              idx <- (seq_len(duration) + duration * (i-1))

              idx <- idx[idx <= len]
              if(length(idx) < 10){ break }

              time <- idx / settings$wavelet$downsample_rate

              image(
                power[idx,],
                x = time,
                y = freq,
                zlim = c(-zlim, zlim),
                xlab = "Time (s)",
                ylab = "Frequency (Hz)",
                main = "Power (dB)",
                col = pal
              )

              image(
                power2[idx,],
                x = time,
                y = freq,
                zlim = c(-zlim2, zlim2),
                xlab = "Time (s)",
                ylab = "Frequency (Hz)",
                main = "Centered Power (dB)",
                col = pal
              )

            }


          })

        })


      })
    )
  )
})




...targets


