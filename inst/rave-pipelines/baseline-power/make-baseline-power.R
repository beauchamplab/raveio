library(targets)

...targets <- list(
  load_baseline_settings = tar_target(
    baseline_settings,
    {
      s <- yaml::read_yaml("./settings.yaml")
      s$baseline <- unlist(s$baseline)
      s
    },
    cue = tar_cue("always"),
    memory = "persistent"
  ),
  calculate_baseline = tar_target(
    baseline,
    {
      power <- data_array[["power"]]
      if(is.null(power)){
        stop("Power data not loaded")
      }
      dim <- dim(power)
      dim[length(dim)] <- 1
      plen <- prod(dim)
      target <- file.path(electrodes[[1]]$cache_root, "..")
      raveio::dir_create2(target)
      target <- normalizePath(target, mustWork = TRUE)
      target <- file.path(target, "_baseline")
      if(dir.exists(target)){
        output <- filearray::filearray_load(target, "readwrite")
        if(!all(dim(output) == dim(power))){
          output$delete()
        }
        if(!identical(dimnames(output), dimnames(power))){
          dimnames(output) <- dimnames(power)
        }
      }
      if(!dir.exists(target)){
        output <- filearray::filearray_create(
          filebase = target,
          dimension = dim(power),
          type = "float",
          partition_size = 1L
        )
      }

      bl_range <- unlist(baseline_settings$baseline)
      dnames <- dimnames(power)
      baseline_indexpoints <- which(
        dnames$Time >= bl_range[[1]] & dnames$Time <= bl_range[[2]])

      filearray::fmap(power, function(input){
        x <- input[[1]]
        dim(x) <- dim
        bl <- dipsaus::baseline_array(
          x, along_dim = 2L,
          baseline_indexpoints = baseline_indexpoints,
          method = baseline_settings$baseline_method
        )
        bl
      }, .y = output, .input_size = plen)
      output
    }
  ),
  tar_target(
    collapsed_over_trial,
    {
      dim <- dim(baseline)
      dim[length(dim)] <- 1
      plen <- prod(dim)
      # apply(baseline, 4, function(x){
      #   dipsaus::collapse(keep = c(2,1), average = TRUE)
      # })
      filearray::fmap2(baseline, function(input){
        x <- input[[1]]
        dim(x) <- dim
        dipsaus::collapse(x, keep = c(2,1), average = TRUE)
      }, .input_size = plen, .simplify = TRUE)
    }
  ),
  tar_target(
    heatmap,
    {
      image(collapsed_over_trial[,,1])
    },
    cue = tar_cue("always")
  )

)

...targets
