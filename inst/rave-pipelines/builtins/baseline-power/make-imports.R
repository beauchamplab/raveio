library(targets)
...targets <- list(
  load_settings_import = tar_target(
    settings_import,
    {
      s <- yaml::read_yaml("settings.yaml")
      s['type']
    },
    cue = tar_cue(mode = "always")
  ),
  load_data = tar_target(
    data_list,
    {
      structure(lapply(settings_import$type, function(type){
        lapply(electrodes, function(e){
          e$load_data(type)
        })
      }), names = settings_import$type)
    },
    cue = tar_cue("always")
  ),
  bind_data = tar_target(
    data_array,
    {
      structure(lapply(settings_import$type, function(type){
        target <- normalizePath(file.path("shared", "cache", type),
                                mustWork = FALSE)
        if(dir.exists(target)){
          unlink(target, recursive = TRUE, force = TRUE)
        } else {
          raveio::dir_create2(dirname(target))
        }
        arr <- filearray::filearray_bind(
          .list = data_list[[type]], symlink = TRUE, filebase = target)

        if(length(data_list[[type]])){
          dnames <- dimnames(data_list[[type]][[1]])
          dnames[[4]] <- settings_initialize$electrodes
          dimnames(arr) <- dnames
        }

        arr
      }), names = settings_import$type)

    }
  )
)
