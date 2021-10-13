library(targets)

...targets <- local({
  source("common.R", local = TRUE)

  ._settings_ <- raveio::load_yaml("settings.yaml")
  project_name <- ._settings_$project_name
  subject_code <- ._settings_$subject_code
  save_scripts <- ._settings_$save_pipeline

  list(
    locate_settings_file = tar_target(
      settings_path,
      "settings.yaml",
      format = "file"
    ),
    creating_subject_instance = tar_target_raw(
      "subject",
      bquote({
        subject <- raveio::RAVESubject$new(
          project_name = .(project_name),
          subject_code = .(subject_code),
          strict = FALSE
        )
        subject
      })
    ),
    preparing_preprocess_instance = tar_target(
      preprocess_instance,
      {
        preprocess_instance <- raveio::RAVEPreprocessSettings$new(
          subject = subject, read_only = FALSE
        )
        preprocess_instance$save()
        preprocess_instance
      }
    ),
    save_pipeline_and_export_timestamp = tar_target_raw(
      "save_pipeline_and_export_timestamp",
      bquote({
        if(.(save_scripts)){
          src <- normalizePath(getwd())
          dst <- file.path(subject$pipeline_path, .(target_name))
          dst <- normalizePath(dst, mustWork = FALSE)
          if(dst != src){
            file.copy(src, subject$pipeline_path,
                      recursive = TRUE, copy.date = TRUE)
          }
        }
        timestamp <- Sys.time()
        dir <- file.path(subject$pipeline_path, "_shared")
        raveio::dir_create2(dir)
        fname <- paste0("timestamp-", .(target_name), ".yaml")
        raveio::save_yaml(list(
          pipeline_name = .(target_name),
          last_modified = timestamp
        ), file.path(dir, fname))
        timestamp
      })
    )
  )

})


...targets
