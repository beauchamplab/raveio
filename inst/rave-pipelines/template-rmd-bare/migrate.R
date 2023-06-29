#' Function defines how pipeline should be migrated from one 'RAVE' subject
#' to another
#' @param project_name character, new 'RAVE' project to be copied to
#' @param subject_code character, new 'RAVE' subject to be copied to
#' @param ... other parameters might be used or ignored, depending on your
#' implementation
#' @param overwrite whether to overwrite the pipeline if it exists in the
#' new subject; this allows users to double-check. Make sure to remove the
#' previous pipeline data as well
#' @param backup whether to backup the existing pipeline; only used when
#' \code{overwrite=TRUE}
#' @returns Nothing
migrate <- function(project_name, subject_code, ..., overwrite = FALSE, backup = TRUE){

  source("common.R", local = TRUE)

  # migrate to project
  if(missing(subject_code) || is.null(subject_code)){

    ## BEGIN: customize the code to migrate to project pipeline folder

    stop("This pipeline is designed at subject level. Please specify the subject")

    ## END: customized code

  } else {

    # create the subject instance
    subject <-
      raveio::RAVESubject$new(project_name = project_name,
                              subject_code = subject_code,
                              strict = FALSE)
    # initialize folders
    subject$initialize_paths(include_freesurfer = FALSE)

    ## BEGIN: customize code to migrate subject-level pipeline

    # this is the path for new pipeline
    pipeline_directory <- file.path(subject$pipeline_path, target_name)
    if(dir.exists(pipeline_directory)){
      # some files exist, make sure don't overwrite important files
      if(!overwrite){
        stop("A pipeline already exists. Please use `overwrite=TRUE` to remove the old one")
      }
      # remove the old files, including data to prevent artifacts
      pipeline_directory <- normalizePath(pipeline_directory)
      if(backup){
        backup_directory <- paste0(
          pipeline_directory,
          strftime(Sys.time(), "_old_%Y%m%d-%H%M%S")
        )
        file.rename(pipeline_directory, backup_directory)
      } else {
        unlink(pipeline_directory, recursive = TRUE)
      }
    }

    # move current directory to the new subject pipeline directory
    # DO NOT activate the new module
    raveio::pipeline_fork(
      src = ".",
      dest = file.path(subject$pipeline_path, target_name),
      activate = FALSE,
      filter_pattern = "\\.(R|yaml|txt|csv|fst|conf)$"
    )

    # Modify the settings file of target pipeline
    settings_path <- file.path(subject$pipeline_path, target_name, "settings.yaml")

    # ensure settings.yaml is correctly set
    settings <- raveio::load_yaml(settings_path)

    # change the subject to the targeting subject
    settings$project_name <- project_name
    settings$subject_code <- subject_code

    # save changes
    raveio::save_yaml(settings, file = settings_path)


    ## END: customized code

  }

}

