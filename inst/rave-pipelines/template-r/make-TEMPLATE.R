library(targets)
library(dipsaus)

source("common.R", local = TRUE, chdir = TRUE)
# tar_option_set(packages = c("raveio"))

...targets <- list(
  load_settings = tar_target(
    `TEMPLATE_settings`,
    {
      settings <- as.list(raveio::load_yaml("settings.yaml"))

      ## Do some checks here, remove some entries that do not need to get cached

      ## load project, subject instances as needed

      # strict requires project or subject to exist
      # recommended to be true is you want to assume their existence
      project <- raveio::RAVEProject$new(project_name = settings$project_name, strict = FALSE)
      subject <- raveio::RAVESubject$new(project_name = settings$project_name, subject_code = settings$subject_code, strict = FALSE)
      preprocess_tools <- subject$preprocess_settings

      # save instances
      settings$project <- project
      settings$subject <- subject
      settings$preprocess_tools <- preprocess_tools

      # make sure return the settings as `TEMPLATE_settings`
      return(settings)
    },
    # Always run this target and cache the results.
    # As long as the results remain unchanged, the following targets
    # will not be triggered
    cue = tar_cue("always")
  )
)


# Fixed usage, return target list as `...targets`
...targets
