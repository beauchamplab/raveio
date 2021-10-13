library(targets)

...targets <- local({
  source("common.R", local = TRUE)

  # load & combine pipelines
  targets <- raveio::load_targets(
    "make-shared.R",
    "make-notch-filter.R"
  )

  # manually hard-wire two targets
  targets$apply_notch_filter$command$deps <- unique(c(
    targets$apply_notch_filter$command$deps,
    "preprocess_instance"
  ))

  targets$save_pipeline_and_export_timestamp$command$deps <- unique(c(
    targets$save_pipeline_and_export_timestamp$command$deps,
    "settings", "apply_notch_filter"
  ))

  # reorder for debug
  targets[c(
    "locate_settings_file",
    "load_settings",
    "creating_subject_instance",
    "preparing_preprocess_instance",
    "find_electrodes_to_be_filtered" ,
    "apply_notch_filter",
    "save_pipeline_and_export_timestamp",
    "saving_pwelch_plots"
  )]

})

...targets
