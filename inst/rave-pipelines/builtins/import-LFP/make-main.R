library(targets)

...targets <- local({
  source("common.R", local = TRUE)

  # load & combine pipelines
  targets <- raveio::load_targets(
    # "make-shared.R",
    "make-import-LFP.R"
  )

  names(targets)

  # force save-pipeline to depend on import-LFP
  # targets$save_pipeline_and_export_timestamp$command$deps <- unique(c(
  #   targets$save_pipeline_and_export_timestamp$command$deps,
  #   "import_LFP", "checked_settings"
  # ))

  # reorder targets basically for debug use
  # the order is internally determined using `$command$deps`
  # targets[c(
  #   'locate_settings_file',
  #   'load_settings',
  #   'creating_subject_instance',
  #   'preparing_preprocess_instance',
  #   "initialize_and_check_subject",
  #   'import_LFP_analog_traces',
  #   'save_pipeline_and_export_timestamp',
  #   "generate_electrodes_csv"
  # )]

  targets
})
...targets

# raveio::pipeline_visualize()
