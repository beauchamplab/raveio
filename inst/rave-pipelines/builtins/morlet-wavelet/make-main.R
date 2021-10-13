library(targets)

...targets <- local({
  source("common.R", local = TRUE)

  # load & combine pipelines
  targets <- raveio::load_targets(
    "make-shared.R",
    "make-long-steps.R",
    "make-morlet-wavelet.R"
  )

  # manually hard-wire two targets
  deps <- targets$apply_morlet_wavelet$command$deps
  deps <- unique(c(deps[deps != "subject"], "preprocess_instance"))
  targets$apply_morlet_wavelet$command$deps <- deps

  targets$save_pipeline_and_export_timestamp$command$deps <- unique(c(
    targets$save_pipeline_and_export_timestamp$command$deps,
    "apply_morlet_wavelet"
  ))

  # reorder for debug

  cat(names(targets), sep = '", \n"')

  targets
  targets[c(
    "locate_settings_file",
    "load_settings",
    "creating_subject_instance",
    "preparing_preprocess_instance",
    "find_electrodes_to_transform",
    "apply_morlet_wavelet",
    "save_pipeline_and_export_timestamp",
    "plot_wavelet_kernels",
    "plot_wavelet_power"
  )]

})

...targets
