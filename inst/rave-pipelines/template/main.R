Sys.setenv(
  "RAVE_PIPELINE" = raveio::find_path(
    "inst/rave-pipelines/baseline-power/",
    root_dir = rstudioapi::getActiveProject())
)

raveio::pipeline_run()
