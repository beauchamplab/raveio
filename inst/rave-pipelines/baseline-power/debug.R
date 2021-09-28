# setwd("inst/rave-pipelines/baseline-power/")
Sys.setenv(
  "RAVE_PIPELINE" = raveio::find_path(
    "inst/rave-pipelines/baseline-power/",
    root_dir = rstudioapi::getActiveProject())
)

# raveio::pipeline_debug(quick = FALSE)
raveio::pipeline_debug()

raveio::pipeline_visualize()

## Actual test run
# raveio::pipeline_run(type = "async")
# raveio::pipeline_run()

##' Check pipeline progress. Must run `raveio::pipeline_run()` first,
##' otherwise error will occur
raveio::pipeline_progress(method = 'details')


# check variable table
raveio::pipeline_vartable()
raveio::pipeline_read()

if(FALSE){
  # make

  targets::tar_progress()
  targets::tar_meta()
  targets::tar_meta(fields = error, complete_only = TRUE)
  targets::tar_meta(fields = warnings, complete_only = TRUE)

  # generate heatmap
  collapsed_over_trial <- targets::tar_read(collapsed_over_trial)
  targets::
  tmp$baseline
  targets::tar_watch()
}



# ## or register option+1,2,3 shortcut
# dipsaus::rs_add_shortcut(1, expr = {
#   Sys.setenv("RAVE_PIPELINE" = "inst/rave-pipelines/baseline-power/")
# }, force = TRUE)
# dipsaus::rs_add_shortcut(2, expr = {
#   raveio::debug_targets()
# }, force = TRUE)
# dipsaus::rs_add_shortcut(3, expr = {
#   try({
#     raveio::run_pipeline()
#   })
#   raveio::visualize_pipeline()
# }, force = TRUE)
