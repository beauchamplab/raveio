##' Let RAVE switch to this module pipeline
##'
Sys.setenv("RAVE_PIPELINE" = "~/Dropbox/projects/rave-pipelines/morlet-wavelet/")

##' Debug pipeline: assign variables to global environment
##' so that you can print them directly
##' setting `quick = FALSE` allows you to run the whole pipeline
##' including its parent pipelines. This is useful is the current
##' pipeline is a combination of multiple sub-pipelines
##'
raveio::pipeline_debug(quick = TRUE)
# raveio::pipeline_debug()

##' Visualize pipeline and the relationship between intermediate variables
##' This requires extra package `visNetwork`.
##' Please run `install.packages('visNetwork')` if missing packages
##'
raveio::pipeline_visualize()

##' Test run the pipeline in production mode
##'
# raveio::pipeline_run(async = TRUE)
raveio::pipeline_run(names = "apply_morlet_wavelet")
raveio::pipeline_run(names = "plot_wavelet_power", async = TRUE)

##' Check pipeline progress to see status (cached/skipped, built, errored...)
##' Must run `raveio::pipeline_run()` first, otherwise error will occur
##'
raveio::pipeline_progress(method = 'details')


##' Get current variable table. Run `raveio::pipeline_run()` first
##'
raveio::pipeline_vartable()

##' Get intermediate variables
##'
raveio::pipeline_hasname("subject")
subject <- raveio::pipeline_read("subject")


##' Launch a shiny app to watch the pipeline. Please install extra
##' packages first:
##' `install.packages(c('bs4Dash', 'gt', 'pingr', 'shinybusy'))`
##'
raveio::pipeline_watch()

dipsaus::rs_add_shortcut(1, {
  raveio::pipeline_debug(quick = TRUE, env = .GlobalEnv)
})
dipsaus::rs_add_shortcut(2, {
  raveio::pipeline_run()
}, force = FALSE)
