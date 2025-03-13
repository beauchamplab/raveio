# get options whether the data directory is on network
# If enabled, then HDF5 files should be copied to local tempdir
# and read if there are multiiple reads from the same file
using_netdrive <- function(){
  if(raveio_getopt("using_netdrive", FALSE)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

finalize_installation_demo_data <- function(update = FALSE, test = FALSE) {
  subject <- RAVESubject$new(project_name = "YAEL", subject_code = "yael_demo_001", strict = FALSE)

  if( test ) {
    if( is.na(subject$freesurfer_path) || !dir.exists(subject$path) ) {
      # needs install
      return(TRUE)
    }
    return(FALSE)
  }
  if(update || is.na(subject$freesurfer_path)) {
    install_subject(
      "yael_demo_001",
      overwrite = TRUE,
      backup = FALSE,
      use_cache = TRUE,
      ask = FALSE
    )
  }

  subject <- RAVESubject$new(project_name = "demo", subject_code = "DemoSubject", strict = FALSE)
  if(update || !dir.exists(subject$path)) {
    install_subject(
      "DemoSubject",
      overwrite = TRUE,
      backup = FALSE,
      use_cache = TRUE,
      ask = FALSE
    )
  }
}

finalize_installation <- function(
  upgrade = c('ask', 'always', 'never', 'config-only', 'data-only'),
  async = TRUE, ...){

  upgrade <- match.arg(upgrade)

  ravepipeline::ravepipeline_finalize_installation(upgrade = upgrade, async = async, ...)


  missing_demo_data <- finalize_installation_demo_data(test = TRUE)
  update_sample_data <- FALSE
  if(upgrade %in% c("always", "data-only")) {
    update_sample_data <- TRUE
  }

  if( missing_demo_data || update_sample_data ) {
    if(async) {
      dipsaus::rs_exec(
        bquote({
          ns <- asNamespace("raveio")
          ns$finalize_installation_demo_data(update = .(update_sample_data))
          message("Done.")
        }),
        quoted = TRUE,
        name = "Installing demo data",
        focus_on_console = TRUE
      )
    } else {
      finalize_installation_demo_data(update = update_sample_data)
    }
  }

  invisible()
}


.onLoad <- function(libname, pkgname) {

  # check if ravetools is installed
  if(isNamespaceLoaded("ravetools") || system.file(package = "ravetools") != "") {
    options("raveio.use.ravetools" = TRUE)
  }

}

