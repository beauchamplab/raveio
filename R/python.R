
# Internal function to activate rpymat with ants and antspynet
activate_ants <- function(verbose = TRUE) {

  using_antspynet <- FALSE
  tryCatch({
    # Try to use dedicated ANTs virtual environment

    # make sure rpymat version is at least 0.1.7.1
    rpymat_ver <- utils::packageVersion("rpymat")
    if( utils::compareVersion(as.character(rpymat_ver), "0.1.7.1") >= 0 ) {

      # Check if
      if(!file.exists(rpymat::env_path(env_name = "rave-ants"))) {
        rpymat::configure_conda(python_ver = "3.11", env_name = "rave-ants")
      }
      rpymat::ensure_rpymat(env_name = "rave-ants", verbose = verbose)
      using_antspynet <- TRUE

    }

  }, error = function(e) {
    message(e)
    message("Unable to configure dedicated conda environment for ANTs. Using the default environment.")
  })

  if(using_antspynet) {
    ants_module_to_check <- "antspynet"
  } else {
    ants_module_to_check <- "ants"
    rpymat::ensure_rpymat(verbose = verbose)
  }

  # check if rpyANTs is configured
  if(!isTRUE(rpyANTs::ants_available(ants_module_to_check))) {
    rpyANTs::install_ants(verbose = verbose)
  }
}
