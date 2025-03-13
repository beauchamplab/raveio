require_package <- function(package) {
  # if(system.file(package = package) == "") {
  #   stop(sprintf("Package [%s] is needed to run the script. Please install it first via\n  install.packages('%s')", package, package), call. = NULL)
  #
  # }
  targets::tar_assert_package(package)
}


package_installed <- function (pkgs, all = FALSE) {
  re <- sapply(pkgs, function(p) {
    system.file("", package = p) != ""
  })
  if (all) {
    re <- all(re)
  }
  re
}

call_pkg_fun <- function (
    package, f_name, ..., .if_missing = c("error", "warning", "none"),
    .missing_default = NULL, .call_pkg_function = TRUE) {
  stopifnot(length(package) == 1)
  if (!package_installed(package)) {
    .if_missing <- match.arg(.if_missing)
    switch(.if_missing, error = {
      stop("Package ", sQuote(package), " is missing.")
    }, warning = {
      warning("Package ", sQuote(package), " is missing.")
    }, {
    })
    return(.missing_default)
  }
  ns <- asNamespace(package)
  fun <- ns[[f_name]]
  if (.call_pkg_function) {
    if (!is.function(fun)) {
      .if_missing <- match.arg(.if_missing)
      switch(.if_missing, error = {
        stop("Package ", sQuote(package), " does not have function ",
             sQuote(f_name))
      }, warning = {
        warning("Package ", sQuote(package), " does not have function ",
                sQuote(f_name))
      }, {
      })
      return(.missing_default)
    }
    return(fun(...))
  }
  else {
    return(fun)
  }
}


get_namespace_function <- function(ns, func, on_missing) {
  if(system.file(package = ns) == "") {
    if(missing(on_missing)) {
      stop("There is no package called ", sQuote(ns), ". No alternative is provided. Please install this package first")
    }
    return(on_missing)
  }

  return( asNamespace(ns)[[func]] )
}

is_from_namespace <- function(x, check_dipsaus = TRUE) {
  if( check_dipsaus ) {
    is_from_namespace_impl <- asNamespace("dipsaus")[["is_from_namespace"]]
    if(is.function(is_from_namespace_impl)) {
      return( is_from_namespace_impl(x) )
    }
  }

  if(!is.environment(x)) {
    x <- environment(x)
  }
  if(!is.environment(x)) { return(FALSE) }
  if(isNamespace(x)) { return(TRUE) }
  if(identical(x, baseenv())) { return(TRUE) }
  if(identical(x, emptyenv())) { return(FALSE) }
  if(identical(x, globalenv())) { return(FALSE) }
  penv <- parent.env(x)
  if(identical(penv, x)) { return(FALSE) }
  return(is_from_namespace(penv, check_dipsaus = FALSE))
}
