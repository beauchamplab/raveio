require_package <- function(package) {
  if(system.file(package = package) == "") {
    stop(
      sprintf(
        "Package [%s] is needed to run the script. Please install it first via\n  install.packages('%s')",
        package,
        package
      ),
      call. = NULL
    )

  }
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

# restore_subject_instance <- function(...) {
#   call_pkg_fun(package = "ravecore", f_name = "restore_subject_instance", ...)
# }

restore_subject_instance <- function(subject_id, strict = FALSE) {
  if(inherits(subject_id, 'RAVESubject')){
    return(subject_id)
  } else {
    if(inherits(subject_id, "Subject")) {
      # RAVE 1.0 subject instance
      stopifnot2(is.character(subject_id$id),
                 msg = "`as_rave_subject`: Cannot find subject ID from the given input")
      subject_id <- subject_id$id
    }
    if(startsWith(subject_id, "@meta_analysis")) {
      subject_id <- gsub("^@meta_analysis/", "", subject_id)
      return( RAVEMetaSubject$new(subject_id) )
    } else {
      return( RAVESubject$new(subject_id, strict = strict) )
    }
  }
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



