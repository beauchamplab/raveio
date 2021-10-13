# Markdown engines

RAVE_KNITR_SUPPORTED_LANG <- c("R", "python")

check_knit_packages <- function(languages = c("R", "python")){
  pkgs <- c('knitr', 'rmarkdown')
  if("python" %in% languages){
    pkgs <- c(pkgs, 'reticulate')
  }
  # check knitr, rmarkdown, reticulate
  pkgs <- pkgs[!dipsaus::package_installed(pkgs)]

  if(length(pkgs)){
    if(!interactive()){
      stop("Package(s) ", paste(pkgs, collapse = ", "), " are required. Please run install.packages(c(", paste0('"', pkgs, '"', collapse = ", "), ")) to install them")
    }
    message("Package(s) ", paste(pkgs, collapse = ", "), " are required. ")
    ans <- utils::askYesNo("Do you want to install them?")
    if(!isTRUE(ans)){
      stop("User abort.")
    }
    remotes::install_cran(pkgs, upgrade = "never")
  }


}

rave_knit_r <- function(export, code, deps = NULL, cue = "thorough", ...){
  # code <- options$code
  code <- paste(c("{", code, "}"), collapse = "\n")
  expr <- parse(text = code)[[1]]
  bquote(
    targets::tar_target_raw(
      name = .(export),
      command = quote({
        .(expr)
        return(.(str2lang(export)))
      }),
      deps = .(deps),
      cue = targets::tar_cue(.(cue))
    )
  )
}

rave_knit_python <- function(export, code, deps = NULL, cue = "thorough", convert = FALSE, local = FALSE, ...){
  bquote(
    targets::tar_target_raw(
      name = .(export),
      command = quote({
        reticulate::py_run_string(
          code = paste(.(code), collapse = "\n"),
          local = .(local),
          convert = .(convert)
        )
        return(reticulate::py[[.(export)]])
      }),
      deps = .(deps),
      cue = targets::tar_cue(.(cue))
    )
  )
}

rave_knitr_engine <- function(targets){

  knitr::knit_engines$set("rave" = function(options) {
    # for R
    if(startsWith(options$export, "unnamed-chunk")){
      stop("RAVE pipeline target chunk must be named. For example:\n",
           '{r target_name, engine="RAVE-target" ...')
    }
    if(grepl("^[^A-Za-z0-9\\-_.]+$", options$export)){
      stop("Chunk label (target name) must be valid variable name that ONLY contains letters, digits, `_`, `-`, or `.`")
    }

    if(options$export %in% sapply(targets, "[[", "export")){
      stop("Chunk with the same export target `", options$export, "` already exists. Cannot have two targets sharing the same export name. Please consider renaming the exported variable name")
    }

    lang <- options$language
    if(length(lang) != 1 || lang == "r"){
      lang <- "R"
    }

    if(! lang %in% RAVE_KNITR_SUPPORTED_LANG){
      stop("Chunk `", options$label, "` has invalid `language` options. Please choose from the following engines: \n  ", paste(RAVE_KNITR_SUPPORTED_LANG, collapse = ", "))
    }

    options$language <- lang
    if(length(options$depends)){
      options$deps <- unlist(strsplit(options$depends, "[, ]+"))
    }
    # assign('options', options, envir = globalenv())

    targets$add(options)

    real_engine <- knitr::knit_engines$get(lang)

    env <- knitr::knit_global()
    switch(
      lang,
      "R" = {
        # keep names
        nms <- c(ls(env, all.names = TRUE, sorted = FALSE), options$export)
        options$engine <- "r"
        res <- real_engine(options)
        nms2 <- ls(env, all.names = TRUE, sorted = FALSE)
        if(!options$export %in% nms2){
          stop("Cannot find variable to be exported in chunk ", options$label)
        }
        nms2 <- nms2[!nms2 %in% nms]
        if(length(nms2)){
          rm(list = nms2, envir = env, inherits = FALSE)
        }
      },
      "python" = {
        options$engine <- "python"
        res <- real_engine(options)
        env[[options$export]] <- reticulate::py[[options$export]]
      },
      {
        # not reach here
        stop("unsupported language")
      }
    )

    return(res)

  })
}

rave_knitr_build <- function(targets, make_file){
  # generate targets
  targets <- as.list(targets)
  nms <- lapply(targets, function(options){
    options$label
  })
  exprs <- structure(
    lapply(targets, function(options){
      switch(
        options$language,
        "R" = {
          quos <- do.call(rave_knit_r, options)
        },
        "python" = {
          quos <- do.call(rave_knit_python, options)
        },
        {
          stop("Unsupported programming language: ", options$language)
        }
      )
      quos
    }), names = nms
  )
  if(file.exists("settings.yaml")){
    settings <- as.list(load_yaml("settings.yaml"))

    # to please CRAN check
    settings_path <- NULL
    nms <- names(settings)

    extras <- structure(lapply(nms, function(nm){
      bquote(
        targets::tar_target_raw(
          .(nm),
          quote({
            settings[[.(nm)]]
          }),
          deps = "settings"
        )
      )
    }), names = paste0("input_", nms))
    exprs <- c(
      list(
        "__Check_settings_file" = quote(
          targets::tar_target_raw(
            "settings_path",
            "settings.yaml",
            format = "file"
          )
        ),
        "__Load_settings" = quote(
          targets::tar_target_raw(
            "settings",
            quote({
              load_yaml(settings_path)
            }),
            deps = "settings_path"
          )
        )
      ), extras, exprs)
  }
  call <- as.call(c(list(quote(list)), exprs))
  call <- as.call(list(quote(`<-`), quote(...targets), call))
  # write to target file
  writeLines(c(
    "library(targets)",
    "library(raveio)",
    'source("common.R", local = TRUE, chdir = TRUE)',
    deparse(call)
  ), con = make_file)
  invisible(call)
}

#' Configure \code{'rmarkdown'} files to build 'RAVE' pipelines
#' @description Allows building 'RAVE' pipelines from \code{'rmarkdown'} files.
#' Please use it in \code{'rmarkdown'} scripts only. Use
#' \code{\link{pipeline_create_template}} to create an example.
#' @param languages one or more programming languages to support; options are
#' \code{'R'} and \code{'python'}
#' @return A function that is supposed to be called later that builds the
#' pipeline scripts
#' @export
configure_knitr <- function(languages = c("R", "python")){

  if(!all(languages %in% RAVE_KNITR_SUPPORTED_LANG)){
    stop("Only the following languages are supported: ", paste(RAVE_KNITR_SUPPORTED_LANG, collapse = ", "), ".")
  }
  if(file.exists("settings.yaml")){
    settings <- as.list(load_yaml("settings.yaml"))
    list2env(settings, envir = knitr::knit_global())
  }

  check_knit_packages(languages)

  targets <- dipsaus::fastqueue2()

  rave_knitr_engine(targets)

  function(make_file){
    rave_knitr_build(targets, make_file)
  }
}
