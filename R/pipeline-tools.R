load_target <- function(src){
  s <- source(src, local = TRUE, echo = FALSE, verbose = FALSE, chdir = TRUE)
  target <- s$value
  if(!is.list(target)){
    stop("Script ", src, " must need to end with a list.")
  }
  target
}



#' @rdname rave-pipeline
#' @export
load_targets <- function(...){
  targets <- lapply(c(...), load_target)
  do.call("c", targets)
}

activate_pipeline <- function(pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")) {
  if(!dir.exists(pipe_dir)){
    stop("`pipe_dir` must be the directory of pipeline files.")
  }

  pipe_dir <- normalizePath(pipe_dir)
  common_script <- file.path(pipe_dir, "common.R")
  entry_point_path <- file.path(pipe_dir, "make-main.R")

  if(!file.exists(common_script)){
    stop("Cannot find `common.R` in the pipeline.")
  }

  if(!file.exists(entry_point_path)){
    stop("Cannot find entry point (`make-main.R`) in the pipeline.")
  }

  parent_frame <- parent.frame()
  current <- Sys.getenv("TAR_PROJECT", "main")
  wd <- normalizePath(getwd())
  do.call(
    on.exit, list(bquote({
      setwd(.(wd))
      Sys.setenv("TAR_PROJECT" = .(current))
    }), add = TRUE, after = TRUE),
    envir = parent_frame
  )
  setwd(pipe_dir)

  tmpenv <- new.env(parent = globalenv())
  source("common.R", local = tmpenv)

  # Sys.setenv("RAVE_PIPELINE" = pipe_dir)

  attr(pipe_dir, "target_name") <- tmpenv$target_name
  attr(pipe_dir, "target_script") <- tmpenv$target_script
  attr(pipe_dir, "target_directory") <- tmpenv$target_directory
  pipe_dir
}

#' @rdname rave-pipeline
#' @export
pipeline_target_names <- function(pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")){
  pipe_dir <- activate_pipeline(pipe_dir)

  # find targets that are not in the main
  script <- attr(pipe_dir, "target_script")

  all_targets <- load_target("make-main.R")
  target_names <- unlist(lapply(all_targets, function(x){
    x$settings$name
  }))
  target_names
}

#' @rdname rave-pipeline
#' @export
pipeline_debug <- function(
  quick = TRUE,
  env = parent.frame(),
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  skip_names
){
  pipe_dir <- activate_pipeline(pipe_dir)

  # find targets that are not in the main
  script <- attr(pipe_dir, "target_script")

  main_targets <- load_target(script)
  all_targets <- load_target("make-main.R")

  if(quick){
    if(missing(skip_names)){
      main_target_names <- unlist(lapply(main_targets, function(x){
        x$settings$name
      }))
      target_names <- unlist(lapply(all_targets, function(x){
        x$settings$name
      }))
      skip_names <- unname(target_names[!target_names %in% main_target_names])
    }
  } else {
    skip_names <- NULL
  }

  if(quick){
    # build with targets
    do.call(targets::tar_make, list(
      callr_function = NULL,
      envir = env, names = as.call(
        list(quote(targets::all_of), skip_names)
      )
    ))
  }

  nms <- names(all_targets)

  w <- getOption("width", 80)

  started <- Sys.time()

  for(ii in seq_along(all_targets)){
    if(length(nms) < ii || nms[[ii]] == ""){
      nm <- "(No name)"
    } else {
      nm <- stringr::str_split(nms[[ii]], "_")[[1]]
      nm[[1]] <- stringr::str_to_sentence(nm[[1]])
      nm <- paste(nm, collapse = " ")
    }

    ...t <- all_targets[[ii]]
    name <- ...t$settings$name
    if(name %in% skip_names){
      v <- targets::tar_read_raw(name)
      assign(name, v, envir = env)
    } else {

      r <- w - stringr::str_length(nm) - 14
      if( r <= 2 ){ r <- 2 }
      nm <- paste(c(
        sprintf(" (%.2f s) ", dipsaus::time_delta(started, Sys.time())),
        rep("-", r), " ", nm, "\n"), collapse = "")
      catgl(nm, level = "INFO")

      counter <- Sys.time()

      {
        message("Evaluating -> ", name, "\r", appendLF = FALSE)
        v <- eval(...t$command$expr, new.env(parent = env))
        assign(name, v, envir = env)
        str <- deparse1(v)
        str <- stringr::str_replace_all(str, "\t|\n", "  ")
        r <- w - stringr::str_length(name) - 25
        if(r < 0){
          r <- w - 5
          s <- "`{name}` <- \n    "
        } else {
          s <- "`{name}` <- "
        }
        str <- stringr::str_sub(str, end = r)
        delta <- dipsaus::time_delta(counter, Sys.time())
        catgl(sprintf("[+%6.2f s] ", delta), s, str, "\n")
        counter <- Sys.time()
      }
    }
  }
}

# May be removed later if not really useful
pipeline_run_interactive <- function(
  names, skip_names, env = parent.frame(),
  pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
){
  pipe_dir <- activate_pipeline(pipe_dir)

  # find targets that are not in the main
  script <- attr(pipe_dir, "target_script")

  main_targets <- load_target(script)
  all_targets <- load_target("make-main.R")

  main_target_names <- unlist(lapply(main_targets, function(x){
    x$settings$name
  }))
  target_names <- unlist(lapply(all_targets, function(x){
    x$settings$name
  }))
  if(missing(skip_names)){
    skip_names <- unname(target_names[!target_names %in% main_target_names])
  }

  if(length(skip_names)){
    # build with targets
    do.call(targets::tar_make, list(
      callr_function = NULL,
      envir = env, names = skip_names
    ))
    for(nm in skip_names){
      assign(nm, pipeline_read(nm, pipe_dir = pipe_dir), envir = env)
    }
  }

  w <- getOption("width", 80)
  started <- Sys.time()

  nms <- names(all_targets)

  for(nm in names){
    ii <- which(target_names == nm)
    if(length(ii)){

      desc <- nms[[ii]]
      if(desc == "") {
        desc <- "(No name)"
      } else {
        desc <- stringr::str_split(nms[[ii]], "_")[[1]]
        desc[[1]] <- stringr::str_to_sentence(desc[[1]])
        desc <- paste(desc, collapse = " ")
      }

      ...t <- all_targets[[ii]]
      name <- ...t$settings$name
      r <- w - stringr::str_length(nm) - 14
      if( r <= 2 ){ r <- 2 }
      nm <- paste(c(
        sprintf(" (%.2f s) ", dipsaus::time_delta(started, Sys.time())),
        rep("-", r), " ", nm, "\n"), collapse = "")
      catgl(nm, level = "INFO")

      counter <- Sys.time()
      {
        message("Evaluating -> ", name, "\r", appendLF = FALSE)
        expr <- ...t$command$expr
        tryCatch({
          v <- eval(expr, new.env(parent = env))
          assign(name, v, envir = env)
          str <- deparse1(v)
          str <- stringr::str_replace_all(str, "\t|\n", "  ")
          r <- w - stringr::str_length(name) - 25
          if(r < 0){
            r <- w - 5
            s <- "`{name}` <- \n    "
          } else {
            s <- "`{name}` <- "
          }
          str <- stringr::str_sub(str, end = r)
          delta <- dipsaus::time_delta(counter, Sys.time())
          catgl(sprintf("[+%6.2f s] ", delta), s, str, "\n")
        }, error = function(e){
          e$call <- expr
          stop(e)
        })
        counter <- Sys.time()
      }
    }

  }
}

#' @rdname rave-pipeline
#' @export
pipeline_visualize <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
){
  pipe_dir <- activate_pipeline(pipe_dir)
  targets::tar_visnetwork(targets_only = TRUE, shortcut = FALSE)
}



#' @rdname rave-pipeline
#' @export
pipeline_progress <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  method = c("summary", "details", "custom"),
  func = targets::tar_progress_summary
){
  method <- match.arg(method)
  activate_pipeline(pipe_dir)

  switch (
    method,
    "summary" = targets::tar_progress_summary(),
    "details" = targets::tar_progress(),
    {
      func()
    }
  )
}

#' @rdname rave-pipeline
#' @export
pipeline_fork <- function(
  src = Sys.getenv("RAVE_PIPELINE", "."),
  dest = tempfile(pattern = "rave_pipeline_"),
  filter_pattern = "\\.(R|yaml|txt|csv|fst|conf)$",
  activate = FALSE
){
  if(!dir.exists(src)){
    stop("pipeline_fork: `src` must be a pipeline directory")
  }
  if(!file.exists(file.path(src, "common.R"))){
    stop("pipeline_fork: `src/common.R` is missing")
  }
  if(!file.exists(file.path(src, "make-main.R"))){
    stop("pipeline_fork: `src/make-main.R` is missing")
  }


  fs <- list.files(src, include.dirs = FALSE, full.names = FALSE, pattern = filter_pattern)

  dir_create2(dest)
  dest <- normalizePath(dest, mustWork = TRUE)
  file.copy(from = file.path(src, fs), to = dest, overwrite = TRUE,
            copy.date = TRUE)

  if( activate ){
    pipeline_build(dest)
    Sys.setenv("RAVE_PIPELINE" = dest)
  }
  dest
}

#' @rdname rave-pipeline
#' @export
pipeline_build <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
){
  pipe_dir <- activate_pipeline(pipe_dir)
  configure_src <- file.path(pipe_dir, "configure.R")

  # build _targets.yaml
  source(configure_src, local = TRUE, chdir = TRUE)

  TRUE
}

#' @rdname rave-pipeline
#' @export
pipeline_read <- function(
  var_names,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  branches = NULL,
  ifnotfound = NULL
) {
  pipe_dir <- activate_pipeline(pipe_dir)
  if(length(var_names) > 1){
    re <- structure(lapply(var_names, function(vn){
      tryCatch({
        do.call(targets::tar_read,
                list(name = str2lang(vn), branches = branches))
      }, error = function(e){
        ifnotfound
      })
    }), names = var_names)
  } else {
    re <- tryCatch({
      do.call(targets::tar_read,
              list(name = str2lang(var_names), branches = branches))
    }, error = function(e){
      ifnotfound
    })
  }
  return(re)
}

#' @rdname rave-pipeline
#' @export
pipeline_vartable <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  targets_only = TRUE,
  complete_only = FALSE,
  ...
) {
  pipe_dir <- activate_pipeline(pipe_dir)
  tryCatch({
    targets::tar_meta(..., targets_only = targets_only,
                      complete_only = complete_only)
  }, error = function(e){
    NULL
  })
}

#' @rdname rave-pipeline
#' @export
pipeline_hasname <- function(
  var_names,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
) {
  tbl <- pipeline_vartable(pipe_dir = pipe_dir)
  var_names %in% tbl$name
}

#' @rdname rave-pipeline
#' @export
pipeline_watch <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  targets_only = TRUE, ...
){
  pipe_dir <- activate_pipeline(pipe_dir)
  targets::tar_watch(..., targets_only = targets_only, display = 'graph')
}

#' @rdname rave-pipeline
#' @export
pipeline_create_template <- function(
  root_path, pipeline_name, overwrite = FALSE,
  activate = TRUE, template_type = c("rmd", 'r')
) {
  template_type <- match.arg(template_type)
  pipeline_name <- tolower(pipeline_name)
  stopifnot2(!pipeline_name %in% c("main", "imports", "initialize", "template"),
             msg = "Pipeline name cannot be `main`, `imports`, `template`, or `initialize`")
  pipe_path <- file.path(root_path, pipeline_name)
  if(dir.exists(pipe_path)){
    if(!overwrite){
      stop("Pipeline ", pipeline_name, " already exists at\n  ", pipe_path)
    } else {
      unlink(pipe_path, recursive = TRUE)
    }
  }
  dir_create2(pipe_path)
  pipe_path <- normalizePath(pipe_path)

  # create a skeleton template
  template_path <- system.file("rave-pipelines", sprintf("template-%s", template_type), package = 'raveio', mustWork = TRUE)
  fs_src <- list.files(template_path)
  fs_dst <- stringr::str_replace_all(fs_src, "TEMPLATE", pipeline_name)
  file.copy(file.path(template_path, fs_src), file.path(pipe_path, fs_dst), overwrite = overwrite, copy.date = TRUE)
  fs <- file.path(pipe_path, fs_dst)
  for(f in fs){
    s <- readLines(f)
    s <- stringr::str_replace_all(s, "TEMPLATE_PATH", pipe_path)
    s <- stringr::str_replace_all(s, "TEMPLATE", pipeline_name)
    s <- stringr::str_replace_all(s, "PROJECT_NAME", "demo")
    s <- stringr::str_replace_all(s, "SUBJECT_CODE", "DemoSubject")
    writeLines(s, f)
  }
  settings <- yaml::read_yaml(file.path(pipe_path, "settings.yaml"))
  settings$epoch <- "default"
  settings$electrodes <- dipsaus::deparse_svec(14L)
  settings$reference <- "default"

  save_yaml(settings, file.path(pipe_path, "settings.yaml"))

  # build the pipeline
  pipeline_build(pipe_path)

  if(activate){
    Sys.setenv("RAVE_PIPELINE" = pipe_path)
  }
  return(pipe_path)
}

#' @rdname rave-pipeline
#' @export
pipeline_create_subject_pipeline <- function(
  subject, pipeline_name, overwrite = FALSE,
  activate = TRUE, template_type = c("rmd", 'r')
){
  template_type <- match.arg(template_type)
  pipeline_name <- tolower(pipeline_name)
  stopifnot2(!pipeline_name %in% c("main", "imports", "initialize", "template"),
             msg = "Pipeline name cannot be `main`, `imports`, `template`, or `initialize`")
  subject <- as_rave_subject(subject, strict = FALSE)
  pipe_path <- file.path(subject$pipeline_path, pipeline_name)
  if(!overwrite && dir.exists(pipe_path)){
    stop("Pipeline ", pipeline_name, " already exists at\n  ", pipe_path)
  }
  # create a skeleton template
  template_path <- system.file("rave-pipelines", sprintf("template-%s", template_type), package = 'raveio', mustWork = TRUE)
  fs_src <- list.files(template_path)
  fs_dst <- stringr::str_replace_all(fs_src, "TEMPLATE", pipeline_name)
  dir_create2(pipe_path)
  pipe_path <- normalizePath(pipe_path)
  file.copy(file.path(template_path, fs_src), file.path(pipe_path, fs_dst), overwrite = overwrite, copy.date = TRUE)

  fs <- file.path(pipe_path, fs_dst)
  for(f in fs){
    s <- readLines(f)
    s <- stringr::str_replace_all(s, "TEMPLATE_PATH", pipe_path)
    s <- stringr::str_replace_all(s, "TEMPLATE", pipeline_name)
    s <- stringr::str_replace_all(s, "PROJECT_NAME", subject$project_name)
    s <- stringr::str_replace_all(s, "SUBJECT_CODE", subject$subject_code)
    writeLines(s, f)
  }
  settings <- yaml::read_yaml(file.path(pipe_path, "settings.yaml"))
  if(length(subject$epoch_names)){
    settings$epoch <- subject$epoch_names[[1]]
  } else {
    settings$epoch <- "default"
  }

  if(length(subject$electrodes)){
    settings$electrodes <- dipsaus::deparse_svec(subject$electrodes)
  }

  if(length(subject$reference_names)){
    settings$reference <- subject$reference_names[[1]]
  } else {
    settings$reference <- "default"
  }

  save_yaml(settings, file.path(pipe_path, "settings.yaml"))

  # build the pipeline
  pipeline_build(pipe_path)

  if(activate){
    Sys.setenv("RAVE_PIPELINE" = pipe_path)
  }
  return(pipe_path)
}

#' @rdname rave-pipeline
#' @export
pipeline_description <- function (file) {
  # file <- file.path(pipeline, 'DESCRIPTION')
  dcf <- read.dcf(file = file)
  if (nrow(dcf) < 1L) {
    stop(sprintf("DESCRIPTION file '%s' is corrupt",
                 file), domain = NA)
  }
  desc <- as.list(dcf[1, ])
  if ((length(desc) == 0)) {
    stop(sprintf("DESCRIPTION file '%s' is missing or broken", file), domain = NA)
  }
  attr(desc, "file") <- file

  class(desc) <- "packageDescription"
  desc
}


#' @name pipeline_settings_get_set
#' @title Get or change pipeline input parameter settings
#' @param key,... the character key(s) to get or set
#' @param default the default value is key is missing
#' @param constraint the constraint of the resulting value; if not \code{NULL},
#' then result must be within the \code{constraint} values, otherwise the
#' first element of \code{constraint} will be returned. This is useful to make
#' sure the results stay within given options
#' @param pipeline_settings_path the settings file of the pipeline, must be
#' a 'yaml' file; default is \code{'settings.yaml'} in the current pipeline
#' @return \code{pipeline_settings_set} returns a list of all the settings.
#' \code{pipeline_settings_get} returns the value of given key.
#' @export
pipeline_settings_set <- function(
  ...,
  pipeline_settings_path = file.path(Sys.getenv("RAVE_PIPELINE", "."), "settings.yaml")
){
  if(!file.exists(pipeline_settings_path)){
    stop("Cannot find settings file:\n  ", pipeline_settings_path)
  }
  settings <- load_yaml(pipeline_settings_path)
  args <- list(...)
  dipsaus::list_to_fastmap2(args, map = settings)
  tf <- tempfile()
  on.exit({ unlink(tf) })
  save_yaml(x = settings, file = tf)
  file.copy(from = tf, to = pipeline_settings_path,
            overwrite = TRUE, recursive = FALSE)
  settings
}


`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}

#' @rdname pipeline_settings_get_set
#' @export
pipeline_settings_get <- function(
  key, default = NULL, constraint = NULL,
  pipeline_settings_path = file.path(Sys.getenv("RAVE_PIPELINE", "."), "settings.yaml")) {
  if(!file.exists(pipeline_settings_path)){
    stop("Cannot find settings file:\n  ", pipeline_settings_path)
  }

  settings <- load_yaml(pipeline_settings_path)

  if(missing(key)){ return(settings) }
  if(!settings$`@has`(key)){
    re <- default
  } else {
    re <- settings[[key]]
  }

  if(length(constraint)){
    re <- re %OF% constraint
  }
  re

}




