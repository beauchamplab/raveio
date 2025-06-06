#' @importFrom dipsaus %?<-%
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @importFrom filearray filearray_load
#' @importFrom filearray filearray_create
#' @importFrom filearray fmap
#' @importFrom promises as.promise
#' @importFrom promises %...>%
#' @importFrom promises %...T>%
#' @importFrom promises %...!%
#' @importFrom promises %...T!%
#' @importFrom checkmate makeAssertCollection
#'
#'
NULL

#' @name raveio-constants
#' @title The constant variables
#' @details
#' \code{SIGNAL_TYPES} has the following options: \code{'LFP'}, \code{'Spike'},
#' \code{'EKG'}, \code{'Auxiliary'}, or \code{'Unknown'}. As
#' of 'raveio' \code{0.0.6}, only \code{'LFP'} (see \code{\link{LFP_electrode}})
#' signal type is supported.
#'
#'
#' \code{LOCATION_TYPES} is a list of the electrode location types:
#' \code{'iEEG'} (this includes the next two), \code{'sEEG'} (stereo),
#' \code{'ECoG'} (surface), \code{'EEG'} (scalp),
#' \code{'Others'}. See field \code{'location'} in
#' \code{\link{RAVEAbstarctElectrode}}
#'
#' \code{MNI305_to_MNI152} is a 4-by-4 matrix converting \code{'MNI305'}
#' coordinates to \code{'MNI152'} space. The difference of these two
#' spaces is: \code{'MNI305'} is an average of 305 human subjects,
#' while \code{'MNI152'} is the average of 152 people. These two coordinates
#' differs slightly. While most of the 'MNI' coordinates reported by
#' 'RAVE' and 'FreeSurfer' are in the \code{'MNI305'} space, many other
#' programs are expecting \code{'MNI152'} coordinates.
#'
#' @export
SIGNAL_TYPES <- c('LFP', 'Spike', 'EKG', 'Auxiliary', 'Unknown')

#' @rdname raveio-constants
#' @export
LOCATION_TYPES <- c('iEEG', 'sEEG', 'ECoG', 'EEG', 'Others')

#' @rdname raveio-constants
#' @export
MNI305_to_MNI152 <- matrix(
  c(0.9975, 0.0146, -0.013, 0,
    -0.0073, 1.0009, -0.0093, 0,
    0.0176, -0.0024, 0.9971, 0,
    -0.0429, 1.5496, 1.184, 1),
  nrow = 4L, byrow = FALSE
)

#' @rdname raveio-constants
#' @export
YAEL_IMAGE_TYPES <- c(
  # Pre-op
  "T1w", "T2w", "FLAIR", "preopCT", "T1wContrast", "fGATIR",
  "postopT1w", "postopT2w", "postopFLAIR", "CT"
)

HDF5_EAGERLOAD <- TRUE

RAVEIO_FILEARRAY_VERSION <- 1L

#' @export
glue::glue

#' @export
promises::`%...>%`

#' @export
promises::`%...T>%`

#' @export
promises::`%...!%`

#' @export
promises::`%...T!%`

r6_reserved_fields <- c('.__enclos_env__', 'clone', 'print', 'initialize', 'private')

verbose_levels <-
  factor(
    c("DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", "FATAL"),
    levels = c("DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", "FATAL"),
    ordered = TRUE
  )

str2lang_alt <- function (s) {
  s <- sprintf("quote(%s)", trimws(s))
  eval(parse(text = s))
}

str2lang <- function (s) {
  get0("str2lang", envir = baseenv(), ifnotfound = str2lang_alt)(s)
}


byte_size_lut <- list(
  "uint8" = 1, "int8" = 1,
  "uint16" = 2, "int16" = 2,
  "uint32" = 4, "int32" = 4,
  "uint64" = 8, "int64" = 8,
  "float" = 4, "double" = 8
)


#' @title Check if current session is on 'CRAN'
#' @description
#' Use this function only for examples and test. The goal is to comply with the
#' 'CRAN' policy. Do not use it in normal functions to cheat. Violating 'CRAN'
#' policy will introduce instability to your code. Make sure reading Section
#' 'Details' before using this function.
#' @param if_interactive whether interactive session will be considered as
#' on 'CRAN'; default is \code{FALSE}
#' @param verbose whether to print out reason of return; default is no
#' @details
#' According to 'CRAN' policy, package examples and test functions may only
#' use maximum 2 'CPU' cores. Examples running too long should be suppressed.
#' Normally package developers will use \code{interactive()} to avoid running
#' examples or parallel code on 'CRAN'. However, when checked locally, these
#' examples will be skipped too. Coding bug in those examples will not be
#' reported.
#'
#' The objective is to allow 'RAVE' package developers to write and test
#' examples locally or on integrated development environment (such as
#' 'Github'), while suppressing them on 'CRAN'. In such way, bugs in the
#' examples will be revealed and fixed promptly.
#'
#' Do not use this function inside of the package functions to cheat or slip
#' illegal code under the eyes of 'CRAN' folks. This will increase their work
#' load and introduce instability to your code. Also this function is subject
#' to deletion in the future.
#'
#' @returns A logical whether current environment should be considered as on
#' 'CRAN'.
#' @export
is_on_cran <- function(if_interactive = FALSE, verbose = FALSE) {

  # check if this is on CRAN
  not_cran_flag <- identical(toupper(as.character(Sys.getenv("NOT_CRAN", ""))), "TRUE")
  limit_core_flag <- identical(toupper(Sys.getenv("_R_CHECK_LIMIT_CORES_")), "TRUE")
  shiny_flag <- dipsaus::shiny_is_running()
  interactive_flag <- interactive()

  if( limit_core_flag ) {
    # CRAN checks will add _R_CHECK_LIMIT_CORES_ to environment
    if( verbose ) {
      message("_R_CHECK_LIMIT_CORES_ is TRUE/true (on CRAN)")
    }
    return(TRUE)
  }

  if ( not_cran_flag ) {
    # testthat on local uses this flag to indicate not on CRAN
    message("NOT_CRAN is TRUE/true (not on CRAN)")
    return(FALSE)
  }

  if( shiny_flag ) {
    # Shiny is enabled, cannot be on CRAN
    message("Inside of shiny reactive context (not on CRAN)")
    return(FALSE)
  }

  if ( interactive_flag ) {
    if_interactive <- isTRUE(if_interactive)
    message(sprintf("Session is interactive (%son CRAN)", ifelse(if_interactive, "", "not ")))
    return(if_interactive)

  }

  message("No flag detected, default is on CRAN")
  return( TRUE )
}

#' @title Print colored messages
#' @param ...,.envir passed to \code{\link[glue]{glue}}
#' @param level passed to \code{\link[dipsaus]{cat2}}
#' @param .pal see \code{pal} in \code{\link[dipsaus]{cat2}}
#' @param .capture logical, whether to capture message and return it without
#' printing
#' @returns The message as characters
#' @details The level has order that sorted from low to high: \code{"DEBUG"},
#' \code{"DEFAULT"}, \code{"INFO"}, \code{"WARNING"}, \code{"ERROR"},
#' \code{"FATAL"}. Each different level will display different colors and
#' icons before the message. You can suppress messages with certain levels
#' by setting 'raveio' options via
#' \code{raveio_setopt('verbose_level', <level>)}. Messages with levels lower
#' than the threshold will be muffled. See examples.
#'
#' @examples
#'
#' # ------------------ Basic Styles ---------------------
#'
#' # Temporarily change verbose level for example
#' raveio_setopt('verbose_level', 'DEBUG', FALSE)
#'
#' # debug
#' catgl('Debug message', level = 'DEBUG')
#'
#' # default
#' catgl('Default message', level = 'DEFAULT')
#'
#' # info
#' catgl('Info message', level = 'INFO')
#'
#' # warning
#' catgl('Warning message', level = 'WARNING')
#'
#' # error
#' catgl('Error message', level = 'ERROR')
#'
#' try({
#'   # fatal, will call stop and raise error
#'   catgl('Error message', level = 'FATAL')
#' }, silent = TRUE)
#'
#' # ------------------ Muffle messages ---------------------
#'
#' # Temporarily change verbose level to 'WARNING'
#' raveio_setopt('verbose_level', 'WARNING', FALSE)
#'
#' # default, muffled
#' catgl('Default message')
#'
#' # message printed for level >= Warning
#' catgl('Default message', level = 'WARNING')
#' catgl('Default message', level = 'ERROR')
#'
#'
#'
#' @export
catgl <- function(..., .envir = parent.frame(), level = 'DEBUG', .pal, .capture = FALSE){
  level <- toupper(level)
  opt_level <- raveio_getopt('verbose_level')
  args <- list(...)
  msg <- tryCatch({
    structure(glue::glue(..., .envir = .envir), log_level = level)
  }, error = function(...){
    s <- args
    if(length(names(s))){
      s <- s[names(s) %in% c('', 'sep', 'collapse')]
    }
    s[[length(s) + 1]] <- ''

    do.call('paste', s)
  })
  if(
    .capture || (
      sum(verbose_levels >= opt_level, na.rm = TRUE) <
      sum(verbose_levels >= level, na.rm = TRUE)
    )
  ) {
    # opt_level is too high, message is muffled. depending on level
    # return or stop
    if(level == 'FATAL'){
      stop(msg)
    }
    return(invisible(msg))
  }
  call <- match.call()
  call <- deparse1(call, collapse = '\n')

  # .envir = parent.frame(), level = 'DEBUG', .pal, .capture = FALSE
  if(dipsaus::package_installed('ravedash')){
    ns <- do.call('asNamespace', list('ravedash'))
    ns$logger(msg, level = switch (
      level,
      "DEFAULT" = "trace",
      "DEBUG" = "debug",
      "INFO" = "info",
      "WARNING" = "warning",
      'ERROR' = 'error',
      'FATAL' = 'fatal',
      { "trace" }
    ))
    if(level == 'FATAL') {
      stop(msg)
    }
  } else {
    if(missing(.pal)){
      dipsaus::cat2(msg, level = level)
    }else{
      dipsaus::cat2(msg, level = level, pal = .pal)
    }
  }

  return(invisible(msg))
}



stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    stop(msg)
  }
}

append_el <- function(el, value, method = 'c'){
  el_expr <- substitute(el)
  value <- do.call(method, list(quote(el), quote(value)))
  do.call('<-', list(el_expr, value), envir = parent.frame())
}

# These functions are available in R 4.0. However, to be backward compatible
deparse1 <- function(expr, collapse = ' '){
  paste(deparse(expr), collapse = collapse)
}

R_user_dir <- function (package, which = c("data", "config", "cache")) {
  stopifnot(is.character(package), length(package) == 1L)
  which <- match.arg(which)
  home <- normalizePath("~")
  path <- switch(which, data = {
    if (nzchar(p <- Sys.getenv("R_USER_DATA_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_DATA_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("APPDATA"), "R", "data")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Application Support", "org.R-project.R")
    else file.path(home, ".local", "share")
  }, config = {
    if (nzchar(p <- Sys.getenv("R_USER_CONFIG_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_CONFIG_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("APPDATA"), "R", "config")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Preferences", "org.R-project.R")
    else file.path(home, ".config")
  }, cache = {
    if (nzchar(p <- Sys.getenv("R_USER_CACHE_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_CACHE_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
    else if (Sys.info()["sysname"] == "Darwin") file.path(home, "Library", "Caches", "org.R-project.R")
    else file.path(home, ".cache")
  })
  file.path(path, "R", package)
}

# These functions uses ravetools/dipsaus
collapse <- function(x, keep, average = FALSE, ...) {
  if(isTRUE(getOption("raveio.use.ravetools", FALSE))) {
    return(ravetools::collapse(x = x, keep = keep, average = average, ...))
  } else {
    return(dipsaus::collapse(x = x, keep = keep, average = average))
  }
}

baseline_array <- function(
    x, along_dim, baseline_indexpoints = NULL, unit_dims = seq_along(dim(x))[-along_dim],
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore", "subtract_mean"),
    ...) {
  method <- match.arg(method)
  if(isTRUE(getOption("raveio.use.ravetools", FALSE))) {
    return(ravetools::baseline_array(
      x = x, along_dim = along_dim, unit_dims = unit_dims,
      method = method, baseline_indexpoints = baseline_indexpoints, ...))
  } else {
    return(dipsaus::baseline_array(
      x = x, along_dim = along_dim, unit_dims = unit_dims,
      method = method, baseline_indexpoints = baseline_indexpoints))
  }
}

#' Enable parallel computing provided by 'future' package within the context
#' @param expr the expression to be evaluated
#' @param env environment of the \code{expr}
#' @param quoted whether \code{expr} has been quoted; default is false
#' @param on_failure alternative 'future' plan to use if forking a process
#' is disallowed; this usually occurs on 'Windows' machines; see details.
#' @param max_workers maximum of workers; default is automatically set by
#' \code{raveio_getopt("max_worker",1L)}
#' @param ... additional parameters passing into
#' \code{\link[dipsaus]{make_forked_clusters}}
#' @returns The evaluation results of \code{expr}
#' @details Some 'RAVE' functions such as \code{\link{prepare_subject_power}}
#' support parallel computing to speed up. However, the parallel computing is
#' optional. You can enable it by wrapping the function calls within
#' \code{with_future_parallel} (see examples).
#'
#' The default plan is to use 'forked' R sessions. This is a convenient, fast,
#' and relative simple way to create multiple R processes that share the same
#' memories. However, on some machines such as 'Windows' the support has not
#' yet been implemented. In such cases, the plan fall backs to a back-up
#' specified by \code{on_failure}. By default, \code{on_failure} is
#' \code{'multisession'}, a heavier implementation than forking the process, and
#' slightly longer ramp-up time.
#' However, the difference should be marginal for most of the functions.
#'
#' When parallel computing is enabled, the number of parallel workers is
#' specified by the option \code{raveio_getopt("max_worker", 1L)}.
#' @examples
#'
#' library(raveio)
#'
#' demo_subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)
#'
#' if(dir.exists(demo_subject$path)) {
#'   with_future_parallel({
#'     prepare_subject_power("demo/DemoSubject")
#'   })
#' }
#'
#' @export
with_future_parallel <- function(expr, env = parent.frame(), quoted = FALSE,
                                 on_failure = 'multisession', max_workers = NA,
                                 ...){
  if(!quoted){
    expr <- substitute(expr)
  }
  if(!is.na(max_workers) && max_workers >= 1){
    max_workers <- min(as.integer(max_workers), raveio_getopt("max_worker", 1L))
  } else {
    max_workers <- raveio_getopt("max_worker", 1L)
  }
  auto_parallel_old <- getOption("raveio.auto.parallel", default = TRUE)
  options("raveio.auto.parallel" = FALSE)
  dipsaus::make_forked_clusters(
    workers = max_workers,
    on_failure = on_failure, clean = FALSE, ...
  )
  on.exit({
    asNamespace("future")$plan("sequential")
    options("raveio.auto.parallel" = auto_parallel_old)
  }, add = TRUE, after = FALSE)

  re <- eval(expr, envir = env)
  asNamespace("future")$plan("sequential")
  options("raveio.auto.parallel" = auto_parallel_old)

  re
}

#' Run \code{\link{lapply}} in parallel
#' @description
#' Uses \code{\link[dipsaus]{lapply_async2}}, but allows better parallel
#' scheduling via \code{\link{with_future_parallel}}. On 'Unix', the function
#' will fork processes. On 'Windows', the function uses strategies specified
#' by \code{on_failure}
#'
#' @param x iterative elements
#' @param FUN function to apply to each element of \code{x}
#' @param FUN.args named list that will be passed to \code{FUN} as arguments
#' @param callback callback function or \code{NULL}. When passed as function,
#' the function takes one argument (elements of \code{x}) as input, and it
#' suppose to return one string character.
#' @param ncores number of cores to use, constraint by the \code{max_worker}
#' option (see \code{\link{raveio_getopt}}); default is the maximum number
#' of workers available
#' @param on_failure alternative strategy if fork process is
#' disallowed (set by users or on 'Windows')
#' @param ... passed to \code{\link[dipsaus]{lapply_async2}}
#'
#' @examples
#'
#'
#' if(!is_on_cran()) {
#' library(raveio)
#'
#' # ---- Basic example ----------------------------
#' lapply_async(1:16, function(x) {
#'   # function that takes long to fun
#'   Sys.sleep(1)
#'   x
#' })
#'
#' # With callback
#' lapply_async(1:16, function(x){
#'   Sys.sleep(1)
#'   x + 1
#' }, callback = function(x) {
#'   sprintf("Calculating|%s", x)
#' })
#'
#' # With ncores
#' pids <- lapply_async(1:16, function(x){
#'   Sys.sleep(0.5)
#'   Sys.getpid()
#' }, ncores = 2)
#'
#' # Unique number of PIDs (cores)
#' unique(unlist(pids))
#'
#' # ---- With scheduler ----------------------------
#' # Scheduler pre-initialize parallel workers and temporary
#' # switches parallel context. The workers ramp-up
#' # time can be saved by reusing the workers.
#' #
#' with_future_parallel({
#'
#'   # lapply_async block 1
#'   pids <- lapply_async(1:16, function(x){
#'     Sys.sleep(1)
#'     Sys.getpid()
#'   }, callback = function(x) {
#'     sprintf("lapply_async without ncores|%s", x)
#'   })
#'   print(unique(unlist(pids)))
#'
#'   # lapply_async block 2
#'   pids <- lapply_async(1:16, function(x){
#'     Sys.sleep(1)
#'     Sys.getpid()
#'   }, callback = function(x) {
#'     sprintf("lapply_async with ncores|%s", x)
#'   }, ncores = 4)
#'   print(unique(unlist(pids)))
#'
#' })
#'
#'
#' }
#'
#'
#' @export
lapply_async <- function(
    x, FUN, FUN.args = list(), callback = NULL, ncores = NULL,
    on_failure = "multisession", ...) {

  future <- asNamespace("future")
  dipsaus <- asNamespace("dipsaus")

  # Determine the number of cores
  # get max number of cores
  max_workers <- as.integer(raveio_getopt("max_worker", 1L))
  if(!isTRUE(is.finite(max_workers)) || max_workers < 1L) {
    # correct `max_workers` is not correctly set
    max_workers <- 1L
  }
  if(length(ncores) == 1) {
    # user specified number of cores
    ncores <- as.integer(ncores)
    if(isTRUE(ncores > 0) && isTRUE(ncores < max_workers)) {
      max_workers <- ncores
    }
  }
  ncores <- max_workers
  if(ncores > length(x)) {
    ncores <- max(length(x), 1L)
  }


  get_globals_size <- dipsaus$get_globals_size
  if( ncores > 1 && is.function(get_globals_size)) {

    # check only when parallel computing is needed
    globals_size <- get_globals_size(FUN)

    if(length(globals_size) == 1 && is.numeric(globals_size) && is.finite(globals_size)) {
      # We expect a globals_size to at least double in the child nodes
      child_mem <- ceiling(globals_size * 4)

      # determine the max number of cores so make sure the memory is not exhausted
      max_mem <- raveio_getopt("max_mem", default = 8) * 1024^3

      ncores_changed <- FALSE
      max_size_changed <- FALSE

      tryCatch({
        max_ncores <- max(floor(max_mem / child_mem), 1L, na.rm = TRUE)
        if( max_ncores < ncores ) {
          ncores_changed <- TRUE
          ncores <- max_ncores
        }
      }, error = function(e) {})

      if( ncores > 1 ) {

        # check if option `future.globals.maxSize` is too low
        # 500 MB default max globals
        max_size <- getOption("future.globals.maxSize", default = 524288000)
        if(isTRUE(child_mem >= max(max_size, 1)) && is.finite(child_mem)) {
          max_size_changed <- TRUE
          options(future.globals.maxSize = child_mem)
        }
      }

      if( ncores_changed || max_size_changed ) {
        msg <- "Large variables detected. Temporarily"
        if( ncores_changed ) {
          msg <- c(msg, sprintf("reduced parallel cores to %.0f", ncores))
          if( max_size_changed ) {
            msg <- c(msg, "and")
          }
        }
        if( max_size_changed ) {
          msg <- c(msg, sprintf("raised limit for globals to %s", dipsaus::to_ram_size(child_mem)))
        }
        message(paste(msg, collapse = " "))
      }
    }

  }

  restore_future <- FALSE
  auto_parallel <- getOption("raveio.auto.parallel", default = TRUE)
  current_plan <- "sequential"

  on.exit({
    if(restore_future) {
      future$plan(current_plan, .skip = TRUE)
    }
  })

  if(isTRUE(auto_parallel)) {
    if( ncores == 1 ) {
      future$plan("sequential")
    } else {
      # check if fork is disabled
      dipsaus::make_forked_clusters(
        workers = ncores,
        on_failure = on_failure, clean = FALSE, ...
      )
      restore_future <- TRUE
    }
  } else {
    restore_future <- FALSE
    current_plan <- future$plan("list")
  }

  # print(ncores)
  chunk_size <- ceiling(length(x) / ncores)
  re <- dipsaus::lapply_async2(x, FUN = FUN, FUN.args = FUN.args, callback = callback,
                               plan = FALSE, future.chunk.size = chunk_size)

  if( restore_future ) {
    future$plan(current_plan)
    restore_future <- FALSE
  }

  re
}


#' @export
print.raveio_digest_expression <- function(x, ..., max_nvars = 5) {
  vnames <- names(attr(x, "global_vars"))

  if(length(vnames) > max_nvars) {
    vnames <- c(vnames[seq_len(max_nvars)], "...")
  }
  cat(sprintf("<Digest from code + variables>\n  variable names: %s\n  MD5: %s\n",
              paste(vnames, collapse = ", "), x))
  invisible(x)
}

#' @export
format.raveio_digest_expression <- function(x, ...) {
  attributes(x) <- NULL
  x
}


key_missing <- function () {
  structure(list(), class = "key_missing")
}

`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}
