#' @importFrom dipsaus %?<-%
#' @importFrom ravepipeline glue
#' @importFrom R6 R6Class
#' @importFrom filearray filearray_load
#' @importFrom filearray filearray_create
#' @importFrom filearray fmap
#'
#' @importFrom ravetools collapse
#' @importFrom ravetools baseline_array
#'
NULL




RAVEIO_FILEARRAY_VERSION <- 1L

stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    stop(msg)
  }
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
#' \dontrun{
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


`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}
