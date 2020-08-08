#' @importFrom dipsaus %?<-%
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @importFrom later later
#' @importFrom rlang quo
#' @importFrom rlang eval_tidy
#' @importFrom rlang is_formula
NULL

#' @export
glue::glue

r6_reserved_fields <- c('.__enclos_env__', 'clone', 'print', 'initialize', 'private')

verbose_levels <-
  factor(
    c("DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", "FATAL"),
    levels = c("DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", "FATAL"),
    ordered = TRUE
  )


#' @title Print colored messages
#' @param ...,.envir passed to \code{\link[glue]{glue}}
#' @param level passed to \code{\link[dipsaus]{cat2}}
#' @param .pal see \code{pal} in \code{\link[dipsaus]{cat2}}
#' @param .capture logical, whether to capture message and return it without
#' printing
#' @return The message as characters
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
  level <- stringr::str_to_upper(level)
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

  if(missing(.pal)){
    dipsaus::cat2(msg, level = level)
  }else{
    dipsaus::cat2(msg, level = level, pal = .pal)
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

