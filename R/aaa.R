#' @importFrom dipsaus %?<-%
#' @importFrom glue glue
NULL

#' @export
glue::glue


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
catgl <- function(..., .envir = parent.frame(), level = 'DEBUG', .pal){
  level <- stringr::str_to_upper(level)
  opt_level <- raveio_getopt('verbose_level')
  if(
    sum(verbose_levels >= opt_level, na.rm = TRUE) <
    sum(verbose_levels >= level, na.rm = TRUE)
  ) {
    # opt_level is too high, message is muffled
    return(invisible())
  }
  call <- match.call()
  call <- deparse1(call, collapse = '\n')

  tryCatch({
    if(missing(.pal)){
      dipsaus::cat2(glue::glue(..., .envir = .envir), level = level)
    }else{
      dipsaus::cat2(glue::glue(..., .envir = .envir), level = level, pal = .pal)
    }
  }, error = function(e){
    warning('Error found while printing the following message:\n',
            call)
    # stop(e, call. = FALSE)
  })
  return(invisible())
}


# deparse1 <- function(expr, collapse = ' '){
#   paste(deparse(expr), collapse = collapse)
# }


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



