#' Enhanced progress with logger message
#' @description For best performance, please install \code{'ravedash'}. This
#' function can replace \code{\link[dipsaus]{progress2}}.
#' @param title,max,...,quiet,session,shiny_auto_close see
#' \code{\link[dipsaus]{progress2}}
#' @param outputId will be used if package \code{'shidashi'} is installed,
#' otherwise will be ignored
#' @param log function, \code{NULL}, or missing; default is missing, which
#' will use \code{logger} function in the package \code{'ravedash'}, or
#' \code{\link[dipsaus]{cat2}} if \code{'ravedash'} is not installed. If
#' \code{log=NULL}, then the message will be suppressed in 'shiny' applications.
#' If a function provided, then the function will be called.
#' @return A list, see \code{\link[dipsaus]{progress2}}
#'
#' @export
progress_with_logger <- function (
  title, max = 1, ..., quiet = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  shiny_auto_close = FALSE, outputId = NULL, log)
{
  initialized <- dipsaus::shiny_is_running()
  if(missing(log)){
    if(dipsaus::package_installed('ravedash')){
      ravedash <- do.call('asNamespace', list('ravedash'))
      log <- function(...){
        ravedash$logger(..., level = 'trace', use_glue = FALSE, reset_timer = !initialized, calc_delta = TRUE)
        if(!initialized){
          initialized <<- TRUE
        }
      }
    } else {
      log <- function(...){
        dipsaus::cat2(..., level = "DEFAULT")
      }
    }
  }
  # If shidashi is installed, then use it, but
  if(dipsaus::package_installed('shidashi')){
    ns <- do.call('asNamespace', list('shidashi'))
    ns$shiny_progress(title = title, max = max, ..., quiet = quiet,
                      session = session, shiny_auto_close = shiny_auto_close,
                      log = log, outputId = outputId)
  } else {
    dipsaus::progress2(title = title, max = max, ..., quiet = quiet,
                       session = session, shiny_auto_close = shiny_auto_close,
                       log = log)
  }
}
