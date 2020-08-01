# units

#' Convert numeric number into print-friendly format
#' @param x numeric or numeric vector
#' @param unit the unit of \code{x}
#' @param label prefix when printing \code{x}
#' @return Still numeric, but print-friendly class
#' @examples
#'
#' sp <- as_rave_unit(1024, 'GB', 'Hard-disk space is ')
#' print(sp, digits = 0)
#'
#' sp - 12
#'
#' as.character(sp)
#'
#' as.numeric(sp)
#'
#' # Vectorize
#' sp <- as_rave_unit(c(500,200), 'MB/s', c('Writing: ', 'Reading: '))
#' print(sp, digits = 0, collapse = '\n')
#'
#' @export
as_rave_unit <- function(x, unit, label = ''){
  structure(x, unit = unit, labels = label, class = 'rave-units')
}

#' @export
`as.character.rave-units` <- function(x, digits = 2, collapse = ', ', ...){
  fmt <- sprintf('%%.%df %%s', digits)
  unit <- attr(x, 'unit')[[1]]
  if(!length(unit)){
    unit <- ''
  }
  paste(attr(x, 'labels'), sprintf(fmt, x, unit), collapse = collapse, sep = '', ...)
}

#' @export
`print.rave-units` <- function(x, digits = 2, collapse = ', ', ...){
  cat(as.character(x, digits = digits, collapse = collapse), ...)
  invisible(x)
}


#' Calculate time difference in seconds
#' @param start,end start and end of timer
#' @param units passed to \code{\link[dipsaus]{time_delta}}
#' @param label \code{rave-units} label for display purpose.
#' @return A number inherits \code{rave-units} class.
#' @seealso \code{\link{as_rave_unit}}
#' @examples
#' start <- Sys.time()
#' Sys.sleep(0.1)
#' end <- Sys.time()
#' dif <- time_diff2(start, end, label = 'Running ')
#' print(dif, digits = 4)
#'
#' is.numeric(dif)
#'
#' dif + 1
#'
#' @export
time_diff2 <- function(start, end, units = 'secs', label = ''){
  delta <- dipsaus::time_delta(start, end, units = units)
  structure(delta, unit = units, labels = label, class = 'rave-units')
}

