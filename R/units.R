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


#' @name cache_path
#' @title Manipulate cached data on the file systems
#' @param check whether to ensure the cache root path
#' @param quiet whether to suppress the message
#' @return \code{cache_root} returns the root path that stores the 'RAVE'
#' cache data; \code{clear_cached_files} returns nothing
#' @examples
#'
#' cache_root()
#'
#' @export
cache_root <- function(check = FALSE){
  re <- raveio::raveio_getopt(key = 'tensor_temp_path', default = NULL)
  if(!length(re)){
    re <- '~/rave_data/cache_dir/'
    raveio::raveio_setopt(key = 'tensor_temp_path', value = re)
  }
  if(check){
    re <- dir_create2(re)
  }
  re
}

#' @rdname cache_path
#' @export
clear_cached_files <- function(quiet = FALSE){
  dir <- '~/rave_data/cache_dir/'
  if(dir.exists(dir)){
    dir <- normalizePath(dir)
    if(!quiet){
      catgl("Clearing ", dir, level = "DEFAULT")
    }
    unlink(dir, recursive = TRUE)
  }
  dir <- R_user_dir('raveio', "cache")
  if(dir.exists(dir)){
    if(!quiet){
      catgl("Clearing ", dir, level = "DEFAULT")
    }
    unlink(dir, recursive = TRUE)
  }
  dir <- cache_root()
  if(dir.exists(dir)){
    if(!quiet){
      catgl("Clearing ", dir, level = "DEFAULT")
    }
    unlink(dir, recursive = TRUE)
  }
  if(!quiet){
    catgl("Done")
  }
}


symlink_enabled <- local({
  enabled <- NA
  function(cache_ok = TRUE){

    if( using_netdrive() ){ return(FALSE) }

    if(isTRUE(raveio_getopt("enable_symlink"))){
      return(TRUE)
    }

    if(cache_ok && !is.na(enabled)){ return(enabled) }
    tdr <- tempdir(check = TRUE)
    data_dir <- raveio_getopt("data_dir", default = tdr)
    if(!length(data_dir) || !dir.exists(data_dir)){
      data_dir <- tdr
    }
    cache_dir <- cache_root()
    if(!length(cache_dir) || !dir.exists(cache_dir)){
      cache_dir <- tdr
    }
    f1 <- tempfile(pattern = '.raveio_simlink_test_from', tmpdir = data_dir)
    f2 <- tempfile(pattern = '.raveio_simlink_test_to', tmpdir = cache_dir)
    on.exit({
      if(file.exists(f1)){
        unlink(f1)
      }
      if(file.exists(f2)){
        unlink(f2)
      }
    }, add = FALSE)
    s <- paste(sample(LETTERS), collapse = "")
    writeLines(s, con = f1)
    file.symlink(f1, to = f2)
    en <- FALSE
    try({
      if(identical(readLines(f2), s)){
        en <- TRUE
      }
    }, silent = TRUE)
    enabled <<- en

    if(file.exists(f1)){
      unlink(f1)
    }
    if(file.exists(f2)){
      unlink(f2)
    }
    on.exit({}, add = FALSE)

    return(enabled)
  }
})

