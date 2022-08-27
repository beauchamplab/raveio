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
#' @param subject_code subject code to remove; default is missing. If
#' \code{subject_code} is provided, then only this subject-related cache
#' files will be removed.
#' @return \code{cache_root} returns the root path that stores the 'RAVE'
#' cache data; \code{clear_cached_files} returns nothing
#' @details 'RAVE' intensively uses cache files. If running on personal
#' computers, the disk space might be filled up very quickly. These cache
#' files are safe to remove if there is no 'RAVE' instance running.
#' Function \code{clear_cached_files} is designed to remove these cache files.
#' To run this function, please make sure that all 'RAVE' instances
#' are shutdown.
#'
#' @examples
#'
#' cache_root()
#'
#' @export
cache_root <- function(check = FALSE){
  re <- raveio_getopt(key = 'tensor_temp_path', default = NULL)
  if(!length(re)){
    re <- '~/rave_data/cache_dir/'
    raveio_setopt(key = 'tensor_temp_path', value = re)
  }
  if(check){
    re <- dir_create2(re)
  }
  re
}

#' @rdname cache_path
#' @export
clear_cached_files <- function(subject_code, quiet = FALSE){

  miss_subject <- missing(subject_code)
  if(miss_subject) {
    clear_dir <- function(dir) {
      if(!dir.exists(dir)) { return() }
      if(!quiet){
        catgl("Clearing ", dir, level = "DEFAULT")
      }
      unlink(dir, recursive = TRUE)
    }
  } else {

    stopifnot2(grepl("^[a-zA-Z0-9_-]{1,}$", subject_code), msg = "clear_cached_files: Invalid subject_code, only letter, digits, _, - are allowed. Subject code cannot be blank as well.")

    clear_dir <- function(dir) {
      if(!dir.exists(dir)) { return() }

      blpath <- file.path(dir, "_baselined_arrays_")
      if(dir.exists(blpath)) {
        if(!quiet){ catgl("Clearing ", blpath, level = "DEFAULT") }
        unlink(blpath, recursive = TRUE)
      }

      rdirs <- list.files(
        path = dir,
        pattern = sprintf("(^|/)%s$", subject_code),
        recursive = TRUE,
        all.files = FALSE,
        include.dirs = TRUE,
        full.names = TRUE,
        no.. = TRUE
      )
      rfiles <- rdirs[dir.exists(rdirs)]
      rdirs <- rdirs[dir.exists(rdirs)]

      if(length(rfiles)) {
        if(!quiet){ catgl("Clearing {length(rfiles)} files with name {subject_code}",
                          level = "DEFAULT") }
        unlink(rfiles)
      }
      if(length(rdirs)) {
        if(!quiet){ catgl("Clearing {length(rdirs)} directories with name {subject_code}",
                          level = "DEFAULT") }
        unlink(rdirs, recursive = TRUE)
      }

    }


  }

  clear_dir('~/rave_data/cache_dir/')
  clear_dir(R_user_dir('raveio', "cache"))
  clear_dir(cache_root())

  # ravetools_path <- file.path(
  #   getOption(
  #     x = "ravetools.tempdir",
  #     default = Sys.getenv(
  #       x ="RAVETOOLS_TEMPDIR",
  #       unset = tempdir(check = FALSE)
  #     )
  #   ),
  #   "ravetools"
  # )
  #
  # if(isTRUE(dir.exists(ravetools_path))) {
  #   if(!quiet){ catgl("Clearing ", ravetools_path, level = "DEFAULT") }
  #   unlink(ravetools_path, recursive = TRUE)
  # }

  if(!quiet){
    catgl("Done", level = "DEFAULT")
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


#' @title Back up and rename the file or directory
#' @param path path to a file or a directory
#' @param remove whether to remove the original path; default is false
#' @param quiet whether not to verbose the messages; default is false
#' @return \code{FALSE} if nothing to back up, or the back-up path
#' if \code{path} exists
#' @examples
#'
#' path <- tempfile()
#' file.create(path)
#'
#' path2 <- backup_file(path, remove = TRUE)
#'
#' file.exists(c(path, path2))
#' unlink(path2)
#'
#'
#' @export
backup_file <- function(path, remove = FALSE, quiet = FALSE) {

  if(length(path) != 1 || is.na(path)) {
    return(invisible(FALSE))
  }
  if(!file.exists(path)){ return(invisible(FALSE)) }

  path <- normalizePath(path, mustWork = TRUE, winslash = "/")

  is_dir <- dir.exists(path)

  # find the extension
  ext <- fileexts(path)

  bname <- basename(path)
  dname <- dirname(path)

  if(ext == '') {
    bname <- gsub("[/]+$", "", bname)
  } else {
    bname <- substr(bname, start = 1L, stop = nchar(bname) - nchar(ext) - 1)
  }

  # check if bname contains timestamp
  bname <- gsub("_\\[backup_[0-9]{8}_[0-9]{6}\\]$", "", x = bname)

  bname2 <- sprintf(
    "%s_[backup_%s]%s",
    bname,
    strftime(Sys.time(), "%Y%m%d_%H%M%S"),
    ifelse(ext == "", "", sprintf(".%s", ext))
  )
  if (!quiet) {
    catgl("{ifelse(remove, 'Moving', 'Copying')} {ifelse(is_dir, 'directory', 'file')} {basename(path)}\n  => {bname2}")
  }
  path2 <- file.path(dname, bname2)

  if( remove ) {
    file.rename(from = path, to = path2)
  } else {
    if(is_dir) {
      dir_create2(path2)
      file.copy(
        from = list.files(
          path = path, all.files = TRUE, full.names = TRUE,
          recursive = FALSE, include.dirs = TRUE, no.. = TRUE
        ),
        to = path2, overwrite = TRUE, recursive = TRUE,
        copy.mode = TRUE, copy.date = TRUE
      )
    } else {
      file.copy(from = path, to = path2, overwrite = TRUE,
                copy.mode = TRUE, copy.date = TRUE, recursive = FALSE)
    }
  }

  return(invisible(path2))

}








