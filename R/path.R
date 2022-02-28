# path-related functions



#' Simple hard disk speed test
#' @param path an existing directory where to test speed, default is temporary
#' local directory.
#' @param file_size in bytes, default is 1 MB.
#' @param quiet should verbose messages be suppressed?
#' @param abort_if_slow abort test if hard drive is too slow. This usually
#' happens when the hard drive is connected via slow internet: if the write
#' speed is less than 0.1 MB per second.
#' @param use_cache if hard drive speed was tested before, abort testing
#' and return cached results or not; default is false.
#' @return A vector of two: writing and reading speed in MB per seconds.
#' @export
test_hdspeed <- function(path = tempdir(), file_size = 1e6, quiet = FALSE,
                         abort_if_slow = TRUE, use_cache = FALSE){

  last_speed <- raveio_getopt('drive_speed')

  if(!all(last_speed == c(50, 20)) && use_cache){
    return(last_speed)
  }

  if(!dir.exists(path)){
    warning(path, ' does not exist.')
    return(last_speed)
  }

  # create tempdir for testing
  root_dir <- file.path(path, '.rave_hd_test')
  has_root <- dir.exists(root_dir)
  test_dir <- file.path(path, '.rave_hd_test', rand_string(8))
  on.exit({
    if(!has_root){
      unlink(root_dir, recursive = TRUE)
    }else{
      unlink(test_dir, recursive = TRUE)
    }
  })
  dir_create2(test_dir)

  if(abort_if_slow && file_size > 1e5){
    # test write speed. If the speed is too low, should ignore test
    file <- tempfile(tmpdir = test_dir)
    dat <- rand_string(1e5 - 1)
    upload <- system.time(writeLines(dat, file, useBytes = TRUE))
    wsp <- 0.1 / (upload[3])
    if( wsp < 0.1 ) {
      if(!quiet){
        catgl('Hard disk speed might be too slow. Abort speed test')
      }
      sp <- c(wsp, wsp)
      raveio_setopt('drive_speed', sp)
      return(sp)
    }
  }

  progress <- dipsaus::progress2(title = 'Testing read/write speed', max = 2,
                                quiet = quiet, shiny_auto_close = TRUE)

  progress$inc('Write to disk...')

  # generate 10M file, tested
  file <- tempfile(tmpdir = test_dir)
  dat <- rand_string(file_size - 1)
  upload <- system.time(writeLines(dat, file, useBytes = TRUE), gcFirst = TRUE)

  progress$inc('Read from disk...')
  download <- system.time({dat_c <- readLines(file, n = 1)}, gcFirst = TRUE)

  if(exists('dat_c') && dat_c != dat){
    warning('Uploaded data is broken...')
  }

  ratio <- file.info(file)$size / 1000000

  speed <- ratio / c(upload[3], download[3])
  names(speed) <- NULL

  raveio_setopt('drive_speed', speed)

  class(speed) <- 'rave-units'
  attr(speed, 'unit') <- 'MB/s'
  attr(speed, 'labels') <- c('Write - ', 'Read - ')
  return(speed)
}



#' Try to find path along the root directory
#' @description Try to find \code{path} under root directory even
#' if the original path is missing; see examples.
#' @param path file path
#' @param root_dir top directory of the search path
#' @param all return all possible paths, default is false
#' @return The absolute path of file if exists, or \code{NULL} if
#' missing/failed.
#' @details When file is missing, \code{find_path} concatenates the
#' root directory and path combined to find the file. For example,
#' if path is \code{"a/b/c/d"},
#' the function first seek for existence of \code{"a/b/c/d"}. If failed,
#' then \code{"b/c/d"}, and then \code{"~/c/d"} until reaching
#' root directory. If \code{all=TRUE}, then all files/directories found
#' along the search path will be returned
#'
#' @examples
#'
#'
#' root <- tempdir()
#'
#' # ------ Case 1: basic use case -------
#'
#' # Create a path in root
#' dir_create2(file.path(root, 'a'))
#'
#' # find path even it's missing. The search path will be
#' # root/ins/cd/a - missing
#' # root/cd/a     - missing
#' # root/a        - exists!
#' find_path('ins/cd/a', root)
#'
#' # ------ Case 2: priority -------
#' # Create two paths in root
#' dir_create2(file.path(root, 'cc/a'))
#' dir_create2(file.path(root, 'a'))
#'
#' # If two paths exist, return the first path found
#' # root/ins/cd/a - missing
#' # root/cd/a     - exists - returned
#' # root/a        - exists, but ignored
#' find_path('ins/cc/a', root)
#'
#' # ------ Case 3: find all -------
#' # Create two paths in root
#' dir_create2(file.path(root, 'cc/a'))
#' dir_create2(file.path(root, 'a'))
#'
#' # If two paths exist, return the first path found
#' # root/ins/cd/a - missing
#' # root/cd/a     - exists - returned
#' # root/a        - exists - returned
#' find_path('ins/cc/a', root, all = TRUE)
#'
#' @export
find_path <- function(path, root_dir, all = FALSE){
  if(file.exists(path)){
    return(path)
  }
  # root_dir %?<-% rave_options('data_dir')
  path <- unlist(stringr::str_split(path, '(/)|(\\\\)|(\\~)'))
  path <- path[path != '']

  re <- NULL

  for(ii in seq_along(path)){
    tmp_path <- do.call(file.path, as.list(c(root_dir, path[seq.int(ii, length(path))])))
    if(file.exists(tmp_path)){
      re <- c(re, normalizePath(tmp_path))
      if(!all){
        return(re)
      }
    }
  }

  # No path found
  return(re)
}




#' @title Force creating directory with checks
#' @param x path to create
#' @param showWarnings,recursive,... passed to \code{\link{dir.create}}
#' @param check whether to check the directory after creation
#' @return Normalized path
#'
#' @examples
#'
#' path <- file.path(tempfile(), 'a', 'b', 'c')
#'
#' # The following are equivalent
#' dir.create(path, showWarnings = FALSE, recursive = TRUE)
#'
#' dir_create2(path)
#'
#'
#' @export
dir_create2 <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    stop('Cannot create directory at ', shQuote(x))
  }
  invisible(normalizePath(x))
}

