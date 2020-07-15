# path-related functions



#' Simple Hard Disk Speed Test
#' @param path an existing directory where to test speed
#' @param file_size in bytes, default is 10 MB
#' @param quiet should verbose messages be suppressed?
#' @return A vector of two: writing and reading speed in MB per seconds.
#' @export
test_hdspeed <- function(path, file_size = 1e7, quiet = FALSE){

  if(!dir.exists(path)){
    stop(path, ' does not exist.')
    return(c(NA, NA))
  }

  # create tempdir for testing
  test_dir = file.path(path, '.rave_hd_test_', rand_string(8))
  on.exit({
    unlink(test_dir, recursive = TRUE)
  })
  dir_create(test_dir)

  progress = dipsaus::progress2(title = 'Testing read/write speed', max = 2, quiet = quiet)

  progress$inc('Write to disk...')

  # generate 10M file, tested
  file = tempfile(tmpdir = test_dir)
  dat = rand_string(file_size - 1)
  upload = system.time(writeLines(dat, file, useBytes = T))

  progress$inc('Read from disk...')
  download = system.time({dat_c = readLines(file)})

  if(exists('dat_c') && dat_c != dat){
    warning('Uploaded data is broken...')
  }

  ratio = file.info(file)$size / 1000000

  speed = ratio / c(upload[3], download[3])
  names(speed) = NULL
  class(speed) <- 'rave-units'
  attr(speed, 'unit') = 'MB/s'
  attr(speed, 'labels') = c('Write - ', 'Read - ')
  return(speed)
}



#' Try to find path along the root directory
#' @description Try to find \code{path} under root directory even
#' if the original path is missing
#' @param path path to a file. It's fine if the file is missing
#' @param root_dir root directory of the file
#' @return The absolute path of file if exists, or \code{NULL} if
#' missing/failed.
#' @details When file is absent, \code{find_path} concatenates the
#' root directory and path combined to find the file. For example,
#' if the root directory is \code{"~/"}, and path is \code{"a/b/c/d"},
#' the function first seek for existence of \code{"~/a/b/c/d"}. If failed,
#' then \code{"~/b/c/d"}, and then \code{"~/c/d"} until reaching
#' top (root directory).
#'
#' @examples
#' \dontrun{
#' # This example runs when demo (YAB) data are installed
#'
#' # Case 1: path exists from root directory
#' find_path('demo/YAB/rave/meta/electrodes.csv',
#'           root_dir = '~/rave_data/data_dir')
#'
#' # Case 2: path missing from root directory
#' find_path('random/folder/not/exists/demo/YAB/rave/meta/electrodes.csv',
#'           root_dir = '~/rave_data/data_dir')
#'
#' }
#'
#'
#' @export
find_path <- function(path, root_dir){
  if(file.exists(path)){
    return(path)
  }
  # root_dir %?<-% rave_options('data_dir')
  path = unlist(stringr::str_split(path, '(/)|(\\\\)|(\\~)'))
  path = path[path != '']

  for(ii in 1:length(path)){
    tmp_path = do.call(file.path, as.list(c(root_dir, path[ii:length(path)])))
    if(file.exists(tmp_path)){
      return(normalizePath(tmp_path))
    }
  }

  # No path found
  return(NULL)
}




#' @title Force creating directory with checks
#' @param x path to create
#' @param showWarnings,recursive,... passed to \code{\link{dir.create}}
#' @param check whether to check the directory after creation
#' @return Normalized path
#' @export
dir_create <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    stop('Cannot create directory at ', shQuote(x))
  }
  invisible(normalizePath(x))
}

