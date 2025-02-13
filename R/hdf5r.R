
h5FileValid <- function(filename){
  if(!length(filename)){ return(FALSE) }
  filename <- filename[[1]]
  if(!file.exists(filename)){ return(FALSE) }
  if(isTRUE(file.info(filename)[['isdir']])){ return(FALSE) }
  filename <- normalizePath(filename)
  return(ieegio::io_h5_valid(file = filename))
}


#' Lazy Load 'HDF5' File via \code{\link[ieegio]{io_read_h5}}
#'
#' @description Wrapper for class \code{\link[ieegio]{LazyH5}}, which load data
#' with "lazy" mode - only read part of dataset when needed.
#'
#' @param file 'HDF5' file
#' @param name \code{group/data_name} path to dataset (\code{H5D} data)
#' @param read_only only used if \code{ram=FALSE}, whether the returned
#' \code{\link[ieegio]{LazyH5}} instance should be read only
#' @param ram load data to memory immediately, default is false
#' @param quiet whether to suppress messages
#'
#' @returns If \code{ram} is true, then return data as arrays, otherwise return
#' a \code{\link[ieegio]{LazyH5}} instance.
#'
#' @seealso \code{\link{save_h5}}
#'
#' @examples
#' file <- tempfile()
#' x <- array(1:120, dim = c(4,5,6))
#'
#' # save x to file with name /group/dataset/1
#' save_h5(x, file, '/group/dataset/1', quiet = TRUE)
#'
#' # read data
#' y <- load_h5(file, '/group/dataset/1', ram = TRUE)
#' class(y)   # array
#'
#' z <- load_h5(file, '/group/dataset/1', ram = FALSE)
#' class(z)   # LazyH5
#'
#' dim(z)
#'
#' @export
load_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE){
  return(ieegio::io_read_h5(
    file = file,
    name = name,
    read_only = read_only,
    ram = ram,
    quiet = quiet
  ))
}




#' Save objects to 'HDF5' file without trivial checks
#' @param x an array, a matrix, or a vector
#' @param file path to 'HDF5' file
#' @param name path/name of the data; for example, \code{"group/data_name"}
#' @param chunk chunk size
#' @param level compress level from 0 - no compression to 10 - max compression
#' @param replace should data be replaced if exists
#' @param new_file should removing the file if old one exists
#' @param ctype data type such as "character", "integer", or "numeric". If
#' set to \code{NULL} then automatically detect types. Note for complex data
#' please store separately the real and imaginary parts.
#' @param quiet whether to suppress messages, default is false
#' @param ... passed to other \code{LazyH5$save}
#' @returns Absolute path of the file saved
#'
#' @seealso \code{\link{load_h5}}
#' @examples
#'
#' file <- tempfile()
#' x <- array(1:120, dim = 2:5)
#'
#' # save x to file with name /group/dataset/1
#' save_h5(x, file, '/group/dataset/1', chunk = dim(x))
#'
#' # read data
#' y <- load_h5(file, '/group/dataset/1')
#' y[]
#' @export
save_h5 <- function(x, file, name, chunk = 'auto', level = 4, replace = TRUE,
                    new_file = FALSE, ctype = NULL, quiet = FALSE, ...){
  ieegio::io_write_h5(
    x = x,
    file = file,
    name = name,
    chunk = chunk,
    level = level,
    replace = replace,
    new_file = new_file,
    ctype = ctype,
    quiet = quiet,
    ...
  )
}


#' Check whether a 'HDF5' file can be opened for read/write
#' @param file path to file
#' @param mode \code{'r'} for read access and \code{'w'} for write access
#' @param close_all whether to close all connections or just close current
#' connection; default is false. Set this to \code{TRUE} if you want to
#' close all other connections to the file
#' @returns logical whether the file can be opened.
#'
#' @examples
#'
#' x <- array(1:27, c(3,3,3))
#' f <- tempfile()
#'
#' # No data written to the file, hence invalid
#' h5_valid(f, 'r')
#'
#' save_h5(x, f, 'dset')
#' h5_valid(f, 'w')  # TRUE
#' h5_valid(f, 'r')  # TRUE
#'
#' @export
h5_valid <- function(file, mode = c('r', 'w'), close_all = FALSE){
  mode <- match.arg(mode)
  return(ieegio::io_h5_valid(file = file, mode = mode, close_all = close_all))
}


#' Returns all names contained in 'HDF5' file
#' @param file, 'HDF5' file path
#' @returns characters, data set names
#' @export
h5_names <- function(file){
  return(ieegio::io_h5_names(file))
}
