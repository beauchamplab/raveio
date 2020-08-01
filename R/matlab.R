#' @title Read 'Matlab' files
#' @description A compatible reader that can read both 'Matlab'
#' files prior and after version 6.0
#' @param file path to a 'Matlab' file
#' @param ram whether to load data into memory. Only available when
#' the file is in 'HDF5' format. Default is false and will load arrays,
#' if set to true, then lazy-load data. This is useful when array is very large.
#' @return A list of All the data stored in the file
#' @details \code{\link[R.matlab]{readMat}} can only read 'Matlab' files
#' prior to version 6. After version 6, 'Matlab' uses 'HDF5' format
#' to store its data, and \code{\link[R.matlab]{readMat}} raises errors.
#' hence \code{H5File} (package \code{hdf5r}) is used to
#' read the file.
#'
#' The performance of \code{read_mat} can be limited when
#' the file is too big or has many datasets as it reads all the
#' data contained in 'Matlab' file into memory.
#' @seealso \code{\link[R.matlab]{readMat}}, \code{\link{load_h5}}
#'
#' @examples
#'
#' # Matlab .mat <= v7.3
#' x <- matrix(1:16, 4)
#' f <- tempfile()
#' R.matlab::writeMat(con = f, x = x)
#'
#' read_mat(f)
#'
#' # Matlab .mat >= v7.3, using hdf5
#' save_h5(x, file = f, name = 'x')
#'
#' read_mat(f)
#'
#' # For v7.3, you don't have to load all data into RAM
#' dat <- read_mat(f, ram = FALSE)
#' dat
#'
#' dat$x[]
#'
#'
#'
#' @export
read_mat <- function(file, ram = TRUE){
  file <- normalizePath(file, mustWork = TRUE)
  # Check if the file is HDF5 format
  if( hdf5r::is_hdf5(file) ){

    f <- hdf5r::H5File$new(filename = file, mode = 'r')
    on.exit(f$close())
    dset_names <- hdf5r::list.datasets(f)
    re <- sapply(dset_names, function(nm){
      r <- load_h5(file, name = nm)
      if(ram){
        r <- r[]
      }
      r
    }, simplify = FALSE, USE.NAMES = TRUE)

  }else{
    re <- R.matlab::readMat(file)
  }
  re
}

