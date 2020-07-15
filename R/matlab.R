#' @title Read 'Matlab' Files
#' @description A compatible reader that can read both 'Matlab'
#' files prior and after version 6.0
#' @param file path to a 'Matlab' file
#' @return A list of All the data stored in the file
#' @details \code{\link[R.matlab]{readMat}} is used to read files
#' prior to version 6. After version 6, 'Matlab' uses 'HDF5' format
#' to store its data, hence \code{\link[hdf5r]{H5File}} is used to
#' read the file.
#'
#' The performance of \code{read_mat} can be limited when
#' the file is too big or has many datasets as it reads all the
#' data contained in 'Matlab' file into memory.
#' @export
read_mat <- function(file){

  # Check if the file is HDF5 format
  if( hdf5r::is_hdf5(file) ){

    f = hdf5r::H5File$new(filename = file, mode = 'r')
    on.exit(f$close())
    dset_names = hdf5r::list.datasets(f)
    re = sapply(dset_names, function(nm){
      f[[nm]][]
    }, simplify = FALSE, USE.NAMES = TRUE)

  }else{
    re = R.matlab::readMat(file)
  }
  re
}

