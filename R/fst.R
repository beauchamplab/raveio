
#' @title Read a 'fst' file
#' @param path path to 'fst' file: must not be connection.
#' @param x data frame to write to path
#' @param as.data.table passed to \code{read_fst} in \code{fst} package
#' @param ... passed to \code{read_fst} or \code{\link[fst]{write_fst}}
#' @name read-write-fst
NULL

#' @rdname read-write-fst
#' @export
save_fst <- function(x, path, ...){
  # catgl('Writing to path: {path}')
  dir <- dirname(path)
  if(!dir.exists(dir)){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  ieegio::io_write_fst(x = x, con = path, ...)
  # fst::write_fst(x = x, path = path, ...)
}


#' @rdname read-write-fst
#' @export
load_fst <- function(path, ..., as.data.table = TRUE){
  if( !is.character(as.data.table) ) {
    if( as.data.table ) {
      method <- "data_table"
    } else {
      method <- "data_frame"
    }
  } else {
    method <- as.data.table
  }
  tryCatch({
    ieegio::io_read_fst(con = path, method = method, ...)
    # fst::read_fst(path, ..., as.data.table = as.data.table)
  }, error = function(e){
    stop("FST load failure: ", path)
  })
}


#' Function try to load 'fst' arrays, if not found, read 'HDF5' arrays
#' @param fst_path 'fst' file cache path
#' @param h5_path alternative 'HDF5' file path
#' @param h5_name 'HDF5' data name
#' @param fst_need_transpose does 'fst' data need transpose?
#' @param fst_need_drop drop dimensions
#' @param ram whether to load to memory directly or perform lazy loading
#' @returns If 'fst' cache file exists, returns \code{\link{LazyFST}} object,
#' otherwise returns \code{\link[ieegio]{LazyH5}} instance
#' @details RAVE stores data with redundancy. One electrode data
#' is usually saved with two copies in different formats: 'HDF5' and
#' 'fst', where 'HDF5' is cross-platform and supported by multiple
#' languages such as \code{Matlab}, \code{Python}, etc, while 'fst'
#' format is supported by R only, with super high read/write speed.
#' \code{load_fst_or_h5} checks whether the presence of 'fst' file,
#' if failed, then it reads data from persistent 'HDF5' file.
#' @export
load_fst_or_h5 <- function(
  fst_path, h5_path, h5_name, fst_need_transpose = FALSE,
  fst_need_drop = FALSE, ram = FALSE
){
  ieegio::io_read_fstarray_or_h5(
    fst_path = fst_path,
    h5_path = h5_path,
    h5_name = h5_name,
    fst_need_transpose = fst_need_transpose,
    fst_need_drop = fst_need_drop,
    ram = ram
  )
}

