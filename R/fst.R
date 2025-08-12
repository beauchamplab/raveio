
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


