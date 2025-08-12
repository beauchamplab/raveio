#' @title Read 'Matlab' files
#' @description A compatible reader that can read both 'Matlab'
#' files prior and after version 6.0
#' @param file path to a 'Matlab' file
#' @param ram whether to load data into memory. Only available when
#' the file is in 'HDF5' format. Default is false and will load arrays,
#' if set to true, then lazy-load data. This is useful when array is very large.
#' @param engine method to read the file, choices are \code{'r'} and
#' \code{'py'} ('Python'); if \code{'py'} is chosen, make sure
#' \code{\link[rpymat]{configure_conda}} is configured.
#' @returns A list of All the data stored in the file
#' @details \code{\link[ieegio]{io_read_mat}} can only read 'Matlab' files
#' prior to version 6. After version 6, 'Matlab' uses 'HDF5' format
#' to store its data, and \code{read_mat} can handle both cases.
#'
#' The performance of \code{read_mat} can be limited when
#' the file is too big or has many datasets as it reads all the
#' data contained in 'Matlab' file into memory.
#' @seealso \code{\link[ieegio]{io_read_mat}}, \code{\link{load_h5}}
#'
#' @examples
#'
#' # Matlab .mat <= v7.3
#' x <- matrix(1:16, 4)
#' f <- tempfile()
#' ieegio::io_write_mat(list(x = x), con = f)
#'
#' read_mat(f)
#'
#' # Matlab .mat >= v7.3, using hdf5
#' # Make sure you have installed hdf5r
#' if( dipsaus::package_installed('hdf5r') ){
#'
#' f <- tempfile()
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
#' }
#'
#'
#'
#' @export
read_mat <- function(file, ram = TRUE, engine = c("r", "py")){
  engine <- match.arg(engine)
  call_pkg_fun(package = "ravecore", f_name = "read_mat", file = file, ram = ram, engine = engine)
}

#' @rdname read_mat
#' @export
read_mat2 <- function(file, ram = TRUE, engine = c("r", "py")){
  engine <- match.arg(engine)
  call_pkg_fun(package = "ravecore", f_name = "read_mat2", file = file, ram = ram, engine = engine)
}

