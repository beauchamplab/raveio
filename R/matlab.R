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
  file <- normalizePath(file, mustWork = TRUE, winslash = "/")
  # Check if the file is HDF5 format

  if(engine == "r") {
    if( h5FileValid(file) ){

      dset_names <- h5_names(file)
      re <- sapply(dset_names, function(nm){
        y <- load_h5(file, name = nm, ram = ram)
        y
      }, simplify = FALSE, USE.NAMES = TRUE)
    }else{
      # re <- R.matlab::readMat(file)
      re <- ieegio::io_read_mat(con = file, method = "R.matlab")
    }
  } else {
    # use python
    rpymat::ensure_rpymat(verbose = FALSE, cache = TRUE)
    mat73 <- tryCatch({
      rpymat::import("mat73", convert = FALSE)
    }, error = function(e) {
      rpymat::add_packages("mat73")
      rpymat::import("mat73", convert = FALSE)
    })
    sio <- tryCatch({
      rpymat::import("scipy.io", convert = FALSE)
    }, error = function(e) {
      rpymat::add_packages("scipy")
      rpymat::import("scipy.io", convert = FALSE)
    })

    dat <- tryCatch({
      sio$loadmat(file_name = file)
    }, error = function(e) {
      mat73$loadmat(filename = file)
    })
    re <- dipsaus::fastmap2()
    for(nm in names(dat)) {
      re[[nm]] <- rpymat::py_to_r(dat[[nm]])
    }
  }

  re
}

#' @rdname read_mat
#' @export
read_mat2 <- function(file, ram = TRUE, engine = c("r", "py")){
  engine <- match.arg(engine)
  file <- normalizePath(file, mustWork = TRUE, winslash = "/")
  # Check if the file is HDF5 format

  if(engine == "r") {
    if( h5FileValid(file) ){

      dset_names <- h5_names(file)
      re <- dipsaus::fastmap2()
      lapply(dset_names, function(nm){
        y <- load_h5(file, name = nm, ram = ram)
        nm_path <- strsplit(nm, "/")[[1]]
        d <- re
        for(ii in seq_along(nm_path)){
          nm <- nm_path[[ii]]
          if(ii != length(nm_path)){
            if(!inherits(d[[nm]], 'fastmap2')){
              d[[nm]] <- dipsaus::fastmap2()
            }
            d <- d[[nm]]
          } else {
            d[[nm]] <- y
          }
        }
        NULL
      })
    }else{
      # re <- dipsaus::list_to_fastmap2(R.matlab::readMat(file))
      re <- dipsaus::list_to_fastmap2(
        ieegio::io_read_mat(con = file, method = "R.matlab")
      )
    }
  } else {
    # use python
    rpymat::ensure_rpymat(verbose = FALSE, cache = TRUE)

    sio <- tryCatch({
      rpymat::import("scipy.io", convert = FALSE)
    }, error = function(e) {
      rpymat::add_packages("scipy")
      rpymat::import("scipy.io", convert = FALSE)
    })

    dat <- tryCatch({
      sio$loadmat(file)
    }, error = function(e) {
      mat73 <- tryCatch({
        rpymat::import("mat73", convert = FALSE)
      }, error = function(e) {
        rpymat::add_packages("mat73")
        rpymat::import("mat73", convert = FALSE)
      })
      mat73$loadmat(file)
    })
    re <- dipsaus::fastmap2()

    iterate <- function(x, prefix = "") {
      if(!inherits(x, "python.builtin.dict")) {
        re[[ gsub("^/", "", prefix) ]] <- rpymat::py_to_r(x)
        return()
      }
      nms <- names(x)
      for(nm in nms) {
        Recall(x[[nm]], prefix = sprintf("%s/%s", prefix, nm))
      }
    }
    iterate(dat)
  }
  re
}

