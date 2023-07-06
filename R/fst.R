#' @title R6 Class to Load 'fst' Files
#' @author Zhengjia Wang
#' @description provides hybrid data structure for 'fst' file
#' @examples
#'
#' if(!is_on_cran()){
#'
#' # Data to save, total 8 MB
#' x <- matrix(rnorm(1000000), ncol = 100)
#'
#' # Save to local disk
#' f <- tempfile()
#' fst::write_fst(as.data.frame(x), path = f)
#'
#' # Load via LazyFST
#' dat <- LazyFST$new(file_path = f, dims = c(10000, 100))
#'
#' # dat < 1 MB
#'
#' # Check whether the data is identical
#' range(dat[] - x)
#'
#' # The reading of column is very fast
#' system.time(dat[,100])
#'
#' # Reading rows might be slow
#' system.time(dat[1,])
#'
#' }
#'
#' @export
LazyFST <- R6::R6Class(
  classname = 'LazyFST',
  cloneable = FALSE,
  portable = TRUE,
  private = list(
    file_path = NULL,
    transpose = FALSE,
    meta = NULL,
    dims = NULL,
    data = NULL,
    last_visited = NULL,
    delayed = 3
  ),
  public = list(

    #' @description to be compatible with \code{\link{LazyH5}}
    #' @param ... ignored
    #' @returns none
    open = function(...){},

    #' @description close the connection
    #' @param ... ignored
    #' @param .remove_file whether to remove the file when garbage collected
    #' @returns none
    close = function(..., .remove_file = FALSE){
      if(.remove_file){
        unlink(private$file_path)
      }
    },

    #' @description to be compatible with \code{\link{LazyH5}}
    #' @param ... ignored
    #' @returns none
    save = function(...){
      warning('NOT Implemented yet')
    },

    #' @description constructor
    #' @param file_path where the data is stored
    #' @param transpose whether to load data transposed
    #' @param dims data dimension, only support 1 or 2 dimensions
    #' @param ... ignored
    initialize = function(file_path, transpose = FALSE, dims = NULL, ...){
      private$file_path <- file_path
      private$transpose <- transpose
      # check if dimension matches
      private$meta <- fst::metadata_fst(file_path)
      if(length(dims) == 2){
        if(private$meta$nrOfRows * length(private$meta$columnNames) == prod(dims)){
          private$dims <- dims
        }else{
          stop('cached data has different dimensions than the given value')
        }
      }else{
        if(is.null(dims)){
          private$dims <- c(private$meta$nrOfRows, length(private$meta$columnNames))
          if(transpose){
            private$dims <- private$dims[c(2,1)]
          }
        }else{
          stop('fast cache only supports 2 dimension data')
        }
      }
    },

    #' @description get data dimension
    #' @param ... ignored
    #' @returns vector, dimensions
    get_dims = function(...){
      private$dims
    },

    #' @description subset data
    #' @param i,j,... index along each dimension
    #' @param drop whether to apply \code{\link{drop}} the subset
    #' @returns subset of data
    subset = function(i = NULL, j = NULL, ..., drop = TRUE){
      if(!length(j)){
        j <- seq_len(private$dims[2])
      }
      if(!length(i)){
        i <- seq_len(private$dims[1])
      }
      if(is.logical(i)){
        i <- which(i)
      }
      if(is.logical(j)){
        j <- which(j)
      }


      real_i <- i <= private$dims[1]
      real_j <- j <= private$dims[2]

      re <- matrix(NA, nrow = length(i), ncol = length(j))

      private$last_visited <- Sys.time()

      # if(is.null(private$data)){
      #   # load all data
      #   private$data = as.matrix(fst::read_fst(private$file_path))
      # }


      # if(private$transpose){
      #   re[real_i, real_j] = t(private$data[j[real_j], i[real_i]])
      # }else{
      #   re[real_i, real_j] = private$data[i[real_i], j[real_j]]
      # }

      if(private$transpose){
        col_names <- private$meta$columnNames[i[real_i]]
        dat <- as.matrix(load_fst(private$file_path, columns = col_names))
        dat <- dat[j[real_j], ]
        re[real_i, real_j] <- t(dat)
      }else{
        col_names <- private$meta$columnNames[j[real_j]]
        dat <- as.matrix(load_fst(private$file_path, columns = col_names))
        dat <- dat[i[real_i], ]
        re[real_i, real_j] <- dat
      }
      rm(dat)

      # Profiling shows gc() here will take lots of time
      # gc()

      dimnames(re) <- NULL

      # wait 10 secs to see if data idle, if true, remove private$data
      # later::later(function(){
      #   d = as.numeric(difftime(Sys.time(), private$last_visited, units = 'secs') )
      #   if(d >= private$delayed){
      #     private$data = NULL
      #     gc()
      #   }
      # }, delay = private$delayed)

      if(drop){
        return(drop(re))
      }else{
        return(re)
      }

    }
  )
)




#' @export
`[.LazyFST` <- function(obj, i, j, ..., drop = FALSE){
  if(missing(i)){
    i <- NULL
  }
  if(missing(j)){
    j <- NULL
  }
  obj$subset(i, j, ..., drop = drop)
}

#' @export
`+.LazyFST` <- function(a, b){
  b + a$subset()
}

#' @export
`-.LazyFST` <- function(a, b){
  -(b - a$subset())
}

#' @export
`*.LazyFST` <- function(a, b){
  b * (a$subset())
}

#' @export
`/.LazyFST` <- function(a, b){
  if(inherits(b, 'LazyFST')){
    b <- b$subset()
  }
  a$subset() / b
}

#' @export
dim.LazyFST <- function(x){
  dim_info <- x$get_dims()
  if(length(dim_info) == 1){
    dim_info <- NULL
  }
  dim_info
}

#' @export
length.LazyFST <- function(x){
  dim_info <- x$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyFST <- function(x, ...){
  as.array(x$subset(), ...)
}

#' @export
Mod.LazyFST <- function(z){
  base::Mod(z$subset())
}

#' @export
Arg.LazyFST <- function(z){
  base::Arg(z$subset())
}


#' @export
exp.LazyFST <- function(x){
  base::exp(x$subset())
}


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
  catgl('Writing to path: {path}')
  dir <- dirname(path)
  if(!dir.exists(dir)){
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  fst::write_fst(x = x, path = path, ...)
}


#' @rdname read-write-fst
#' @export
load_fst <- function(path, ..., as.data.table = TRUE){
  tryCatch({
    fst::read_fst(path, ..., as.data.table = as.data.table)
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
#' otherwise returns \code{\link{LazyH5}} instance
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
  # check if fst_path exists
  if(file.exists(fst_path)){
    if(ram){
      re <- as.matrix(load_fst(fst_path))
      dimnames(re) <- NULL
      if(fst_need_transpose){
        re <- t(re)
      }
      if(fst_need_drop){
        re <- drop(re)
      }
      return(re)
    }else{
      re <- LazyFST$new(file_path = fst_path, transpose = fst_need_transpose)
      return(re)
    }
  }else{
    re <- load_h5(file = h5_path, name = h5_name, read_only = TRUE, ram = ram)
    return(re)
  }
}


#' @name convert-fst
#' @title Convert 'fst' files to other formats
#' @description 'HDF5', 'csv' are common file formats that can be easily
#' read into 'Matlab' or 'Python'
#' @param fst_path path to 'fst' file
#' @param hdf5_path path to 'HDF5' file; if file exists before the conversion,
#' the file will be erased first. Please make sure the files are backed up.
#' @param csv_path path to 'csv' file; if file exists before the conversion,
#' the file will be erased first. Please make sure the files are backed up.
#' @param exclude_names table names to exclude
#' @returns \code{convert_fst_to_hdf5} will return a list of data saved to 'HDF5';
#' \code{convert_fst_to_csv} returns the normalized 'csv' path.
#' @export
convert_fst_to_hdf5 <- function(fst_path, hdf5_path, exclude_names = NULL) {
  # DIPSAUS DEBUG START
  # fst_path <- "~/rave_data/data_dir/demo/_project_data/power_explorer/exports/KC_demo_export-20210525-071414.fst"
  # hdf5_path <- tempfile()

  tbl <- fst::fst(fst_path)

  if(file.exists(hdf5_path)) {
    unlink(hdf5_path)
  }

  nms <- names(tbl)
  nms <- nms[!nms %in% exclude_names]

  for(nm in nms) {
    dat <- tbl[[nm]]
    if(is.factor(dat)) { dat <- as.character(dat) }
    compress_level <- 9
    if(is.numeric(dat)) {
      compress_level <- 4
    }
    save_h5(x = dat, file = hdf5_path, name = nm, level = compress_level, replace = TRUE)
  }

  read_mat2(hdf5_path, ram = FALSE)
}

#' @rdname convert-fst
#' @export
convert_fst_to_csv <- function(fst_path, csv_path, exclude_names = NULL) {
  # DIPSAUS DEBUG START
  # fst_path <- "~/rave_data/data_dir/demo/_project_data/power_explorer/exports/KC_demo_export-20210525-071414.fst"
  # csv_path <- tempfile()

  tbl <- fst::fst(fst_path)

  if(file.exists(csv_path)) {
    unlink(csv_path)
  }

  nms <- names(tbl)
  nms <- nms[!nms %in% exclude_names]

  utils::write.csv(tbl[, nms], file = csv_path, row.names = FALSE)
  invisible(normalizePath(csv_path))
}
