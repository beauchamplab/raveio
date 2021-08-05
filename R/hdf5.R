# File IO: HDF5 file wrapper

ensure_rhdf5 <- function(prompt = TRUE){
  if( dipsaus::package_installed('rhdf5') ){ return(TRUE) }

  ans <- TRUE
  if( prompt && interactive() && !dipsaus::shiny_is_running() ){
    ans <- utils::askYesNo("BioConductor package `rhdf5` has not been installed. Do you want to install now?")
  }
  if(!isTRUE(ans)){
    stop("Abort. Please manually call `BiocManager::install('rhdf5')` to install.")
  }

  BiocManager::install('rhdf5', update = FALSE, ask = FALSE, type = 'source')
}

h5FileIsOpen <- function (filename) {
  filename <- normalizePath(filename, mustWork = FALSE)
  L <- rhdf5::h5validObjects()
  isobject <- sapply(L, function(x) {
    tryCatch({
      rhdf5::H5Iget_type(x) %in% c("H5I_FILE", "H5I_GROUP", "H5I_DATASET")
    }, error = function(e){ FALSE })
  })
  if (length(isobject) > 0) {
    isopen <- any(sapply(L[which(isobject)], function(x) {
      tryCatch({
        rhdf5::H5Fget_name(x) == filename
      }, error = function(e){ return(FALSE) })
    }))
  }
  else {
    isopen <- FALSE
  }
  isopen
}

# used to close connections for writing
H5FcloseAll <- function(filename) {
  filename <- normalizePath(filename, mustWork = FALSE)
  if(!h5FileValid(filename)){ return(0L) }
  L <- rhdf5::h5validObjects()
  isobject <- sapply(L, function(x) {
    tryCatch({
      rhdf5::H5Iget_type(x) %in% c("H5I_FILE", "H5I_GROUP", "H5I_DATASET")
    }, error = function(e){ FALSE })
  })
  if (length(isobject) > 0 && any( isobject, na.rm = TRUE )) {
    nclosed <- sapply(L[which(isobject)], function(x) {
      tryCatch({
        if(rhdf5::H5Fget_name(x) == filename){
          # close
          itype <- rhdf5::H5Iget_type(x)
          switch (itype,
                  'H5I_FILE' = {
                    rhdf5::H5Fclose(x)
                  },
                  'H5I_GROUP' = {
                    rhdf5::H5Gclose(x)
                  }, {
                    rhdf5::H5Dclose(x)
                  }
          )
          return( 1L )
        } else {
          return( 0L )
        }
      }, error = function(e){
        return( 0L )
      })

    })
    return( sum(nclosed) )
  } else {
    return( 0L )
  }
}

H5FcloseOthers <- function(h5obj, filename, exclude = NULL) {
  tryCatch({
    # message(3)
    if(missing(filename)){
      filename <- rhdf5::H5Fget_name(h5obj)
    }
    filename <- normalizePath(filename, mustWork = FALSE)
    if(!h5FileValid(filename)){ return(0L) }
    L <- rhdf5::h5validObjects()
    isobject <- sapply(L, function(x) {
      rhdf5::H5Iget_type(x) %in% c("H5I_FILE", "H5I_GROUP", "H5I_DATASET")
    })
    if (length(isobject) > 0 && any( isobject, na.rm = TRUE )) {
      exclude <- c(exclude, h5obj)
      exclude_id <- unlist(lapply(exclude, function(x){
        if(!isS4(x)){ return(NULL) }
        return( x@ID )
      }))
      nclosed <- sapply(L[which(isobject)], function(x) {
        if(rhdf5::H5Fget_name(x) == filename){
          # identical to h5obj
          if(isTRUE(h5obj@ID %in% exclude_id)){ return( 0L ) }
          # close
          itype <- rhdf5::H5Iget_type(x)
          switch (itype,
                  'H5I_FILE' = {
                    rhdf5::H5Fclose(x)
                  },
                  'H5I_GROUP' = {
                    rhdf5::H5Gclose(x)
                  },
                  H5I_DATASET = {
                    rhdf5::H5Dclose(x)
                  }
          )
          return( 1L )
        } else {
          return( 0L )
        }
      })
      return( sum(nclosed) )
    } else {
      return( 0L )
    }

  }, error = function(e){
    0L
  })
}

h5FileValid <- function(filename){
  if(!length(filename)){ return(FALSE) }
  filename <- filename[[1]]
  if(!file.exists(filename)){ return(FALSE) }
  if(isTRUE(file.info(filename)[['isdir']])){ return(FALSE) }
  filename <- normalizePath(filename)
  return(tryCatch({
    rhdf5::H5Fis_hdf5(filename)
  }, error = function(e){ FALSE }))
}

h5FileObject <- function(filename){
  filename <- normalizePath(filename, mustWork = FALSE)
  if(!h5FileValid(filename)){ return(NULL) }
  L <- rhdf5::h5validObjects()
  for(x in L){
    try({
      if(rhdf5::H5Iget_type(x) %in% c("H5I_FILE")){
        if(rhdf5::H5Fget_name(x) == filename){
          return(x)
        }
      }
    }, silent = TRUE)
  }
  return(NULL)
}

h5fileHasData <- function(filename, dataname){
  if(!h5FileValid(filename)){ return(FALSE) }
  fobj <- h5FileObject(filename)
  df <- NULL
  try({
    if(!is.null(fobj)){
      df <- rhdf5::h5ls(fobj, recursive = TRUE)
    }
  }, silent = TRUE)
  if(is.null(df)){
    df <- rhdf5::h5ls(filename, recursive = TRUE)
  }

  dnames <- sprintf('%s/%s', df$group, df$name)
  dnames <- dnames[df$otype == 'H5I_DATASET']
  dnames <- stringr::str_remove_all(dnames, '^[/]+')
  dataname <- stringr::str_remove_all(dataname, '^[/]+')
  dataname %in% dnames
}

h5guessChunk <- function (
  space_maxdims,
  chunk_size = getOption("raveio.h5.chunk_size", 4096)
) {
  chunk_num_elem <- floor(chunk_size)
  space_rank <- length(space_maxdims)
  chunk_dim <- rep(ceiling(chunk_num_elem^(1/space_rank)),
                   space_rank)
  chunk_dim <- pmin(space_maxdims, chunk_dim)
  bounded <- chunk_dim == space_maxdims
  while (prod(chunk_dim) < chunk_num_elem & !all(bounded)) {
    mult_factor <- (chunk_num_elem/prod(chunk_dim))^(1/sum(!bounded))
    chunk_dim[!bounded] <- ceiling(chunk_dim[!bounded] *
                                     mult_factor)
    chunk_dim <- pmin(space_maxdims, chunk_dim)
    bounded <- chunk_dim == space_maxdims
  }
  return(chunk_dim)
}

h5dataType <- function (storage.mode, size = 255L) {
  tid <- switch(
    storage.mode[1],
    double = 'H5T_IEEE_F64LE',
    integer = 'H5T_STD_I32LE',
    integer64 = 'H5T_STD_I64LE',
    logical = 'H5T_STD_I8LE',
    raw = 'H5T_STD_U8LE',
    character = 'H5T_STRING',
    # {
    #   tid <- H5Tcopy("H5T_C_S1")
    #   H5Tset_strpad(tid, strpad = "NULLPAD")
    #   if (!is.numeric(size)) {
    #     stop("parameter 'size' has to be defined for storage.mode character.")
    #   }
    #   H5Tset_size(tid, size)
    #   tid
    # },
    {
      stop("datatype ", storage.mode, " not yet implemented.\n",
           "Try 'logical', 'double', 'integer', 'integer64' or 'character'.")
    }
  )
}

#' @title Lazy 'HDF5' file loader
#' @author Zhengjia Wang
#' @description provides hybrid data structure for 'HDF5' file
#'
#' @examples
#'
#' if( dipsaus::package_installed('rhdf5') ){
#'
#' # Data to save
#' x <- array(rnorm(1000), c(10,10,10))
#'
#' # Save to local disk
#' f <- tempfile()
#' save_h5(x, file = f, name = 'x', chunk = c(10,10,10), level = 0)
#'
#' # Load via LazyFST
#' dat <- LazyH5$new(file_path = f, data_name = 'x', read_only = TRUE)
#'
#' dat
#'
#' # Check whether the data is identical
#' range(dat - x)
#'
#' # Read a slice of the data
#' system.time(dat[,10,])
#' }
#'
#'
#' @export
LazyH5 <- R6::R6Class(
  classname = 'LazyH5',
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    file = NULL,
    name = NULL,
    read_only = TRUE,
    data_ptr = NULL,
    file_ptr = NULL,
    last_dim = NULL
  ),
  active = list(

    #' @field file_ptr_valid whether file pointer is valid or broken;
    #' internally used
    file_ptr_valid = function(){
      if(!"H5IdComponent" %in% class(private$file_ptr)){ return(FALSE) }
      isTRUE(
        tryCatch({
          rhdf5::H5Iis_valid(private$file_ptr)
        }, error = function(e){ FALSE })
      )
    },

    #' @field data_ptr_valid whether data pointer is valid or broken;
    #' internally used
    data_ptr_valid = function(){
      if(!"H5IdComponent" %in% class(private$data_ptr)){ return(FALSE) }
      isTRUE(
        tryCatch({
          rhdf5::H5Iis_valid(private$data_ptr)
        }, error = function(e){ FALSE })
      )
    },

    #' @field file_valid whether file is a valid 'HDF5' file
    file_valid = function(){
      h5FileValid(private$file)
    }
  ),
  public = list(

    #' @field quiet whether to suppress messages
    quiet = FALSE,

    #' @description garbage collection method
    #' @return none
    finalize = function(){
      self$close()
    },

    #' @description overrides print method
    #' @return self instance
    print = function(){
      if(!is.null(private$data_ptr)){
        base::cat(
          sprintf(
            "Class: HDF5 DATASET (%s)",
            ifelse(self$data_ptr_valid, "active", "closed")
          ),
          "\nDataset: ", private$name, "\n",
          "Filename: ", private$file, "\n",
          "Write access: ", ifelse(private$read_only, "no", "yes"), "\n",
          "Dimension: ", paste(private$last_dim, collapse = 'x'), "\n",
          sep = ''
        )
      }
      invisible(self)
    },

    #' @description constructor
    #' @param file_path where data is stored in 'HDF5' format
    #' @param data_name the data stored in the file
    #' @param read_only whether to open the file in read-only mode. It's highly
    #' recommended to set this to be true, otherwise the file connection is
    #' exclusive.
    #' @param quiet whether to suppress messages, default is false
    #' @return self instance
    initialize = function(file_path, data_name, read_only = FALSE, quiet = FALSE){
      ensure_rhdf5()

      if(read_only){
        private$file <- normalizePath(file_path)

        stopifnot(length(private$file) == 1)
        stopifnot2(
          file.exists(private$file),
          rhdf5::H5Fis_hdf5(private$file),
          msg = sprintf('HDF5 file %s not exists or invalid.', private$file)
        )
      }else{
        file_path <- normalizePath(file_path, mustWork = FALSE)
        private$file <- file_path
      }
      self$quiet <- isTRUE(quiet)
      private$name <- stringr::str_replace(data_name, "^[/]{0,}", '/')
      private$read_only <- read_only

      if(file.exists(private$file) && !self$file_valid){
        # check if the file is valid HDF5
        stop("Trying to open a non-HDF5 file that already exists.")
      }
    },

    #' @description save data to a 'HDF5' file
    #' @param x vector, matrix, or array
    #' @param chunk chunk size, length should matches with data dimension
    #' @param level compress level, from 1 to 9
    #' @param replace if the data exists in the file, replace the file or not
    #' @param new_file remove the whole file if exists before writing?
    #' @param force if you open the file in read-only mode, then saving
    #' objects to the file will raise error. Use \code{force=TRUE} to force
    #' write data
    #' @param ctype data type, see \code{\link{mode}}, usually the data type
    #' of \code{x}. Try \code{mode(x)} or \code{storage.mode(x)} for hints.
    #' @param size deprecated, for compatibility issues
    #' @param ... passed to self \code{open()} method
    save = function(x, chunk = 'auto', level = 7, replace = TRUE,
                    new_file = FALSE, force = TRUE, ctype = NULL,
                    size = NULL,
                    ...){
      # function (name, robj = NULL, dtype = NULL, space = NULL, dims = NULL,
      #           chunk_dims = "auto", gzip_level = 4, link_create_pl = h5const$H5P_DEFAULT,
      #           dataset_create_pl = h5const$H5P_DEFAULT, dataset_access_pl = h5const$H5P_DEFAULT)
      read_only <- private$read_only
      if(read_only){
        if(!force){
          stop('File is read-only. Use "force=TRUE"')
        }else{
          # Close current pointer
          self$close(all = TRUE)
          private$read_only <- FALSE

          on.exit({
            private$read_only <- TRUE
            self$close(all = TRUE)
          }, add = TRUE, after = TRUE)
        }
      } else {
        on.exit({
          self$close(all = TRUE)
        }, add = TRUE, after = TRUE)
      }

      if(new_file && file.exists(private$file)){
        self$close(all = TRUE)
        file.remove(private$file)
      }

      # self$open(new_dataset = replace, robj = x, chunk = chunk, gzip_level = level, ...)
      self$open(new_dataset = TRUE)
      has_data <- self$data_ptr_valid

      if(has_data && !replace && !force){
        stop("File [{private$file}] already contains [{private$name}]. Set `replace=TRUE` or `force=TRUE` to force replace the dataset")
      }

      if(has_data){
        # remove it
        H5FcloseOthers(private$file_ptr)
        rhdf5::H5Ldelete(h5loc = private$file_ptr, name = private$name)
      }

      # need to create a new one
      if(is.null(ctype)) {
        ctype <- storage.mode(x)
      } else {
        ctype <- ctype[[1]]
      }

      if(isTRUE(ctype %in% 'character')){
        size_x <- max(nchar(x), na.rm = TRUE)
        if(length(size) != 1 || size < size_x){
          size <- size_x
        }
        if( size < 10 ){
          size <- 10
        }
      }
      # dtype <- h5dataType(ctype)
      dim_x <- dim(x)
      if(!length(dim_x)){ dim_x <- length(x) }

      if(length(chunk) != length(dim_x) ||
         (length(chunk) == 1 && isTRUE(chunk == 'auto'))){
        chunk <- h5guessChunk(dim_x)
      }

      # need to create new dataset
      g <- stringr::str_split(private$name, '/', simplify = TRUE)
      g <- g[stringr::str_trim(g) != '']
      g <- g[-length(g)]

      ptr <- private$file_ptr
      if(length(g)){
        p <- Reduce(function(a,b){
          # c(a, sprintf('%s/%s', a, b))
          sprintf('%s/%s', a, b)
        }, g, init = '', accumulate = TRUE)[-1]
        gp <- cbind(
          c('/', p[-length(p)]),
          g
        )

        df <- rhdf5::h5ls(private$file_ptr)
        df <- df[df$otype == 'H5I_GROUP',]

        for(ii in seq_len(nrow(gp))){
          parent_name <- gp[ii, 1]
          group_name <- gp[ii, 2]
          if(!any(df$group == parent_name & df$name == group_name)){
            ptr <- rhdf5::H5Gcreate(ptr, group_name)
          } else {
            ptr <- ptr&group_name
          }
        }
      }

      # ptr is now parent of dataset
      g <- stringr::str_split(private$name, '/', simplify = TRUE)
      g <- g[[length(g)]]
      # Create dataset
      # rhdf5::h5writeDataset.array(
      #   ptr, name = g, dims = dim_x,
      #                chunk = chunk, storage.mode = ctype,
      #                level = level, size = size,
      #                obj = x, native = FALSE)
      ptr <- rhdf5::h5createDataset(
        file = ptr,
        dataset = g,
        dims = dim_x,
        # H5type = if(ctype == "character") "H5T_C_S1" else NULL,
        storage.mode = ctype,
        chunk = chunk,
        level = level,
        size = size,
        # fillValue = NA,
        native = FALSE
      )
      private$data_ptr <- (private$file_ptr)&(private$name)
      private$data_ptr[] <- x

      # reinitialize parameters
      self$open()

    },


    #' @description open connection
    #' @param new_dataset only used when the internal pointer is closed, or
    #' to write the data
    #' @param robj data array to save
    #' @param ... ignored
    open = function(new_dataset = FALSE, robj, ...){
      # base::print(sys.call(-1))
      # check data pointer
      # if valid, no need to do anything, otherwise, enter if clause
      if(new_dataset || !self$data_ptr_valid){

        # Check if the connection is on and private$file_ptr is valid
        if( !self$file_ptr_valid ){
          # no, close all other connections

          if(!private$read_only){

            # Make sure the file exists and is valid, and connections are closed
            if(file.exists(private$file)){

              # need to write, but file pointer is invalid
              H5FcloseAll(private$file)

              # check if the file is valid HDF5
              if(!self$file_valid){
                stop("Trying to open a non-HDF5 file that already exists.")
              }

            } else {
              # Need to create a new file (H5F_ACC_EXCL in case)
              private$file_ptr <- rhdf5::H5Fcreate(name = private$file,
                                                   native = FALSE,
                                                   flags = 'H5F_ACC_EXCL')
              private$file <- normalizePath(private$file)
              rhdf5::H5Fclose(private$file_ptr)
            }
          }

          open_flag <- ifelse(isTRUE(private$read_only), 'H5F_ACC_RDONLY', 'H5F_ACC_RDWR')

          tryCatch({
            # message(4)
            # private$file_ptr <- hdf5r::H5File$new(private$file, mode)
            private$file_ptr <- rhdf5::H5Fopen(name = private$file,
                                               native = FALSE,
                                               flags = open_flag)
          }, error = function(e){
            # Open for writing, we should close all connections first
            # then the file can be opened, otherwise, Access type: H5F_ACC_RDONLY
            # will lock the file for writing

            # f <- hdf5r::H5File$new(private$file, 'r')
            nclosed <- H5FcloseAll(private$file)
            if(!self$quiet){
              catgl('Closing all other connections to [{private$file}] - {nclosed}')
            }

            private$file_ptr <- rhdf5::H5Fopen(name = private$file,
                                               native = FALSE,
                                               flags = open_flag)
          })
        }

        has_data <- h5fileHasData(filename = private$file, private$name)
        if(has_data){
          private$data_ptr <- (private$file_ptr)&(private$name)
        } else if (!new_dataset){
          # new_dataset = TRUE means private$data_ptr will be created
          stop(catgl('File [{private$file}] has no [{private$name}] in it.'))
        }

      }


      if( self$data_ptr_valid ){
        space <- rhdf5::H5Dget_space(private$data_ptr)
        diminfo <- rhdf5::H5Sget_simple_extent_dims(space)
        rhdf5::H5Sclose(space)

        private$last_dim <- diminfo$size
      }


    },


    #' @description close connection
    #' @param all whether to close all connections associated to the data file.
    #' If true, then all connections, including access from other programs,
    #' will be closed
    close = function(all = NA){
      if(is.na(all)){
        all <- !private$read_only
      }
      try({
        if( all ){
          H5FcloseAll(private$file)
          # base::print('closing all')
        } else {

          # only close data and file pointer
          if(self$data_ptr_valid){
            rhdf5::H5Dclose(private$data_ptr)
            # base::print('closing d')
          }

          if(self$file_ptr_valid){
            rhdf5::H5Fclose(private$file_ptr)
            # base::print('closing f')
          }
        }
      }, silent = TRUE)
    },

    #' @description subset data
    #' @param i,j,... index along each dimension
    #' @param drop whether to apply \code{\link{drop}} the subset
    #' @param stream whether to read partial data at a time
    #' @param envir if \code{i,j,...} are expressions, where should the
    #' expression be evaluated
    #' @return subset of data
    subset = function(
      ...,
      drop = FALSE, stream = FALSE,
      envir = parent.frame()
    ) {

      dims <- self$get_dims(stay_open = TRUE)
      self$open()

      # step 1: eval indices
      # args <- (function(...){eval(substitute(alist(...)))})(,1:10,c(-1,0,NA,1),1:100)
      args <- eval(substitute(alist(...)))
      if(length(args) == 0 || (length(args) == 1 && args[[1]] == '')){
        return(private$data_ptr[])
      }
      args <- lapply(args, function(x){
        if(x == ''){
          return(x)
        }else{
          return(eval(x, envir = envir))
        }
      })

      # step 2: get allocation size
      sapply(seq_along(dims), function(ii){
        if(is.logical(args[[ii]])){
          return(sum(is.na(args[[ii]]) | args[[ii]]))
        }else if(is.numeric(args[[ii]])){
          return(length(args[[ii]]))
        }else if(length(args[[ii]])){
          # must be blank '', otherwise raise error
          return(dims[ii])
        } else {
          return( 0L )
        }
      }) ->
        alloc_dim

      # step 4: get mapping
      lapply(seq_along(dims), function(ii){
        if(is.logical(args[[ii]])){
          tmp <- args[[ii]]
          tmp <- tmp[is.na(tmp) | tmp]
          tmp[is.na(tmp)] <- FALSE
          return(tmp)
        }else if(is.numeric(args[[ii]])){
          re <- args[[ii]] <= dims[ii] & args[[ii]] > 0
          re[is.na(re)] <- FALSE
          return(re)
        }else{
          return(args[[ii]])
        }
      }) ->
        mapping

      # Read

      re <- array(NA, dim = alloc_dim)


      if(stream){
        # step 3: get legit indices
        legit_args <- lapply(seq_along(dims), function(ii){
          if(is.logical(args[[ii]])){
            return(which(!is.na(args[[ii]]) & args[[ii]]))
          }else if(is.numeric(args[[ii]])){
            return(
              args[[ii]][!is.na(args[[ii]]) & args[[ii]] <= dims[ii] & args[[ii]] > 0]
            )
          }else if(length(args[[ii]])){
            return(NULL)
          } else {
            return(integer(0))
          }
        })
        sub <- rhdf5::h5read(
          private$file_ptr,
          name = private$name,
          index = legit_args,
          compoundAsDataFrame = FALSE,
          drop = FALSE
        )
        re <- do.call(`[<-`, c(list(quote(re)), mapping, list(quote(sub))))
      }else{
        legit_args <- lapply(seq_along(dims), function(ii){
          if(is.logical(args[[ii]])){
            return(which(!is.na(args[[ii]]) & args[[ii]]))
          }else if(is.numeric(args[[ii]])){
            return(
              args[[ii]][!is.na(args[[ii]]) & args[[ii]] <= dims[ii] & args[[ii]] > 0]
            )
          }else if(length(args[[ii]])){
            args[[ii]]
          } else {
            return(integer(0))
          }
        })
        re <- do.call(`[<-`, c(list(re), mapping, list(
          value = do.call('[', c(list(private$data_ptr[]), legit_args, list(drop = FALSE)))
        )))
      }

      # self$close(all = !private$read_only)
      self$close(all = FALSE)


      if(drop){
        return(drop(re))
      }else{
        return(re)
      }
    },


    #' @description get data dimension
    #' @param stay_open whether to leave the connection opened
    #' @param refresh whether to discard cache and read from file
    #' @return dimension of the array
    get_dims = function(stay_open = FALSE, refresh = FALSE){
      if(!self$file_valid) { return(NULL) }
      if(!refresh && length(private$last_dim)){ return(private$last_dim) }
      quiet <- self$quiet
      # on.exit({ self$close(all = FALSE) }, add = TRUE)
      tryCatch({
        # message(1)
        if(!quiet){
          self$quiet <- TRUE
        }
        self$open()
      }, error = function(e){})
      self$quiet <- quiet
      if(!stay_open){
        self$close(all = FALSE)
      }
      private$last_dim
    }
  )
)

#' @export
`[.LazyH5` <- function(obj, ...){
  on.exit({obj$close(all = FALSE)}, add = TRUE)
  obj$subset(..., envir = parent.frame())
}

#' @export
`+.LazyH5` <- function(a, b){
  b + a$subset()
}

#' @export
`-.LazyH5` <- function(a, b){
  -(b - a$subset())
}

#' @export
`*.LazyH5` <- function(a, b){
  b * (a$subset())
}

#' @export
`/.LazyH5` <- function(a, b){
  if(inherits(b, 'LazyH5')){
    b <- b$subset()
  }
  a$subset() / b
}

#' @export
dim.LazyH5 <- function(x){
  dim_info <- x$get_dims(stay_open = FALSE)
  if(length(dim_info) == 1){
    dim_info <- NULL
  }
  dim_info
}

#' @export
length.LazyH5 <- function(x){
  dim_info <- x$get_dims()
  prod(dim_info)
}

#' @export
as.array.LazyH5 <- function(x, ...){
  as.array(x$subset(), ...)
}

#' @export
Mod.LazyH5 <- function(z){
  base::Mod(z$subset())
}

#' @export
Arg.LazyH5 <- function(z){
  base::Arg(z$subset())
}


#' @export
exp.LazyH5 <- function(x){
  base::exp(x$subset())
}

#' Lazy Load 'HDF5' File via \pkg{rhdf5}
#'
#' @description Wrapper for class \code{\link{LazyH5}}, which load data with
#' "lazy" mode - only read part of dataset when needed.
#'
#' @param file 'HDF5' file
#' @param name \code{group/data_name} path to dataset (\code{H5D} data)
#' @param read_only only used if \code{ram=FALSE}, whether the returned
#' \code{\link{LazyH5}} instance should be read only
#' @param ram load data to memory immediately, default is false
#' @param quiet whether to suppress messages
#'
#' @return If \code{ram} is true, then return data as arrays, otherwise return
#' a \code{\link{LazyH5}} instance.
#'
#' @seealso \code{\link{save_h5}}
#'
#' @examples
#'
#' if( dipsaus::package_installed('rhdf5') ){
#'
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
#' }
#'
#' @export
load_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE){

  read_only <- read_only || ram
  re <- LazyH5$new(file_path = file, data_name = name, read_only = TRUE, quiet = quiet)
  on.exit({ re$close(all = FALSE) }, add = TRUE)
  if(ram){
    return(re[])
  } else {
    re$open()
    return(re)
  }
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
#' @return Absolute path of the file saved
#'
#' @seealso \code{\link{load_h5}}
#' @examples
#'
#' if( dipsaus::package_installed('rhdf5') ){
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
#'
#' }
#'
#' @export
save_h5 <- function(x, file, name, chunk = 'auto', level = 4, replace = TRUE,
                    new_file = FALSE, ctype = NULL, quiet = FALSE, ...){

  # Make sure all connections are closed
  ensure_rhdf5()
  H5FcloseAll(file)
  # call <- match.call()

  # call[['file']] <- NULL
  # call[['name']] <- NULL

  f <- LazyH5$new(file, name, read_only = FALSE, quiet = quiet)
  on.exit({ f$close(all = TRUE) }, add = TRUE)

  # print(call)
  f$save(x = x, chunk = chunk, level = level, replace = replace,
         new_file = new_file, ctype = ctype, ...)
  # call[[1]] <- quote(f$save)
  # eval(call)

  return(invisible(normalizePath(file)))
}


#' Check whether a 'HDF5' file can be opened for read/write
#' @param file path to file
#' @param mode \code{'r'} for read access and \code{'w'} for write access
#' @param close_all whether to close all connections or just close current
#' connection; default is false. Set this to \code{TRUE} if you want to
#' close all other connections to the file
#' @return logical whether the file can be opened.
#'
#' @examples
#'
#' if( dipsaus::package_installed('rhdf5') ){
#'
#' x <- array(1:27, c(3,3,3))
#' f <- tempfile()
#'
#' # No data written to the file, hence invalid
#' h5_valid(f, 'r')
#'
#' save_h5(x, f, 'dset')
#' h5_valid(f, 'w')
#'
#' }
#'
#' @export
h5_valid <- function(file, mode = c('r', 'w'), close_all = FALSE){
  ensure_rhdf5()
  mode <- match.arg(mode)
  tryCatch({
    # message(2)
    file <- normalizePath(file, mustWork = TRUE)

    open_mode <- ifelse(mode == 'r', 'H5F_ACC_RDONLY', 'H5F_ACC_RDWR')
    f <- rhdf5::H5Fopen(name = file, flags = open_mode, native = FALSE)
    # f <- hdf5r::H5File$new(filename = file, mode = mode)
    if(close_all){
      H5FcloseAll(file)
    } else {
      rhdf5::H5Fclose(f)
    }
    TRUE
  }, error = function(e){
    FALSE
  })

}


#' Returns all names contained in 'HDF5' file
#' @param file, 'HDF5' file path
#' @return characters, data set names
#' @export
h5_names <- function(file){
  ensure_rhdf5()
  if(!h5FileValid(file)){ return(character(0)) }
  fobj <- h5FileObject(file)
  df <- NULL
  try({
    if(!is.null(fobj)){
      df <- rhdf5::h5ls(fobj, recursive = TRUE)
    }
  }, silent = TRUE)
  if(is.null(df)){
    df <- rhdf5::h5ls(file, recursive = TRUE)
  }
  dnames <- sprintf('%s/%s', df$group, df$name)
  dnames <- dnames[df$otype == 'H5I_DATASET']
  dnames <- stringr::str_remove_all(dnames, '^[/]+')
  dnames
}
