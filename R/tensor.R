
temp_tensor_dir <- function(){
  d <- raveio_getopt('tensor_temp_path')
  if(!dir.exists(d)){
    d <- tempdir(check = TRUE)
  }
  d <- file.path(d, get('.session_string', envir = asNamespace("ravepipeline")))
  dir_create2(d)
  normalizePath(d)
}

temp_tensor_file <- function(filename = NA){
  d <- temp_tensor_dir()
  if(is.na(filename)){
    tempfile(tmpdir = d, fileext = '.fst')
  } else {
    file.path(d, filename)
  }
}

#' @title R6 Class for large Tensor (Array) in Hybrid Mode
#' @description can store on hard drive, and read slices of GB-level
#' data in seconds
#' @examples
#'
#' if(!is_on_cran()){
#'
#' # Create a tensor
#' ts <- Tensor$new(
#'   data = 1:18000000, c(3000,300,20),
#'   dimnames = list(A = 1:3000, B = 1:300, C = 1:20),
#'   varnames = c('A', 'B', 'C'))
#'
#' # Size of tensor when in memory is usually large
#' # `lobstr::obj_size(ts)` -> 8.02 MB
#'
#' # Enable hybrid mode
#' ts$to_swap_now()
#'
#' # Hybrid mode, usually less than 1 MB
#' # `lobstr::obj_size(ts)` -> 814 kB
#'
#' # Subset data
#' start1 <- Sys.time()
#' subset(ts, C ~ C < 10 & C > 5, A ~ A < 10)
#' #> Dimension:  9 x 300 x 4
#' #> - A: 1, 2, 3, 4, 5, 6,...
#' #> - B: 1, 2, 3, 4, 5, 6,...
#' #> - C: 6, 7, 8, 9
#' end1 <- Sys.time(); end1 - start1
#' #> Time difference of 0.188035 secs
#'
#' # Join tensors
#' ts <- lapply(1:20, function(ii){
#'   Tensor$new(
#'     data = 1:9000, c(30,300,1),
#'     dimnames = list(A = 1:30, B = 1:300, C = ii),
#'     varnames = c('A', 'B', 'C'), use_index = 2)
#' })
#' ts <- join_tensors(ts, temporary = TRUE)
#'
#' }
#'
#' @export
Tensor <- R6::R6Class(
  classname = 'Tensor',
  cloneable = FALSE,
  parent_env = asNamespace('raveio'),
  private = list(
    .data = NULL,
    fst_locked = FALSE,
    multi_files = FALSE,

    # swap_file file or files to save data to
    .swap_file = character(0),
    set_swap_file = function(fs){

      # use normalized path
      fs <- vapply(fs, function(f){
        if(!file.exists(f)){ file.create(f) }

        f <- normalizePath(f)

        # create a wrapper
        fin <- dipsaus::new_function2(alist(e=), {
          if(e$temporary){
            path <- !!f
            # In the previous implementation, we test
            # path %in% e$swap_file, but that might leave some
            # files uncleaned if the final object registered with
            # finalizer changes its swap_files
            if(isTRUE(file.exists(path))){
              if(getOption("raveio.debug", FALSE)){
                catgl('Removing ', path, level = "DEFAULT")
              }
              unlink(path)
            }
          }
        }, env = baseenv())

        dipsaus::shared_finalizer(self, key = f, fin = fin, onexit = TRUE)
        f
      }, FUN.VALUE = '', USE.NAMES = FALSE)
      private$.swap_file <- fs
    }
  ),
  public = list(

    #' @field dim dimension of the array
    dim = NULL,

    #' @field dimnames dimension names of the array
    dimnames = NULL,

    #' @field use_index whether to use one dimension as index when storing data
    #' as multiple files
    use_index = FALSE,

    #' @field hybrid whether to allow data to be written to disk
    hybrid = FALSE,

    #' @field last_used timestamp of the object was read
    last_used = NULL,

    #' @field temporary whether to remove the files once garbage collected
    temporary = TRUE,

    #' @description release resource and remove files for temporary instances
    do_finalize = function(){
      # if(self$temporary){
      #   # recycle at the end of session
      #   f = RaveFinalizer$new(NULL)
      #   f$files = self$swap_file
      # }
    },

    #' @description print out the data dimensions and snapshot
    #' @param ... ignored
    #' @returns self
    print = function(...){
      cat('Dimension: ', paste(sprintf('%d', self$dim), collapse = ' x '), '\n')

      if(length(self$dimnames) > 0){
        a <- lapply(self$dimnames, function(x){
          s <- paste(x, collapse = ', ')
          if(stringr::str_length(s) > 20){
            s <- paste0(stringr::str_sub(s, end = 17), '...')
          }
          s
        })
        for(x in seq_along(a)){
          cat('- ', names(a)[x], ': ', a[[x]], '\n', sep = '')
        }
      }
      cat("\n")
      invisible(self)
    },

    #' @description Internally used, whether to use multiple files to cache
    #' data instead of one
    #' @param mult logical
    .use_multi_files = function(mult){
      private$multi_files <- isTRUE(mult)
    },

    #' @description constructor
    #' @param data numeric array
    #' @param dim dimension of the array
    #' @param dimnames dimension names of the array
    #' @param varnames characters, names of \code{dimnames}
    #' @param hybrid whether to enable hybrid mode
    #' @param use_index whether to use the last dimension for indexing
    #' @param temporary whether to remove temporary files when existing
    #' @param multi_files if \code{use_index} is true, whether to use multiple
    #' @param swap_file where to store the data in hybrid mode
    #' files to save data by index; default stores in
    #' \code{raveio_getopt('tensor_temp_path')}
    initialize = function(data, dim, dimnames, varnames, hybrid = FALSE,
                          use_index = FALSE, swap_file = temp_tensor_file(),
                          temporary = TRUE, multi_files = FALSE){

      self$temporary <- temporary
      # get attributes of data
      dim %?<-% base::dim(data)
      dim %?<-% length(data)
      if(length(dim) < 2){
        dim <- c(dim, 1)

        dim(data) <- dim
        # set_attr_inplace(data, 'dim', dim)
      }else if(length(dim(data)) != length(dim)){
        dim(data) <- dim
        # set_attr_inplace(data, 'dim', dim)
      }

      if(multi_files){
        n_partition <- max(dim[length(dim)], 1)
        if(n_partition > 1){
          use_index <- TRUE
          if(n_partition != length(swap_file)){
            swap_file <- paste0(swap_file[1], '_part', seq_len(n_partition))
          }
        }else{
          multi_files <- FALSE
        }
      }


      dimnames %?<-% base::dimnames(data)
      dimnames %?<-% lapply(seq_along(varnames), function(v){ seq_len(dim[v]) })

      names(dimnames) <- varnames

      self$last_used <- Sys.time()
      self$dimnames <- dimnames
      self$dim <- dim

      if(hybrid){
        if(use_index){
          if(multi_files){
            n_partition <- max(dim[length(dim)], 1)
            part <- 1
            env <- environment()
            apply(data, length(dim), function(x){
              x <- data.frame(V1 = as.vector(x))
              save_fst(x, swap_file[env$part], compress = 20)
              env$part <- env$part + 1
              NA
            })
          }else{
            data <- apply(data, length(dim), as.vector)
            data <- as.data.frame(data)
            names(data) <- paste0('V', seq_len(ncol(data)))
            save_fst(data, swap_file, compress = 20)
          }

        }else{
          data <- data.frame(V1 = as.vector(data))
          save_fst(data, swap_file, compress = 20)
        }
      }else{
        private$.data <- data
      }
      self$hybrid <- hybrid
      self$use_index <- use_index
      self$swap_file <- swap_file
      private$multi_files <- multi_files

      rm(data)


      # if(!missing(dim)){
      #   self$dim = dim
      #   if(!assertthat::are_equal(dim(data), dim)){
      #     cat2('Dimension does not match', level = 'WARNING')
      #   }
      # }else if(!is.null(base::dim(data))){
      #   self$dim = base::dim(data)
      # }else{
      #   self$dim = length(data)
      # }
      #
      # if(!missing(dimnames)){
      #   self$dimnames = dimnames
      # }else if(!is.null(base::dimnames(data))){
      #   self$dimnames = base::dimnames(data)
      # }else{
      #   self$dimnames = lapply(1:length(varnames), function(v){
      #     1:(self$dim[v])
      #   })
      # }
      # names(self$dimnames) = varnames
      # # dimnames(data) = self$dimnames
      #
      # private$.data = data
      # self$last_used = Sys.time()
    },


    #' @description subset tensor
    #' @param ... dimension slices
    #' @param drop whether to apply \code{\link{drop}} on subset data
    #' @param data_only whether just return the data value, or wrap them as a
    #' \code{Tensor} instance
    #' @param .env environment where \code{...} is evaluated
    #' @returns the sliced data
    subset = function(..., drop = FALSE, data_only = FALSE,
                      .env = parent.frame()){
      ..wrapper <- list2env(self$dimnames, parent = .env)
      # expr = lapply(lazyeval::lazy_dots(...), function(x){x$env = .env; x})
      # class(expr) <- 'lazy_dots'
      # re = lazyeval::lazy_eval(expr, data = self$dimnames)
      # quos <- rlang::quos(...)
      is_missing_dots <- dipsaus::missing_dots(envir = environment())
      quos <- match.call(expand.dots = TRUE)
      # assign("quos", quos, envir = globalenv())
      # assign('is_missing_dots', is_missing_dots, envir = globalenv())
      quos <- as.list(quos)[-1]
      nms <- names(quos)

      if(length(nms) == 0){
        nms <- rep('', length(quos))
      }
      sel <- !nms %in% c("drop", "data_only", ".env")
      quos <- quos[sel][!is_missing_dots]
      nms <- nms[sel][!is_missing_dots]

      for(ii in seq_along(nms)){
        if( nms[[ii]] == '' ){
          fml <- eval(quos[[ii]]) #eval(bquote(.(quos[[ii]])), env = ..wrapper)

          # if is formula
          if(dipsaus::sexp_type2(fml) == 6){
            quos[[ii]] <- list(
              name = as.character(fml[[2]]),
              quo = fml[[3]]
            )
            next
          }
        }
        quos[[ii]] <- list(
          name = nms[[ii]],
          quo = quos[[ii]]
        )
      }

      quos <- dipsaus::drop_nulls(quos)

      re <- lapply(quos, function(item){
        # Use eval_dirty!
        # quo = rlang::quo_set_env(quo, ..wrapper)
        # eval_tidy(quo)
        dipsaus::eval_dirty(item$quo, env = ..wrapper)
      })
      names(re) <- sapply(quos, '[[', 'name')

      dims <- self$dim
      varnames <- names(self$dimnames)

      tmp <- self$dimnames; tmp <- lapply(tmp, function(x){rep(TRUE, length(x))})
      sub_dimnames <- self$dimnames

      for(i in seq_along(re)){
        if(!names(re)[i] %in% varnames){
          n <- varnames[length(re[[i]]) == dims]
          if(length(n) == 0){
            next
          }else if(length(n) > 1){
            warning('Varname not specified')
            n <- n[1]
          }

          names(re)[i] <- n
        }else{
          n <- names(re)[i]
        }
        tmp[[n]] <- re[[i]]
        sub_dimnames[[n]] <- sub_dimnames[[n]][re[[i]]]
      }
      if(drop){
        for(n in names(sub_dimnames)){
          if(length(sub_dimnames[[n]]) <= 1){
            sub_dimnames[[n]] <- NULL
          }
        }
      }

      # sub = do.call(`[`, args = c(list(self$data), tmp, list(drop = drop)))
      # if hybrid, then we only load partial file
      if(!is.null(private$.data)){
        sub <- do.call(`[`, args = c(alist(private$.data), tmp, list(drop = drop)))
      }else{
        # hybrid
        max_dim <- length(self$dim)
        if(self$use_index){
          # we have to load the last index
          if(is.logical(tmp[[max_dim]])){
            tmp[[max_dim]] <- which(tmp[[max_dim]])
          }
          load_dim <- self$dim; load_dim[max_dim] <- length(tmp[[max_dim]])
          if(private$multi_files){
            sub <- do.call(cbind, lapply(tmp[[max_dim]], function(part){
              load_fst(self$swap_file[[part]], columns = 'V1')[[1]]
            }))

          }else{
            sub <- as.matrix(load_fst(self$swap_file, columns = paste0('V', tmp[[max_dim]])))
          }

          dim(sub) <- load_dim
          tmp[[max_dim]] <- seq_along(tmp[[max_dim]])
          sub <- do.call(`[`, args = c(alist(sub), tmp, list(drop = drop)))
          dimnames(sub) <- sub_dimnames
        }else{
          sub <- do.call(`[`, args = c(alist(self$get_data()), tmp, list(drop = drop)))
        }
      }


      if(data_only){
        return(sub)
      }
      # get class
      cls <- class(self)

      is_r6 <- sapply(cls, function(cln){
        tryCatch({
          cl <- get(cln, mode = 'environment')
          if(cl$classname == 'Tensor' && R6::is.R6Class(cl)){
            return(TRUE)
          }
          if(cl$get_inherit()$classname %in% cls && R6::is.R6Class(cl)){
            return(TRUE)
          }
          return(FALSE)
        },
        error = function(e){
          return(FALSE)
        }, quiet = TRUE)
      })

      cls <- cls[is_r6]

      if('Tensor' %in% cls){
        for(cln in cls){
          cl <- get(cln, mode = 'environment')
          sub <- cl$new(sub, dim = dim(sub), dimnames = sub_dimnames, varnames = names(sub_dimnames))
          return(sub)
        }
      }else{
        sub <- Tensor$new(sub, dim = dim(sub), dimnames = sub_dimnames, varnames = names(sub_dimnames))
        return(sub)
      }


    },


    #' @description converts tensor (array) to a table (data frame)
    #' @param include_index logical, whether to include dimension names
    #' @param value_name character, column name of the value
    #' @returns a data frame with the dimension names as index columns and
    #' \code{value_name} as value column
    flatten = function(include_index = FALSE, value_name = 'value'){
      nrow <- prod(self$dim)
      re <- data.frame(V = as.vector(self$get_data()))
      names(re) <- value_name
      if(include_index){
        for(i in seq_along(self$varnames)){
          vn <- self$varnames[i]
          if(i > 1){
            each <- prod(self$dim[1: (i - 1)])
          }else{
            each <- 1
          }
          times <- nrow / self$dim[i] / each

          re[[vn]] <- rep(self$dimnames[[i]], each = each, times = times)
        }
        re <- cbind(re[-1], re[1])
      }
      re
    },

    #' @description Serialize tensor to a file and store it via
    #' \code{\link[fst]{write_fst}}
    #' @param use_index whether to use one of the dimension as index for faster
    #' loading
    #' @param delay if greater than 0, then check when last used, if not long
    #' ago, then do not swap to hard drive. If the difference of time is
    #' greater than \code{delay} in seconds, then swap immediately.
    to_swap = function(use_index = FALSE, delay = 0){
      if(delay == 0){
        self$to_swap_now(use_index = use_index)
      }else{
        delta <- difftime(Sys.time(), self$last_used, units = 'secs')
        if(as.numeric(delta) >= delay){
          # this object might not be in use
          self$to_swap_now(use_index = use_index)
        }
      }
    },

    #' @description Serialize tensor to a file and store it via
    #' \code{\link[fst]{write_fst}} immediately
    #' @param use_index whether to use one of the dimension as index for faster
    #' loading
    to_swap_now = function(use_index = FALSE){
      if(!all(file.exists(self$swap_file))){
        self$swap_file <- temp_tensor_file()
        private$multi_files <- FALSE
      }
      swap_file <- self$swap_file

      self$hybrid <- TRUE
      d <- private$.data
      if(is.null(d)){
        return()
      }
      private$.data <- NULL
      if(use_index || private$multi_files){
        # use the last dim as index
        index <- length(self$dim)
        dim(d) <- c(prod(self$dim) / self$dim[index], self$dim[index])
      }else{
        dim(d) <- NULL
      }
      d <- as.data.frame(d)
      names(d) <- paste0('V', seq_len(ncol(d)))

      if(private$multi_files && length(d) == length(swap_file)){
        for(ii in seq_len(length(d))){
          save_fst(d[ii], path = swap_file, compress = 20)
        }
        self$use_index <- TRUE
        self$swap_file <- swap_file
      }else{
        swap_file <- swap_file[1]
        save_fst(d, path = swap_file, compress = 20)
        self$use_index <- use_index
        self$swap_file <- swap_file
        private$multi_files <- FALSE
      }

    },


    #' @description restore data from hard drive to memory
    #' @param drop whether to apply \code{\link{drop}} to the data
    #' @param gc_delay seconds to delay the garbage collection
    #' @returns original array
    get_data = function(drop = FALSE, gc_delay = 3){
      self$last_used <- Sys.time()
      d <- NULL
      if(!is.null(private$.data)){
        d <- private$.data
      }else if(all(file.exists(self$swap_file))){
        # load data
        if(private$multi_files){
          dim <- self$dim[-length(self$dim)]
          sa <- array(load_fst(self$swap_file[[1]], from=1, to=1)[[1]], dim)
          d <- vapply(seq_len(self$dim[length(self$dim)]), function(part){
            load_fst(self$swap_file[[part]], as.data.table = FALSE)[[1]]
          }, FUN.VALUE = sa)
        }else{
          d <- as.matrix(load_fst(self$swap_file, as.data.table = FALSE))
          dim(d) <- self$dim
        }

        dimnames(d) <- self$dimnames
        if(gc_delay > 0){
          private$.data <- d
        }
      }else{
        stop('Cannot find data from swap file(s).')
      }
      if(drop && !is.null(d)){
        d <- d[drop=TRUE]
      }

      if(self$hybrid){
        private$.data <- NULL
      }

      return(d)
    },

    #' @description set/replace data with given array
    #' @param v the value to replace the old one, must have the same dimension
    #' @param notice the a tensor is an environment. If you change at one place,
    #' the data from all other places will change. So use it carefully.
    set_data = function(v){
      if(private$fst_locked){
        stop('This tensor instance is locked for read-only purpose. Cannot set data!')
      }
      self$last_used <- Sys.time()
      private$.data <- v
      if(self$hybrid && !is.null(v)){
        self$to_swap_now(use_index = self$use_index)
      }
    },


    #' @description apply mean, sum, or median to collapse data
    #' @param keep which dimensions to keep
    #' @param method \code{"mean"}, \code{"sum"}, or \code{"median"}
    #' @returns the collapsed data
    collapse = function(keep, method = 'mean'){
      sel <- keep %in% seq_along(self$dim)
      if(any(!sel)){
        stop('Argument keep is improper.')
      }
      d <- self$get_data()

      if(!is.numeric(d) && !is.complex(d)){
        stop('This tensor is not a numeric tensor')
      }

      # if(any(!is.finite(d))){
      #   cat2('Tensor contains NaNs, converting to zeros', level = 'WARNING')
      #   d[!is.finite(d)] = 0
      # }

      f_c <- function(d){
        switch (
          method,
          'mean' = {
            d <- dipsaus::collapse(d, keep = keep)
            d <- d / prod(self$dim[-keep])
          },
          'median' = {
            d <- apply(d, keep, median)
          }, {
            d <- dipsaus::collapse(d, keep = keep)
          }
        )
        d
      }


      if(is.complex(d)){
        d <- f_c(Re(d)) + 1i * f_c(Im(d))
      }else{
        d <- f_c(d)
      }



      return(d)
    },


    #' @description apply the tensor by anything along given dimension
    #' @param by R object
    #' @param fun function to apply
    #' @param match_dim which dimensions to match with the data
    #' @param mem_optimize optimize memory
    #' @param same_dimension whether the return value has the same dimension as
    #' the original instance
    operate = function(by, fun = .Primitive("/"), match_dim,
                       mem_optimize = FALSE, same_dimension = FALSE){
      by_vector <- as.vector(by)
      if(missing(match_dim)){
        return(fun(self$get_data(), by_vector))
      }
      stopifnot2(
        all(match_dim %in% seq_along(self$dim)),
        (is.null(by) || sum(abs(self$dim[match_dim] - dim(by))) == 0),
        msg = 'Dimension does not match: self$dim[match_dim] = dim(by) ?'
      )
      rest_dims <- seq_along(self$dim)[-match_dim]
      max_dim <- length(self$dim)

      if(mem_optimize && self$hybrid && self$use_index &&
         max_dim %in% rest_dims && self$dim[[max_dim]] != 1){
        # This is a special case where we can avoid using too much memories
        rest_dims <- rest_dims[rest_dims != max_dim]

        .fun <- function(ii){
          if(private$multi_files){
            sub <- load_fst(self$swap_file[[ii]], as.data.table = FALSE, columns = 'V1')[[1]]
          }else{
            sub <- load_fst(self$swap_file, as.data.table = FALSE, columns = paste0('V', ii))[[1]]
          }

          dim(sub) <- self$dim[-max_dim]
          if(length(rest_dims)){
            perm <- c(match_dim, rest_dims)
            sub <- fun(aperm(sub, perm), by_vector)
            sub <- aperm(sub, order(perm))
          }else{
            sub <- fun(sub, by_vector)
          }
          sub
        }

        if(same_dimension){
          re <- lapply(seq_len(self$dim[[max_dim]]), function(ii){
            sub <- .fun(ii)
            # This means sub and original x has the same dimension
            # like baseline, then we fast cache the new data
            dimnames <- self$dimnames
            dimnames[[max_dim]] <- dimnames[[max_dim]][ii]
            dim <- c(self$dim[-max_dim], 1)
            sub <- Tensor$new(data = sub, dim = dim, dimnames = dimnames,
                             varnames = self$varnames, hybrid = FALSE, use_index = FALSE,
                             temporary = FALSE, multi_files = FALSE)
            sub$to_swap_now(use_index = FALSE)
            sub
          })

          re <- join_tensors(re)

        }else{
          re <- vapply(seq_len(self$dim[[max_dim]]), .fun, FUN.VALUE = array(0, dim = self$dim[-max_dim]))
        }

        return(re)
      }else{
        # general case
        perm <- c(match_dim, rest_dims)

        if(mem_optimize && same_dimension && max_dim %in% match_dim){
          byidx <- which(match_dim == max_dim)
          byperm <- perm[perm != max_dim]
          last_name <- self$varnames[[max_dim]]
          tmp <- new.env(parent = emptyenv())
          tmp$ii <- 1
          dimnames <- self$dimnames
          dim <- self$dim
          re <- apply(by, byidx, function(y){
            last_d <- self$dimnames[[last_name]][[tmp$ii]]
            tmp$ii <- tmp$ii + 1
            expr <- sprintf('self$subset(%s = %s == last_d, data_only = TRUE, drop = FALSE)',
                           last_name, last_name)
            sub <- eval(parse(text = expr))
            if(is.unsorted(perm)){
              sub <- aperm(sub, perm = perm)
              sub <- fun(sub, as.vector(y))
              sub <- aperm(sub, order(perm))
            }else{
              sub <- fun(sub, as.vector(y))
            }
            # save to temp file
            dimnames[[max_dim]] <- last_d
            dim[[max_dim]] <- 1
            sub <- Tensor$new(data = sub, dimnames = dimnames, dim = dim, varnames = self$varnames, hybrid = FALSE, use_index = FALSE, temporary = FALSE, multi_files = FALSE)
            sub$to_swap_now(use_index = FALSE)
            sub
          })
          re <- join_tensors(re)
          return(re)
        }

        if(is.unsorted(perm)){
          sub <- aperm(self$get_data(), perm = perm)
          sub <- fun(sub, by_vector)
          sub <- aperm(sub, order(perm))
        }else{
          sub <- fun(self$get_data(), by_vector)
        }
        return(sub)
      }
    }
  ),
  active = list(

    #' @field varnames dimension names (read-only)
    varnames = function(){
      return(names(self$dimnames))
    },

    #' @field read_only whether to protect the swap files from being changed
    read_only = function(v){
      if(missing(v)){
        return(private$fst_locked)
      }else{
        private$fst_locked <- isTRUE(v)
      }
    },


    #' @field swap_file file or files to save data to
    swap_file = function(v){
      if(!missing(v)){
        private$set_swap_file(v)
      }
      private$.swap_file
    }
  )
)




# Documented on 2019-10-11

#' @title 'iEEG/ECoG' Tensor class inherit from \code{\link{Tensor}}
#' @author Zhengjia Wang
#' @description Four-mode tensor (array) especially designed for
#' 'iEEG/ECoG' data. The Dimension names are: \code{Trial},
#' \code{Frequency}, \code{Time}, and \code{Electrode}.
#' @export
ECoGTensor <- R6::R6Class(
  classname = 'ECoGTensor',
  parent_env = asNamespace('raveio'),
  inherit = Tensor,
  cloneable = FALSE,
  public = list(

    #' @description converts tensor (array) to a table (data frame)
    #' @param include_index logical, whether to include dimension names
    #' @param value_name character, column name of the value
    #' @returns a data frame with the dimension names as index columns and
    #' \code{value_name} as value column
    flatten = function(include_index = TRUE, value_name = 'value'){
      nrow <- prod(self$dim)
      re <- data.frame(V = as.vector(self$get_data()))
      names(re) <- value_name
      if(include_index){
        for(i in seq_along(self$varnames)){
          vn <- self$varnames[i]
          if(i > 1){
            each <- prod(self$dim[1: (i - 1)])
          }else{
            each <- 1
          }
          times <- nrow / self$dim[i] / each

          re[[vn]] <- rep(self$dimnames[[i]], each = each, times = times)
          if(i == 1){
            re[['Trial_Number']] <- rep(1:self$dim[1], each = 1, times = times)
          }
        }
        re <- cbind(re[-1], re[1])
      }
      re
    },

    #' @description constructor
    #' @param data array or vector
    #' @param dim dimension of data, mush match with \code{data}
    #' @param dimnames list of dimension names, equal length as \code{dim}
    #' @param varnames names of \code{dimnames}, recommended names are:
    #' \code{Trial}, \code{Frequency}, \code{Time}, and \code{Electrode}
    #' @param hybrid whether to enable hybrid mode to reduce RAM usage
    #' @param swap_file if hybrid mode, where to store the data; default
    #' stores in \code{raveio_getopt('tensor_temp_path')}
    #' @param temporary whether to clean up the space when exiting R session
    #' @param multi_files logical, whether to use multiple files instead of
    #' one giant file to store data
    #' @param use_index logical, when \code{multi_files} is true, whether use
    #' index dimension as partition number
    #' @param ... further passed to \code{\link{Tensor}} constructor
    #' @returns an \code{ECoGTensor} instance
    initialize = function(data, dim, dimnames, varnames, hybrid = FALSE,
                          swap_file = temp_tensor_file(), temporary = TRUE,
                          multi_files = FALSE, use_index = TRUE, ...){
      self$temporary <- temporary
      # get attributes of data
      dim %?<-% base::dim(data)
      dim %?<-% length(data)
      dimnames %?<-% base::dimnames(data)
      dimnames %?<-% lapply(seq_along(varnames), function(v){ seq_len(dim[v]) })

      names(dimnames) <- varnames

      self$last_used <- Sys.time()
      self$dimnames <- dimnames
      self$dim <- dim




      tryCatch({
        if('Frequency' %in% varnames){
          self$dimnames$Frequency <- as.numeric(self$dimnames$Frequency)
        }
      }, error = function(e){})
      tryCatch({
        if('Time' %in% varnames){
          self$dimnames$Time <- as.numeric(self$dimnames$Time)
        }
      }, error = function(e){})
      tryCatch({
        if('Electrode' %in% varnames){
          self$dimnames$Electrode <- as.numeric(self$dimnames$Electrode)
        }
      }, error = function(e){})

      super$initialize(
        data = data, dim = dim, dimnames = dimnames, varnames = varnames, hybrid = hybrid,
        swap_file = swap_file, temporary = temporary,
        multi_files = multi_files, use_index = use_index, ...
      )
      rm(data)

      # private$.data = data
      #
      # self$hybrid = hybrid
      # self$use_index = T
      #
      # self$swap_file = swap_file

      # to_swap
      if(hybrid){
        self$to_swap_now(use_index = use_index)
      }
    }
  )
)


#' @title Join Multiple Tensors into One Tensor
#' @author Zhengjia Wang
#' @param tensors list of \code{\link{Tensor}} instances
#' @param temporary whether to garbage collect space when exiting R session
#' @returns A new \code{\link{Tensor}} instance with the last dimension
#' @details Merges multiple tensors. Each tensor must share the same dimension
#' with the last one dimension as 1, for example, \code{100x100x1}. Join 3
#' tensors like this will result in a \code{100x100x3} tensor. This function
#' is handy when each sub-tensors are generated separately. However, it does no
#' validation test. Use with cautions.
#' @examples
#' tensor1 <- Tensor$new(data = 1:9, c(3,3,1), dimnames = list(
#' A = 1:3, B = 1:3, C = 1
#' ), varnames = c('A', 'B', 'C'))
#' tensor2 <- Tensor$new(data = 10:18, c(3,3,1), dimnames = list(
#'   A = 1:3, B = 1:3, C = 2
#' ), varnames = c('A', 'B', 'C'))
#' merged <- join_tensors(list(tensor1, tensor2))
#' merged$get_data()
#'
#' @export
join_tensors <- function(tensors, temporary = TRUE){
  # Join tensors by the last dim. This is a quick and dirty way - doesn't
  # do any checks
  if(!length(tensors)){
    return(NULL)
  }

  dim <- dim(tensors[[1]])
  n_dims <- length(dim)
  dimnames <- dimnames(tensors[[1]])
  last_dnames <- unlist(lapply(tensors, function(tensor){
    tensor$dimnames[[n_dims]]
  }))
  dimnames[[n_dims]] <- last_dnames
  dim[[n_dims]] <- length(last_dnames)

  swap_files <- unlist(lapply(tensors, function(tensor){
    # swap!
    tensor$to_swap_now(use_index = FALSE)

    tensor$swap_file
  }))



  cls <- Tensor
  if('ECoGTensor' %in% class(tensors[[1]])){
    cls <- ECoGTensor
  }

  varnames <- names(dimnames)
  re <- cls$new(data = 1, dim = rep(1, n_dims),
               dimnames = sapply(varnames, function(nm){1}, simplify = FALSE, USE.NAMES = TRUE),
               varnames = varnames, hybrid = FALSE)
  re$swap_file <- swap_files
  re$.use_multi_files(TRUE)
  re$hybrid <- TRUE
  re$set_data(NULL)
  re$dim <- dim
  re$dimnames <- dimnames
  re$temporary <- temporary
  re
}





#' @export
dim.Tensor <- function(x){
  x$dim
}

#' @export
dimnames.Tensor <- function(x){
  x$dimnames
}

#' @export
`[.ECoGTensor` <- function(obj, i, j, k, l){
  dim <- obj$dim
  if(missing(i)){
    i <- 1:dim[1]
  }
  if(missing(j)){
    j <- 1:dim[2]
  }
  if(missing(k)){
    k <- 1:dim[3]
  }
  if(missing(l)){
    l <- 1:dim[4]
  }
  obj$subset(
    Trial = i,
    Frequency = j,
    Time = k,
    Electrode = l,
    drop = FALSE
  )

  #
  #   nd <- obj$data[i,j,k,l, drop = FALSE]
  #   dimnames = obj$dimnames
  #   dimnames[['Trial']] = dimnames[['Trial']][i]
  #   dimnames[['Frequency']] = dimnames[['Frequency']][j]
  #   dimnames[['Time']] = dimnames[['Time']][k]
  #   dimnames[['Electrode']] = dimnames[['Electrode']][l]
  #   ECoGTensor$new(data = nd,
  #                  dim = dim(nd),
  #                  dimnames = dimnames,
  #                  varnames = c('Trial', 'Frequency', 'Time', 'Electrode'))
}

#' @exportS3Method raveio::content
content.Tensor <- function(obj, ...){
  obj$get_data()
}

content <- function(obj, ...){
  UseMethod('content')
}


#' @export
subset.Tensor <- function(x, ..., .env = parent.frame()){
  x$subset(...,.env = .env)
}

#' @export
as.vector.Tensor <- function(x, ...){
  d <- x$get_data()
  base::as.vector(d, ...)
}




