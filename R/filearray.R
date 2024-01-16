filearray_checkload_or_remove <- function(filebase, mode = "readonly", symlink_ok = TRUE, ...){
  if(file.exists(filebase)){
    tryCatch({
      return(filearray::filearray_checkload(
        filebase = filebase, mode = mode,
        symlink_ok = symlink_ok, ...
      ))
    }, error = function(e){
      unlink(filebase, recursive = TRUE, force = TRUE)
    })
  }
  return(NULL)
}

filearray_create2 <- function(filebase, ..., dimnames = NULL){
  dname <- dirname(filebase)
  if(!dir.exists(dname)){
    dir_create2(dname)
  }

  re <- filearray::filearray_create(filebase = filebase, ...)
  if(length(dimnames)){
    mode <- re$.mode
    on.exit({ re$.mode <- mode }, add = FALSE)
    dimnames(re) <- dimnames
    re$.mode <- mode
    on.exit({}, add = FALSE)
  }
  re
}

#' Generate and automatically cache a file array
#' @description
#' Avoid repeating yourself
#' @param expr expression that should result in a matrix or an array
#' @param filebase where to store the array
#' @param dimension what is the supposed dimension, default is automatically
#' calculated from array. If specified explicitly and the file array dimension
#' is inconsistent, a new calculation will be triggered.
#' @param type file array type, default is \code{"auto"}; can be explicitly
#' specified; see \code{\link[filearray]{filearray_create}}. Inconsistent
#' type will trigger a new calculation.
#' @param partition_size file array partition size; default is \code{1};
#' set it to \code{NA} to generate it automatically. Notice inconsistent
#' partition size will not trigger calculation if the key variables remain
#' the same
#' @param quoted whether \code{expr} has been quoted; default is false
#' @param env environment where to evaluate \code{expr}; the evaluation will
#' be clean, meaning that intermediate variables in the expression will not
#' be available in \code{env}.
#' @param globals names of variables such that any changes
#' should trigger a new evaluation of \code{expr}. This argument is highly
#' recommended to be set explicitly (with atomic variables) though the
#' function automatically calculates the global variables
#' @param verbose whether to verbose debug information
#' @param ... passed to \code{\link[globals]{findGlobals}}
#' @examples
#'
#'
#' c <- 2
#' b <- list(d = matrix(1:9,3))
#' filebase <- tempfile()
#'
#' expr <- quote({
#'   message("New calculation")
#'   re <- c + b$d
#'   dimnames(re) <- list(A=1:3, B = 11:13)
#'   re
#' })
#'
#' # first time running
#' arr <- filearray_from_expr(
#'   expr, quoted = TRUE,
#'   filebase = filebase
#' )
#'
#' # cached, no re-run
#' arr <- filearray_from_expr(
#'   expr, quoted = TRUE,
#'   filebase = filebase
#' )
#'
#' # file array object
#' arr
#'
#' # read into memory
#' arr[]
#'
#' # get digest results
#' arr$get_header("raveio::filearray_cache")
#'
#' ## Clean up this example
#' unlink(filebase, recursive = TRUE)
#'
#' @export
filearray_from_expr <- function(
    expr, filebase, globals, dimension, type = "auto", partition_size = 1L,
    quoted = FALSE, env = parent.frame(),
    verbose = FALSE, ...
) {
  # DIPSAUS DEBUG START
  # a <- new.env(); with(a, {a <- function(){c}}); a$c <- 2
  # b <- list(a = matrix(1:9,3))
  # expr <- quote({print(a$a()+b$a)})
  # filebase <- tempfile()
  # dimension <- c(3,3)
  # type = "auto"
  # partition_size <- NA
  # quoted <- TRUE
  # env = parent.frame()
  # verbose = TRUE
  # cache_as_filearray(expr, filebase)

  if(!quoted) {
    expr <- substitute(expr)
  }
  expr_digest <- dipsaus::digest(deparse1(expr, collapse = "\n"))
  if(missing(globals)) {
    globals <- globals::findGlobals(expr = expr, envir = env, substitute = FALSE, ...)
  }
  globals <- sort(globals)
  global_vars <- structure(
    lapply(globals, function(nm) {
      vars <- get0(nm, envir = env, inherits = TRUE, ifnotfound = NULL)
      dipsaus::digest(vars)
    }),
    names = globals
  )
  digest_results <- structure(
    dipsaus::digest(list(
      expr_digest, global_vars
    )),
    expr_digest = expr_digest,
    global_vars = global_vars,
    class = "raveio_digest_expression"
  )

  if( verbose ) {
    cat("Calculated digest results:\n")
    print(digest_results, max_nvars = 10)
  }

  if( file.exists(filebase) ) {

    tryCatch(
      {
        arr <- filearray::filearray_load(filebase, mode = "readwrite")
        if(!missing(dimension)) {
          dm <- dim(arr)
          if(length(dm) != length(dimension) || !all(dm == dimension)) {
            stop("Cache dimension not match")
          }
        }
        if(!identical(type, "auto") && !identical(arr$type(), type)) {
          stop("Cache type is not consistent")
        }
        cache_info <- arr$get_header("raveio::filearray_cache")

        if(length(cache_info) != 1 || !inherits(cache_info, "raveio_digest_expression") || !is.character(cache_info)) {
          stop("Cached array header `raveio::filearray_cache` is not derived from `raveio::digest_expression`, re-cache")
        }

        if(!identical(digest_results, cache_info)) {
          if( verbose ) {
            # check expression
            if(!identical(attr(digest_results, "expr_digest"), attr(cache_info, "expr_digest"))) {
              stop("Expression that generates the cache has changed, need recache")
            }
            # check variable names
            gvars_old <- attr(cache_info, "global_vars")
            gvars_new <- attr(digest_results, "global_vars")
            if(!setequal(names(gvars_old), names(gvars_new))) {
              stop("Variable names that generate the cache have changed, need recache")
            }
            diff_names <- unlist(lapply(names(gvars_new), function(nm) {
              if(!identical(gvars_old[[nm]], gvars_new[[nm]])) { return(nm) }
              return(NULL)
            }))
            stop("The following variables have changed, need recache: ", paste(diff_names, collapse = ", "))
          } else {
            stop("The data generating cache have been changed, need recache")
          }
        }

        if(verbose) {
          catgl("Using existing cache.", level = "default")
        }
        return(arr)
      },
      error = function(e) {
        if(verbose) {
          catgl(e$message, level = "default")
        }
      }
    )

  }

  # cache not exists or need to re-cache
  array_data <- eval(expr, envir = list(), enclos = env)
  dm <- dim(array_data)
  if(length(dm) < 2) {
    dm <- c(length(array_data), 1L)
  }
  if(!missing(dimension)) {
    if(length(dm) != length(dimension) || !all(dm == dimension)) {
      stop("`filearray_cache`: the resulting array data has no dimension ", paste(dimension, collapse = "x"))
    }
  }
  dimension <- dm

  if(identical(type, "auto")) {
    if(is.numeric(array_data)) {
      type <- "double"
    } else {
      type <- storage.mode(array_data)
    }
  }

  if(file.exists(filebase)) { unlink(filebase, recursive = TRUE) }
  filebase <- normalizePath(filebase, mustWork = FALSE)
  dir_create2(dirname(filebase))

  if(verbose) {
    catgl("Generating cache as filearray ({type}).", level = "default")
  }
  arr <- filearray::filearray_create(
    filebase = filebase,
    dimension = dimension,
    type = type,
    partition_size = partition_size,
    initialize = FALSE
  )
  arr$.mode <- "readwrite"
  arr[] <- array_data
  dimnames(arr) <- dimnames(array_data)

  # save signatures
  arr$set_header(
    key = "raveio::filearray_cache",
    value = digest_results
  )
  arr
}
