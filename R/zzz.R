
default_settings <- function(s = dipsaus::fastmap2()){
  s[['tensor.temp.path']] <- tempdir()
  s
}

validate_settings <- function(s = dipsaus::fastmap2()){
  d <- default_settings()
  if(length(s[['tensor.temp.path']]) != 1 || !isTRUE(is.character(s[['tensor.temp.path']]))){
    s[['tensor.temp.path']] <- d[['tensor.temp.path']]
  }
  s
}

load_setting <- function(){
  s <- get0('.settings', ifnotfound = default_settings())
  sess_str <- get('.session_string')
  conf_path <- R_user_dir(package = 'raveio', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')
  if(file.exists(conf_file)){
    load_yaml(conf_file, map = s)
  }
  s$session_string <- sess_str
  validate_settings(s)
  s
}

#' Set/Get 'raveio' option
#' @description Persist settings on local configuration file
#' @param key character, option name
#' @param value character or logical of length 1, option value
#' @param default is key not found, return default value
#' @param all whether to reset all non-default keys
#' @seealso \code{R_user_dir}
#' @details \code{raveio_setopt} stores key-value pair in local path.
#' The values are persistent and shared across multiple sessions.
#' There are some read-only keys such as \code{"session_string"}. Trying to
#' set those keys will result in error.
#'
#' \code{raveio_getopt} returns value corresponding to the keys. If key is
#' missing, the whole option will be returned.
#'
#' If set \code{all=TRUE}, \code{raveio_resetopt} resets all keys including
#' non-standard ones. However \code{"session_string"} will never reset.
#' @name raveio-option
NULL

#' @rdname raveio-option
#' @export
raveio_setopt <- function(key, value){

  stopifnot2(length(value) == 1 && isTRUE(
    is.character(value) || is.logical(value)
  ), msg = 'settings value must be string or logical of length 1')

  stopifnot2(!key %in% c('session_string'),
             msg = sprintf('Key %s is read-only', sQuote(key)))

  conf_path <- R_user_dir(package = 'raveio', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')
  s <- load_setting()
  s[[key]] <- value
  .subset2(s, 'remove')('session_string')
  dir_create2(conf_path)
  save_yaml(s, conf_file)
  invisible(value)
}

#' @rdname raveio-option
#' @export
raveio_resetopt <- function(all = FALSE){
  s <- get('.settings')
  if(all){
    nms <- names(s)
    nms <- nms[!nms %in% c('session_string')]
    .subset2(s, 'remove')(nms)
  }
  default_settings(s)
  validate_settings(s)

  # remove some temporary settings
  .subset2(s, 'remove')('tensor.temp.path')
  conf_path <- R_user_dir(package = 'raveio', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')

  if(all && file.exists(conf_file)){
    unlink(conf_file)
  } else {
    dir_create2(conf_path)
    save_yaml(s, conf_file)
  }

  # validate again as temporary settings are removed
  validate_settings(s)

  invisible(as.list(s))
}

#' @rdname raveio-option
#' @export
raveio_getopt <- function(key, default = NA){
  s <- get('.settings')

  if(missing(key)){
    return(as.list(s))
  }

  if(.subset2(s, 'has')(key)){
    return(s[[key]])
  }
  default
}

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, c("R_user_dir"))
  pkg <- getNamespace(pkgname)
  sess_str <- rand_string(15)
  assign('.session_string', sess_str, envir = pkg)
  s <- load_setting()
  assign('.settings', s, envir = pkg)

  cenv <- environment(.subset2(s, 'reset'))

  # .onUnload is suppose to work, but in RStudio environment
  # when users force restart rsession, .onUnload is ignored
  # and hence it's possible to leave massive amount of temporary files.
  # To clean these files, use reg.finalizer on settings, settings
  # map stays with current session. When
  # settings is gced, remove these files.
  reg.finalizer(cenv, function(cenv){
    ts_path <- file.path(cenv$get('tensor.temp.path'),
                         cenv$get('session_string'))
    if(isTRUE(dir.exists(ts_path))){
      unlink(ts_path, recursive = TRUE)
    }
  }, onexit = TRUE)

}

.onUnload <- function(libpath){
  s <- load_setting()
  sess_str <- get('.session_string')
  ts_path <- file.path(s[['tensor.temp.path']], sess_str)

  if(dir.exists(ts_path)){
    unlink(ts_path, recursive = TRUE)
  }
}

