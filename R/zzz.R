
default_settings <- function(s = dipsaus::fastmap2()){
  s[['..temp']] <- list()
  s[['tensor_temp_path']] <- '~/rave_data/cache_dir/'
  s[['verbose_level']] <- 'DEBUG'
  s[['raw_data_dir']] <- '~/rave_data/raw_dir/'
  s[['data_dir']] <- '~/rave_data/data_dir/'
  s[['bids_data_dir']] <- '~/rave_data/bids_dir/'
  s[['file_structure']] <- 'native'

  # Not validated (but not recommended to change)
  s[['module_root_dir']] <- '~/rave_modules/'
  s[['module_lookup_file']] <- '~/rave_modules/modules.csv'
  s[['delay_input']] <- 20
  s[['test_mode']] <- FALSE
  s[['fast_cache']] <- TRUE
  s[['image_width']] <- 1280L
  s[['image_height']] <- 768L
  s[['drive_speed']] <- c(50, 20)
  s[['disable_startup_speed_check']] <- FALSE
  s[['max_worker']] <- parallel::detectCores() - 1
  s[['max_mem']] <- dipsaus::get_ram() / 1024^3

  # Not used
  s[['server_time_zone']] <- 'America/Chicago'
  s[['suma_nodes_per_electrodes']] <- 42L
  s[['matlab_path']] <- '/Applications/MATLAB_R2016b.app/bin'
  s[['py2_path']] <- ''
  s[['py3_path']] <- ''
  s[['py_virtualenv']] <- ''
  s
}

validate_settings <- function(s = dipsaus::fastmap2()){
  d <- default_settings()

  # ------------- Temporary tensor path --------------
  tpath <- s[['tensor_temp_path']]
  if(length(tpath) == 0){
    s[['tensor_temp_path']] <- d[['tensor_temp_path']]
  } else if(length(tpath) > 1 || !isTRUE(is.character(tpath))){
    warning('Option tensor_temp_path is not length 1 character, reset to default')
    s[['tensor_temp_path']] <- d[['tensor_temp_path']]
  }

  # ------------- catgl verbose level --------------
  verbose <- s[['verbose_level']]
  verbose <- verbose[verbose %in% c('DEBUG', 'DEFAULT', 'INFO', 'WARNING', 'ERROR', 'FATAL')]
  if(length(verbose) == 0){
    warning('Option verbose_level is not valid. Choices are: ',
            '"DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", and "FATAL". ',
            'Reset to default.')
    verbose <- d[['verbose_level']]
  }
  s[['verbose_level']] <- verbose[[1]]

  # ------------- Raw data path --------------
  raw_dir <- s[['raw_data_dir']]
  raw_dir <- stringr::str_trim(raw_dir)
  if(length(raw_dir) != 1 || !isTRUE(is.character(raw_dir)) || raw_dir %in% c('', '.', '/')){
    warning('raw_data_dir should be a length 1 character to root of the raw data directories')
    raw_dir <- d[['raw_data_dir']]
  }
  s[['raw_data_dir']] <- normalizePath(raw_dir, mustWork = FALSE)

  # ------------- RAVE data path --------------
  data_dir <- s[['data_dir']]
  if(length(data_dir) != 1 || !isTRUE(is.character(data_dir)) || data_dir %in% c('', '.', '/')){
    warning('data_dir should be a length 1 character to root of the rave data directories')
    data_dir <- d[['data_dir']]
  }
  s[['data_dir']] <- normalizePath(data_dir, mustWork = FALSE)

  # ------------- BIDS data path --------------
  bids_dir <- s[['bids_data_dir']]
  if(length(bids_dir) != 1 || !isTRUE(is.character(bids_dir)) || bids_dir %in% c('', '.', '/')){
    warning('bids_data_dir should be a length 1 character to root of the BIDS data directories')
    bids_dir <- d[['bids_data_dir']]
  }
  s[['bids_data_dir']] <- normalizePath(bids_dir, mustWork = FALSE)

  # ------------- File structure: BIDS/native --------------
  file_structure <- s[['file_structure']]
  if(length(file_structure) != 1 || !isTRUE(is.character(file_structure)) || !file_structure %in% c('native', 'BIDS')){
    warning('file_structure can only be ', sQuote('native'), ' or ', sQuote('BIDS'), '. Reseting to `native`')
    file_structure <- d[['file_structure']]
  }
  s[['file_structure']] <- file_structure

  s
}

load_setting <- function(reset_temp = TRUE){
  s <- get0('.settings', ifnotfound = default_settings())
  tmp <- s$..temp
  sess_str <- get('.session_string')
  conf_path <- R_user_dir(package = 'raveio', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')
  if(file.exists(conf_file)){
    load_yaml(conf_file, map = s)
  }
  s$session_string <- sess_str
  if( reset_temp ){
    s$..temp <- list()
  } else {
    s$..temp <- tmp
  }

  validate_settings(s)
  s
}

#' Set/Get 'raveio' option
#' @description Persist settings on local configuration file
#' @param key character, option name
#' @param value character or logical of length 1, option value
#' @param default is key not found, return default value
#' @param all whether to reset all non-default keys
#' @param .save whether to save to local drive, internally used to temporary
#' change option. Not recommended to use it directly.
#' @param cfile file name in configuration path
#' @param temp when saving, whether the key-value pair should be considered
#' temporary, a temporary settings will be ignored when saving; when getting
#' options, setting \code{temp} to false will reveal the actual settings.
#' @return \code{raveio_setopt} returns modified \code{value};
#' \code{raveio_resetopt} returns current settings as a list;
#' \code{raveio_confpath} returns absolute path for the settings file;
#' \code{raveio_getopt} returns the settings value to the given key, or
#' \code{default} if not found.
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
raveio_setopt <- function(key, value, .save = TRUE){

  stopifnot2(isTRUE(
    mode(value) %in% c('numeric', 'logical', 'character')
  ), msg = 'settings value must be numeric, character or logical')

  if(is.character(value) && length(value) > 1){
    stop('settings value must be length 1 for characters')
  }

  stopifnot2(!key %in% c('session_string'),
             msg = sprintf('Key %s is read-only', sQuote(key)))

  conf_path <- R_user_dir(package = 'raveio', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')
  s <- load_setting(reset_temp = FALSE)

  previous <- s[[key]]
  s[[key]] <- value
  validate_settings(s)

  if( .save ){
    s$..temp[[key]] <- NULL
    s <- as.list(s)
    s <- s[!names(s) %in% c('session_string', '..temp')]
    dir_create2(conf_path)
    save_yaml(s, conf_file)
  } else {
    # temporarily set value and restore previous value because
    s$..temp[[key]] <- s[[key]]
    if(length(previous) && all(!is.na(previous))){
      s[[key]] <- previous
    }
  }

  invisible(value)
}

#' @rdname raveio-option
#' @export
raveio_resetopt <- function(all = FALSE){
  s <- get('.settings')
  if(all){
    nms <- names(s)
    nms <- nms[!nms %in% c('session_string', '..temp')]
    .subset2(s, 'remove')(nms)
  }
  default_settings(s)
  validate_settings(s)

  # remove some temporary settings
  .subset2(s, 'remove')('tensor_temp_path')
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
raveio_getopt <- function(key, default = NA, temp = TRUE){
  s <- get('.settings')
  tmp <- s$..temp

  if(missing(key)){
    s <- as.list(s)
    if(temp){
      for(nm in names(tmp)){
        s[[nm]] <- tmp[[nm]]
      }
    }
    return(s)
  }

  if(temp && (key %in% names(tmp))){
    return(tmp[[key]])
  }
  if(.subset2(s, 'has')(key)){
    return(s[[key]])
  }
  default
}

#' @rdname raveio-option
#' @export
raveio_confpath <- function(cfile = 'settings.yaml'){
  d <- R_user_dir('raveio', 'config')
  normalizePath(file.path(d, cfile), mustWork = FALSE)
}

.onLoad <- function(libname, pkgname) {
  # backports::import(pkgname, c("R_user_dir", "deparse1"))
  pkg <- getNamespace(pkgname)
  sess_str <- rand_string(15)
  assign('.session_string', sess_str, envir = pkg)
  s <- load_setting(reset_temp = TRUE)
  assign('.settings', s, envir = pkg)

  cenv <- environment(.subset2(s, 'reset'))

  # .onUnload is suppose to work, but in RStudio environment
  # when users force restart rsession, .onUnload is ignored
  # and hence it's possible to leave massive amount of temporary files.
  # To clean these files, use reg.finalizer on settings, settings
  # map stays with current session. When
  # settings is gced, remove these files.
  reg.finalizer(cenv, function(cenv){
    ts_path <- file.path(cenv$get('tensor_temp_path'),
                         cenv$get('session_string'))
    if(isTRUE(dir.exists(ts_path))){
      unlink(ts_path, recursive = TRUE)
    }
  }, onexit = TRUE)

}

.onUnload <- function(libpath){
  s <- load_setting(reset_temp = TRUE)
  sess_str <- get('.session_string')
  ts_path <- file.path(s[['tensor_temp_path']], sess_str)

  if(dir.exists(ts_path)){
    unlink(ts_path, recursive = TRUE)
  }
}

