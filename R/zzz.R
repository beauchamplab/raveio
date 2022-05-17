get_os <- function(){
  if("windows" %in% stringr::str_to_lower(.Platform$OS.type)){
    return("windows")
  }
  os <- stringr::str_to_lower(R.version$os)
  if(stringr::str_detect(os, '^darwin')){
    return('darwin')
  }
  if(stringr::str_detect(os, '^linux')){
    return('linux')
  }
  if(stringr::str_detect(os, '^solaris')){
    return('solaris')
  }
  if(stringr::str_detect(os, '^win')){
    return('windows')
  }
  return('unknown')
}

safe_system <- function(cmd, ..., intern = TRUE, ignore.stderr = TRUE,
                        minimized = TRUE, invisible = TRUE, show.output.on.console = TRUE){
  suppressWarnings({
    if(get_os() == 'windows'){
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr,
                    minimized = minimized, invisible = invisible,
                    show.output.on.console = show.output.on.console, ...)
    } else {
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr, ...)
    }
  })
  ret
}

safe_system2 <- function(cmd, args, ..., stdout = TRUE, stderr = FALSE, onFound = NULL, onNotFound = NA){

  if(Sys.which(cmd) == ""){
    return(onNotFound)
  }

  suppressWarnings({
    ret <- system2(cmd, args, ..., stdout = stdout, stderr = stderr)
  })
  if(is.function(onFound)){
    ret <- onFound(ret)
  }
  ret
}




default_settings <- local({
  defaults <- list()

  ensure_defaults <- function(){
    if(!length(defaults)){
      defaults[['..temp']] <- list()
      defaults[['tensor_temp_path']] <- '~/rave_data/cache_dir/'
      defaults[['verbose_level']] <- 'DEBUG'
      defaults[['raw_data_dir']] <- '~/rave_data/raw_dir/'
      defaults[['data_dir']] <- '~/rave_data/data_dir/'
      defaults[['bids_data_dir']] <- '~/rave_data/bids_dir/'
      defaults[['file_structure']] <- 'native'

      # Not validated (but not recommended to change)
      defaults[['module_root_dir']] <- '~/rave_modules/'
      defaults[['module_lookup_file']] <- '~/rave_modules/modules.csv'
      defaults[['delay_input']] <- 20
      defaults[['test_mode']] <- FALSE
      defaults[['fast_cache']] <- TRUE
      defaults[['image_width']] <- 1280L
      defaults[['image_height']] <- 768L
      defaults[['drive_speed']] <- c(50, 20)
      defaults[['disable_startup_speed_check']] <- FALSE
      defaults[['max_worker']] <- parallel::detectCores() - 1
      defaults[['disable_fork_clusters']] <- FALSE
      ram <- tryCatch({
        dipsaus::get_ram() / 1024^3
      }, error = function(e){
        8
      })
      if(is.na(ram) || ram < 0.5){
        ram <- 8
      }
      defaults[['max_mem']] <- ram

      # Not used
      defaults[['server_time_zone']] <- 'America/Chicago'
      defaults[['suma_nodes_per_electrodes']] <- 42L
      defaults[['matlab_path']] <- '/Applications/MATLAB_R2016b.app/bin'
      defaults[['py2_path']] <- ''
      defaults[['py3_path']] <- ''
      defaults[['py_virtualenv']] <- ''
    }
    defaults <<- defaults
  }

  function(s = dipsaus::fastmap2()){
    ensure_defaults()
    dipsaus::list_to_fastmap2(defaults, map = s)
    s
  }
})

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
  tpath <- s[['tensor_temp_path']]
  # Set options so that ravetools can use this path
  options("ravetools.tempdir" = tpath)
  Sys.setenv("RAVETOOLS_TEMPDIR" = tpath)

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

  # ------------- Whether to allow forked clusters ----------
  disable_fork_clusters <- s[['disable_fork_clusters']]
  if(!length(disable_fork_clusters)){ disable_fork_clusters <- FALSE }
  if(!is.logical(disable_fork_clusters)){ disable_fork_clusters <- as.logical(disable_fork_clusters) }
  if(isTRUE(disable_fork_clusters)){
    options(
      "dipsaus.no.fork" = TRUE,
      "dipsaus.cluster.backup" = "multisession"
    )
  } else {
    options("dipsaus.no.fork" = FALSE)
    disable_fork_clusters <- FALSE
  }
  s[['disable_fork_clusters']] <- disable_fork_clusters

  # ------------- 3D viewer templates ----------
  template_subject <- s[['threeBrain_template_subject']]
  if(length(template_subject) != 1 ||
     is.na(template_subject) ||
     !is.character(template_subject)) {
    template_subject <- "N27"
  } else {
    temp_dir <- threeBrain::default_template_directory(check = FALSE)
    if(!dir.exists(file.path(temp_dir, template_subject))) {
      template_subject <- "N27"
    }
  }
  options(threeBrain.template_subject = template_subject)
  s[['threeBrain_template_subject']] <- template_subject


  s
}

flush_conf <- function(s, conf_file){
  if( isTRUE(getOption("raveio.settings_readonly", FALSE)) ){
    return()
  }

  bak <- paste0(conf_file, strftime(Sys.time(), ".%y%m%d-%H%M%S.bak"))
  valid_backup <- FALSE
  if( file.exists(conf_file) ){
    # backup file
    file.copy(conf_file, bak)

    # check if backup file is valid
    valid_backup <- tryCatch({
      yaml::read_yaml(bak)
      TRUE
    }, error = function(e){
      FALSE
    })
  }

  info <- NULL
  if( valid_backup ){
    # bak exists and readable
    info <- stringr::str_trim(readLines(bak), side = "right")
    info <- info[info != '']
  }

  f <- tempfile()
  save_yaml(s, f)

  cmp_info <- NULL
  cmp_info <- stringr::str_trim(readLines(f), side = "right")
  cmp_info <- cmp_info[cmp_info != '']

  if( !is.null(cmp_info) && identical(cmp_info, info) ){
    unlink(f)
    unlink(bak)
    return()
  }

  try({
    file.copy(f, conf_file, overwrite = TRUE)
  }, silent = TRUE)

  # check if conf_file exists
  if( !file.exists(conf_file) ){
    # copy failed (might because of permission issues)
    warning("Unable to write configuration file to:\n  ", conf_file, "\nPermission denied?")
    unlink(f)
    unlink(bak)
    return()
  }

  # check if conf_file is valid yaml file
  valid <- tryCatch({
    yaml::read_yaml(conf_file)
    TRUE
  }, error = function(e){
    FALSE
  })

  if( valid ){
    unlink(f)
    unlink(bak)
    return()
  }

  # if invalid and backup file is valid
  if( valid_backup ){
    warning("Unable to update configurations. Rewind to previous version.")
    try({
      file.copy(bak, conf_file, overwrite = TRUE)
      unlink(bak)
    }, silent = TRUE)
    unlink(f)
    return()
  }

  # if invalid and backup file is also invalid
  if( file.exists(bak) ){
    warning("Unable to update configurations. The settings file is corrupted. \n",
            "Resetting to default settings. The original copy has been backed up at \n  ", bak)
    unlink(conf_file, force = TRUE)
    unlink(f)
    return()
  }

  warning("Unable to update configurations. The settings file is corrupted. ",
          "Resetting to default settings.")
  unlink(conf_file, force = TRUE)
  unlink(f)
  return()
}

load_setting <- function(reset_temp = TRUE){
  s <- get0('.settings', ifnotfound = default_settings())
  tmp <- s$..temp
  sess_str <- get('.session_string')
  conf_path <- R_user_dir(package = 'raveio', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')
  if(file.exists(conf_file)){
    tryCatch({
      load_yaml(conf_file, map = s)
    }, error = function(e){
      bak <- paste0(conf_file, strftime(Sys.time(), ".%y%m%d-%H%M%S.bak"))
      file.copy(conf_file, bak)
      unlink(conf_file, force = TRUE)
      warning("Configuration file is corrupted:\n  ", conf_file, "\nReset to default values. The original copy has been backed up at:\n  ", bak)
    })
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
    flush_conf(s, conf_file)
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
  s <- get0('.settings', ifnotfound = default_settings())
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
    flush_conf(s, conf_file)
  }

  # validate again as temporary settings are removed
  validate_settings(s)

  invisible(as.list(s))
}


# get options whether the data directory is on network
# If enabled, then HDF5 files should be copied to local tempdir
# and read if there are multiiple reads from the same file
using_netdrive <- function(){
  if(raveio_getopt("using_netdrive", FALSE)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @rdname raveio-option
#' @export
raveio_getopt <- function(key, default = NA, temp = TRUE){
  s <- get0('.settings', ifnotfound = default_settings())
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

finalize_installation <- function(
  upgrade = c('ask', 'always', 'never'),
  async = TRUE){

  # # ignore async
  # upgrade <- match.arg(upgrade)
  # if( upgrade == 'ask' ) {
  #   ensure_rhdf5(prompt = TRUE)
  # } else {
  #   ensure_rhdf5(prompt = FALSE)
  # }
}

.onAttach <- function(libname, pkgname) {
  # check if rhdf5 has been installed
  s <- NULL

  pkg <- getNamespace(pkgname)
  if(length(pkg$.startup_msg)){
    s <- c(pkg$.startup_msg, "")
  }

  # if(isTRUE(system.file(package = "rhdf5") == "")){
  #   s <- c(s, "Package `raveio` has been successfully loaded. \nHowever, BioConductor package `rhdf5` has not been installed. \nPlease run the following command:\n\n  BiocManager::install('rhdf5', update = FALSE, type = 'source')\n")
  # }

  if(length(s)){
    s <- paste(s, collapse = "\n")
    packageStartupMessage(s)
  }

}

.onLoad <- function(libname, pkgname) {

  # Sys.unsetenv("RAVE_PIPELINE")

  pkg <- getNamespace(pkgname)
  sess_str <- rand_string(15)
  # .session_string <<- sess_str
  assign('.session_string', sess_str, envir = pkg)

  err_f <- function(e){
    assign('.startup_msg', e$message, envir = pkg)
    NULL
  }
  s <- tryCatch({
    load_setting(reset_temp = TRUE)
  }, error = err_f, warning = err_f)

  if( is.null(s) ){
    s <- default_settings()
  }

  .settings <<- s

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
  try({
    s <- load_setting(reset_temp = TRUE)
    sess_str <- get('.session_string')
    ts_path <- file.path(s[['tensor_temp_path']], sess_str)

    if(dir.exists(ts_path)){
      unlink(ts_path, recursive = TRUE)
    }
  }, silent = TRUE)
}

