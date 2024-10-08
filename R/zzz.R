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

normalize_path <- function(path, must_work = NA) {
  path <- unlist(lapply(path, function(p) {
    if(!file.exists(p)) {
      dname <- dirname(p)
      dname <- normalizePath(dname, winslash = "/", mustWork = must_work)
      p <- file.path(dname, basename(p), fsep = "/")
    } else {
      p <- normalizePath(p, winslash = "/", mustWork = must_work)
    }
    p
  }))

  gsub("[/|\\\\]+", "/", path)
}

file_move <- function(from, to) {
  if(dipsaus::package_installed("fs")) {
    fs <- asNamespace("fs")
    impl <- fs$file_move
    if(is.function(impl)) {
      impl(path = from, new_path = to)
      return(invisible(to))
    }
  }
  file.rename(from = from, to = to)
  return(invisible(to))
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

require_package <- function(package) {
  # if(system.file(package = package) == "") {
  #   stop(sprintf("Package [%s] is needed to run the script. Please install it first via\n  install.packages('%s')", package, package), call. = NULL)
  #
  # }
  targets::tar_assert_package(package)
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
  # options("ravetools.tempdir" = tpath)
  # Sys.setenv("RAVETOOLS_TEMPDIR" = tpath)

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
#' @returns \code{raveio_setopt} returns modified \code{value};
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
#' The following keys are reserved by 'RAVE':
#'
#' \describe{
#' \item{\code{data_dir}}{Directory path, where processed data are stored;
#' default is at home directory, folder \code{~/rave_data/data_dir}}
#' \item{\code{raw_data_dir}}{Directory path, where raw data files are stored,
#' mainly the original signal files and imaging files;
#' default is at home directory, folder \code{~/rave_data/raw_dir}}
#' \item{\code{max_worker}}{Maximum number of CPU cores to use; default
#' is one less than the total number of CPU cores}
#' \item{\code{mni_template_root}}{Directory path, where 'MNI' templates
#' are stored}
#' }
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

  re <- NULL
  key_found <- FALSE
  if(temp && (key %in% names(tmp))){
    re <- tmp[[key]]
    key_found <- TRUE
  }
  if(!key_found && .subset2(s, 'has')(key)){
    re <- s[[key]]
    key_found <- TRUE
  }

  if(!key_found) {
    re <- default
  }

  try(silent = TRUE, expr = {
    if( identical(key, "max_worker") ) {
      if( re <= 0L ) {
        re <- 1L
      } else if(
        identical(key, "max_worker") &&
        (
          # identical(Sys.getenv("OMP_THREAD_LIMIT"), "2") ||
          identical(toupper(Sys.getenv("_R_CHECK_LIMIT_CORES_")), "TRUE")
        ) &&
        re > 2L
      ) {
        # Make sure using max 2 CPU cores on CRAN
        re <- 1L
      }
    }
  })

  re

}

#' @rdname raveio-option
#' @export
raveio_confpath <- function(cfile = 'settings.yaml'){
  d <- R_user_dir('raveio', 'config')
  normalizePath(file.path(d, cfile), mustWork = FALSE)
}

finalize_installation <- function(
  upgrade = c('ask', 'always', 'never', 'config-only', 'data-only'),
  async = TRUE, ...){

  upgrade <- match.arg(upgrade)

  template_path <- file.path(R_user_dir('raveio', 'data'), "rave-pipelines")
  if(dir.exists(template_path)) {
    if(upgrade %in% c("never")) { return() }
    if(upgrade == "ask") {
      ans <- dipsaus::ask_yesno("Existing version of `rave-pipelines` is detected, upgrade?", end = "\n", error_if_canceled = FALSE, rs_title = "Upgrade module templates")
      if(!isTRUE(ans)) { return() }
    }
  }

  update_sample_data <- FALSE
  if(upgrade %in% c("always", "data-only")) {
    update_sample_data <- TRUE
  }

  if(upgrade %in% c('always')) {
    upgrade <- TRUE
  } else {
    upgrade <- FALSE
  }

  repo_name <- 'rave-ieeg/rave-pipelines'
  if( getOption("ravemanager.nightly", FALSE) ) {
    repo_name <- 'rave-ieeg/rave-pipelines'
  }

  # Backup ravedash sessions since they might be too old now
  cache_path <- cache_root()
  fs <- list.dirs(cache_path, full.names = FALSE, recursive = FALSE)
  fs <- fs[grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", fs)]

  if(length(fs)) {
    for(path in file.path(cache_path, fs)) {
      backup_file(path, remove = TRUE, quiet = TRUE)
    }
  }

  if(async) {
    dipsaus::rs_exec(bquote({
      ns <- asNamespace("raveio")
      ns$pipeline_install_github(
        repo = repo_name,
        to = "default",
        upgrade = .(upgrade)
      )
      ns$update_local_snippet(force = TRUE)


      update_sample_data <- .(update_sample_data)
      subject <- ns$RAVESubject$new(project_name = "YAEL", subject_code = "yael_demo_001", strict = FALSE)
      if(update_sample_data || is.na(subject$freesurfer_path)) {
        ns$install_subject(
          "yael_demo_001",
          overwrite = TRUE,
          backup = FALSE,
          use_cache = TRUE,
          ask = FALSE
        )
      }

      subject <- ns$RAVESubject$new(project_name = "demo", subject_code = "DemoSubject", strict = FALSE)
      if(update_sample_data || !dir.exists(subject$path)) {
        ns$install_subject(
          "DemoSubject",
          overwrite = TRUE,
          backup = FALSE,
          use_cache = TRUE,
          ask = FALSE
        )
      }

      message("Done.")
    }),
    quoted = TRUE,
    name = "Upgrade pipeline templates",
    focus_on_console = TRUE)
  } else {
    pipeline_install_github(
      repo = repo_name,
      to = "default", upgrade = upgrade
    )
    update_local_snippet(force = TRUE)

    subject <- RAVESubject$new(project_name = "YAEL", subject_code = "yael_demo_001", strict = FALSE)
    if(update_sample_data || is.na(subject$freesurfer_path)) {
      install_subject(
        "yael_demo_001",
        overwrite = TRUE,
        backup = FALSE,
        use_cache = TRUE,
        ask = FALSE
      )
    }

    subject <- RAVESubject$new(project_name = "demo", subject_code = "DemoSubject", strict = FALSE)
    if(update_sample_data || !dir.exists(subject$path)) {
      install_subject(
        "DemoSubject",
        overwrite = TRUE,
        backup = FALSE,
        use_cache = TRUE,
        ask = FALSE
      )
    }


  }



  invisible()

}

#' @title Install 'RAVE' modules
#' @param modules a vector of characters, repository names; default is to
#' automatically determined from a public registry
#' @param dependencies whether to update dependent packages; default is false
#' @returns nothing
#' @export
install_modules <- function(modules, dependencies = FALSE) {

  # update registries
  regs <- get_modules_registries()

  if(missing(modules) || !length(modules)) {
    modules <- sapply(regs, '[[', 'repo')
    message('Found the following registries:\n  ', paste(modules, collapse = ", "))
  }

  for(repo in modules) {
    tryCatch({
      pipeline_install_github(
        repo = repo,
        to = "default",
        upgrade = dependencies
      )
    }, error = function(e) {
      # TODO: try to use the URL
      warning("Cannot install [", repo, "]. Reason: ", e$message)
    })

  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  # check if rhdf5 has been installed
  s <- NULL

  pkg <- asNamespace(pkgname)
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

  pkg <- asNamespace(pkgname)
  sess_str <- rand_string(15)
  # .session_string <<- sess_str
  assign('.session_string', sess_str, envir = pkg)

  err_f <- function(e){
    assign('.startup_msg', sprintf("Issues loading `raveio`: %s\n", paste(e$message, collapse = "\n")), envir = pkg)
    NULL
  }
  s <- NULL
  tryCatch({
    suppressWarnings({
      s <- load_setting(reset_temp = TRUE)
    })
  }, error = err_f, warning = err_f)

  if( is.null(s) ){
    s <- default_settings()
  }

  .settings <<- s

  assign('.settings', s, envir = pkg)
  cenv <- environment(.subset2(s, 'reset'))

  assign(".target_formats", dipsaus::fastmap2(), envir = pkg)

  target_format_register_onload()

  # .onUnload is suppose to work, but in RStudio environment
  # when users force restart rsession, .onUnload is ignored
  # and hence it's possible to leave massive amount of temporary files.
  # To clean these files, use reg.finalizer on settings, settings
  # map stays with current session. When
  # settings is gced, remove these files.
  reg.finalizer(cenv, function(cenv){
    try(expr = {
      if(is.function(cenv$get)) {
        tf_path <- cenv$get('tensor_temp_path')
        sess_str2 <- paste(sess_str, collapse = "")
        if(
          length(tf_path) == 1 && !is.na(tf_path) && is.character(tf_path) &&
          !trimws(tf_path) %in% c("", ".", "/") && file.exists(tf_path) &&
          !is.na(sess_str2) && nzchar(sess_str2)
        ) {
          ts_path <- file.path(tf_path, sess_str2)
          if(isTRUE(dir.exists(ts_path))){
            unlink(ts_path, recursive = TRUE)
          }
        }
      }
    })

  }, onexit = TRUE)

  # check if ravetools is installed
  if(isNamespaceLoaded("ravetools") || system.file(package = "ravetools") != "") {
    options("raveio.use.ravetools" = TRUE)
  }

}

.onUnload <- function(libpath){
  try({
    s <- load_setting(reset_temp = TRUE)
    sess_str <- get('.session_string')
    tf_path <- s[['tensor_temp_path']]
    if(
      length(tf_path) == 1 && !is.na(tf_path) && is.character(tf_path) &&
      !trimws(tf_path) %in% c("", ".", "/") && file.exists(tf_path) &&
      length(sess_str) == 1 && !is.na(sess_str) && nzchar(sess_str)
    ) {
      ts_path <- file.path(tf_path, sess_str)
      if(isTRUE(dir.exists(ts_path))){
        unlink(ts_path, recursive = TRUE)
      }
    }
  }, silent = TRUE)
}


global_preferences <- function(name = "default", ..., .initial_prefs = list(),
                              .prefix_whitelist = NULL, .type_whitelist = NULL,
                              .overwrite = FALSE, .verbose = FALSE) {
  stopifnot2(
    grepl(pattern = "^[a-zA-Z0-9_-]+$",
          x = name),
    msg = "preference `name` must only contain letters (a-z), digits (0-9), underscore (_), and hyphen (-)"
  )
  name <- trimws(tolower(name))

  if(length(name) != 1 || is.na(name) || !nzchar(name) || grepl("(^\\.|[/\\\\])", name)) {
    stop("Invalid preference name [", name, "]. Must be non-hidden file name")
  }

  pref_path <- file.path(R_user_dir("raveio", which = "config"), "preferences", name)

  preference <- tryCatch({
    preference <- dipsaus::rds_map(pref_path)
    stopifnot(preference$is_valid)
    preference
  }, error = function(e) {
    if(file.exists(pref_path)) {
      unlink(pref_path, unlink(TRUE))
    }
    dipsaus::rds_map(pref_path)
  })

  prefix_whitelist <- unlist(.prefix_whitelist)
  type_whitelist <- unlist(.type_whitelist)

  validate_names <- function(nms, expected_length) {
    if(length(nms) != expected_length) {
      stop("Preference names cannot be blank")
    }
    if(expected_length == 0) { return() }
    if(any(nms == "")) {
      stop("Preference names cannot be blank")
    }
    # nms <- c("global.graphics.cex", "power_explorer.graphics.cex", "global.graphics.use_ggplot", "power_explorer.export.default_format")

    parsed <- t(sapply(nms, function(nm) {
      item <- strsplit(nm, ".", fixed = TRUE)[[1]]
      if(length(item) < 3) {
        stop("Invalid preference name `", nm, "`. \nPreference must have syntax [global/<module ID>].[type].[key]. \nFor example: `global.graphics.use_ggplot`, `power_explorer.export.default_format`, ...")
      }
      item <- c(item[c(1, 2)], paste(item[-c(1, 2)], collapse = "."))

      if(length(prefix_whitelist)) {
        if(!item[[1]] %in% prefix_whitelist) {
          stop(
            "Cannot set preference `",
            nm,
            "`: invalid prefix. Allowed prefix choices are: ",
            paste(sprintf("`%s`", prefix_whitelist), collapse = ", ")
          )
        }
      }

      if(length(type_whitelist)) {
        if(!item[[2]] %in% type_whitelist) {
          stop(
            "Cannot set preference `",
            nm,
            "`: invalid type. Allowed type choices are: ",
            paste(sprintf("`%s`", type_whitelist), collapse = ", ")
          )
        }
      }

      item
    }))

  }

  re <- list(
    get = preference$get,
    mget = preference$mget,

    set = function(key, value, signature) {
      validate_names(key, 1)
      if(is.null(value)) {
        # unset
        preference$remove(key)
      } else {
        if( missing(signature) ) {
          preference$set(key, value)
        } else {
          preference$set(key, value, signature)
        }
      }
      return(invisible(value))
    },
    mset = function(..., .list = NULL) {
      .list <- c(list(...), .list)
      nms <- names(.list)
      validate_names(nms, length(.list))
      is_null <- nms[sapply(nms, function(nm) { is.null(.list[[nm]]) })]
      if(length(is_null)) {
        preference$remove(is_null)
        nms <- nms[!nms %in% is_null]
      }
      if(length(nms)) {
        lapply(nms, function(nm){
          preference$set(nm, .list[[nm]])
        })
      }
      invisible(.list)
    },

    keys = function(include_signatures = FALSE) {
      if(include_signatures) {
        re <- preference$keys(TRUE)
        if(!length(re)) {
          re <- array(character(0L), c(0L, 2L))
        }
        rownames(re) <- NULL
        colnames(re) <- c("key", "signature")
      } else {
        re <- unname(preference$keys(FALSE))
      }
      re
    },
    has = preference$has,

    size = preference$size,
    remove = preference$remove,

    reset = preference$reset,
    destroy = preference$destroy
  )


  # avoid evaluating dots
  dot_names <- ...names()
  list_names <- names(.initial_prefs)
  nms <- c(dot_names, list_names)
  nms <- nms[!nms %in% ""]
  if(length(nms)) {
    if( !.overwrite ) {
      nms <- nms[ !nms %in% preference$keys() ]
    }
    if(length(nms)) {
      if( .verbose ) {
        catgl("Initializing the following preference(s): \n{ paste(nms, collapse = '\n') }", level = "DEBUG")
      }

      default_vals <- as.list(.initial_prefs[list_names %in% nms])
      nms <- nms[nms %in% dot_names]

      for(nm in nms) {
        default_vals[[nm]] <- ...elt(which(dot_names == nm))
      }
      re$mset(.list = default_vals)
    }
  }

  re
}

