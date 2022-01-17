# dest <- file.path(raveio:::R_user_dir('raveio', "data"), "pipelines")
# raveio::dir_create2(dest)
# directory <- '~/Dropbox/projects/rave-pipelines/'
# pipeline_install_bare(directory, dest, upgrade = FALSE)

pipeline_install_directory <- function(
  directory, dest, upgrade = FALSE, force = FALSE, ...){

  directory <- normalizePath(directory, mustWork = TRUE)

  # find RAVE-CONFIG
  config_path <- file.path(directory, c("RAVE-CONFIG", "DESCRIPTION"))
  config_path <- config_path[file.exists(config_path)]

  if(!length(config_path)){
    stop("A RAVE pipeline must contains a RAVE-CONFIG or DESCRIPTION file")
  }
  config_path <- config_path[[1]]

  desc <- pipeline_description(config_path)

  if(!length(desc$Type)){
    stop("Cannot find `type` in the configuration file. ")
  }
  type <- desc$Type[[1]]
  tmp_dir <- tempfile()
  dir_create2(tmp_dir)
  on.exit({
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)
  file.copy(config_path, file.path(tmp_dir, "DESCRIPTION"), overwrite = TRUE, recursive = FALSE)
  remotes::install_deps(tmp_dir, upgrade = upgrade, force = force, ...)

  if( length(desc$InteractiveModules) ) {
    modules <- strsplit(desc$InteractiveModules, "[,\n]+")[[1]]

    module_yaml <- file.path(directory, "modules.yaml")
    module_settings <- list()
    if(file.exists(module_yaml)){
      settings <- load_yaml(module_yaml)
      module_settings <- as.list(settings$modules)
    }

    modules <- lapply(modules, function(module){
      path <- file.path(directory, module)
      if(!dir.exists(path)){
        warning(glue("Unable to find module { module }."))
        return()
      }
      module_id <- basename(path)
      module_info <- as.list(module_settings[[module_id]])
      if(!length(module_info$label)){
        module_label <- gsub("[-_]+", " ", module_id)
        if(nchar(module_label)){
          substr(module_label, 1, 1) <- toupper(substr(module_label, 1, 1))
        }
        module_info$label <- module_label
      }
      module_info
      list(
        module_id = module_id,
        module_info = module_info,
        module_path = path
      )
    })
    # copy to shidashi template
    template_path <- file.path(R_user_dir('raveio', 'data'), "shidashi_modules")
    module_path <- file.path(template_path, "modules")
    dir_create2(module_path)

    settings <- dipsaus::fastmap2()
    module_yaml <- file.path(template_path, "modules.yaml")
    if(file.exists(module_yaml)) {
      load_yaml(module_yaml, map = settings)
    }
    module_settings <- list()

    for(module in modules){

      module_target <- file.path(module_path, module$module_id)
      if(dir.exists(module_target)){
        unlink(module_target, recursive = TRUE)
      }
      file.copy(module$module_path, module_path, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)

      module_settings[[module$module_id]] <- module$module_info

    }

    settings$modules <- module_settings

    save_yaml(settings, module_yaml)
  }

  if( desc$Type == "rave-pipeline-collection" ){
    # install sub-versions if possible
    if(length(desc$SubPipelines)){
      sub_pipes <- strsplit(desc$SubPipelines, "[,\n]+")[[1]]
      for(pname in sub_pipes){
        pdir <- file.path(directory, pname)
        pipeline_install_directory(pdir, dest, upgrade = FALSE, force = force, ...)
      }
    }
  } else {
    pipeline_root <- file.path(dest, desc$Package, desc$Version)
    if(dir.exists(pipeline_root)){
      # if(!force){
      #   stop("Pipeline ", desc$Package, " - version ", desc$Version,
      #        ' already exists. Please use `force=TRUE` to force install')
      # }
      unlink(pipeline_root, recursive = TRUE, force = TRUE)
    }
    dir_create2(pipeline_root)

    fs <- list.files(directory, all.files = TRUE, full.names = FALSE, recursive = FALSE, include.dirs = FALSE, no.. = TRUE)

    file.copy(file.path(directory, fs), pipeline_root, recursive = TRUE, copy.date = TRUE)

    # Update version file
    version_file <- file.path(dest, desc$Package, "versions.yaml")
    save_yaml(desc, version_file)
  }
  invisible()

}

#' @name pipeline_install
#' @title Install 'RAVE' pipelines
#' @param src pipeline directory
#' @param repo 'Github' repository in user-repository combination, for example,
#' \code{'dipterix/rave-pipeline'}
#' @param to installation path; choices are \code{'default'}, \code{'custom'},
#' \code{'workdir'}, and \code{'tempdir'}. Please specify pipeline root path
#' via \code{\link{pipeline_root}} when \code{'custom'} is used.
#' @param upgrade whether to upgrade the dependence; default is \code{FALSE}
#' for stability, however, it is highly recommended to upgrade your
#' dependencies
#' @param force whether to force installing the pipelines
#' @param ... other parameters not used
#' @return nothing
#' @export
pipeline_install_local <- function(
  src, to = c("default", "custom", "workdir", "tempdir"),
  upgrade = FALSE, force = FALSE, ...
) {
  src <- normalizePath(src, mustWork = TRUE)
  stopifnot2(dir.exists(src), msg = "`pipeline_install_local`: `src` must be a valid directory")
  to <- match.arg(to)
  switch (
    to,
    "custom" = {
      dest <- pipeline_root()
      if(length(dest) > 1){
        dest <- dest[dest != "."]
      }
      dest <- normalizePath(dest[[1]])
    },
    "workdir" = {
      dest <- normalizePath(".")
    },
    "tempdir" = {
      dest <- normalizePath(tempdir(check = TRUE))
    },
    {
      dest <- normalizePath(file.path(R_user_dir('raveio', "data"), "pipelines"), mustWork = FALSE)
    }
  )

  pipeline_install_directory(directory = src, dest = dest, upgrade = upgrade, force = force, ...)

}

#' @rdname pipeline_install
#' @export
pipeline_install_github <- function(
  repo, to = c("default", "custom", "workdir", "tempdir"),
  upgrade = FALSE, force = FALSE, ...
) {
  to <- match.arg(to)
  args <- list(...)
  remote_argnames <- c("ref", "subdir", "auth_token", "sha", "host")
  remote_args <- args[names(args) %in% remote_argnames]
  remote_args$repo <- repo
  remote <- do.call(remotes::github_remote, remote_args)
  exdir <- tempfile()
  tarball <- remotes::remote_download(remote)
  on.exit({
    unlink(tarball)
    unlink(exdir, recursive = TRUE, force = TRUE)
  }, add = TRUE)


  utils::untar(tarball, exdir = exdir)

  src <- exdir
  conf_path <- file.path(src, c("RAVE-CONFIG", "DESCRIPTION"))
  if(!any(file.exists(conf_path))){
    srcs <- list.dirs(src, full.names = TRUE, recursive = FALSE)
    for(src in srcs){
      conf_path <- file.path(src, c("RAVE-CONFIG", "DESCRIPTION"))
      if(any(file.exists(conf_path))){
        break
      }
    }
  }

  args <- args[!names(args) %in% remote_argnames]
  args$src <- src
  args$to <- to
  args$upgrade <- upgrade
  args$force <- force
  do.call(pipeline_install_local, args)
}

#' @rdname rave-pipeline
#' @export
pipeline_root <- local({
  root <- NULL
  function(root_path){
    if(!missing(root_path)){
      if(any(is.na(root_path))){ stop("pipeline root cannot be NA") }
      if('.' %in% root_path){
        root_path <- root_path[root_path != '.']
        root <<- c(".", normalizePath(root_path, mustWork = FALSE))
      } else {
        root <<- normalizePath(root_path, mustWork = FALSE)
      }
      if(!any(dir.exists(root))){
        warning("The following pipeline root directories do not exist: \n  |> ", paste(root, collapse = "\n  |> "))
      }
    } else {
      if(is.null(root)){
        root <<- c(".", file.path(R_user_dir('raveio', "data"), "pipelines"))
      }
    }
    unique(root)
  }
})

#' @rdname rave-pipeline
#' @export
pipeline_list <- function(root_path = pipeline_root()){
  names <-
    unlist(lapply(
      root_path,
      list.dirs,
      full.names = FALSE,
      recursive = FALSE
    ))
  names <- names[!stringr::str_starts(names, "[.~_]")]
  names <- names[!names %in% c("R", "src", "inst", "man", "doc")]
  names <- names[vapply(names, function(nm){
    try({
      pipeline_find(nm, root_path = root_path)
      return(TRUE)
    }, silent = TRUE)
    return(FALSE)
  }, FALSE)]
  names
}

#' @rdname rave-pipeline
#' @export
pipeline_find <- function(name, root_path = pipeline_root()){

  paths <- file.path(root_path, name)
  paths <- paths[dir.exists(paths)]

  for(path in paths){
    path <- tryCatch({
      vpath <- file.path(path, "versions.yaml")
      if(file.exists(vpath)){
        # read version file
        v <- load_yaml(file.path(path, "versions.yaml"))
        path <- file.path(path, v$Version)
      }
      path <- activate_pipeline(path)
      return(path)
    }, error = function(e){
      NULL
    })
    if(!is.null(path)){
      return(path)
    }
  }
  stop("Cannot find RAVE pipeline `", name, "`. Have you installed it?")
}

#' @rdname rave-pipeline
#' @export
pipeline_attach <- function(name, root_path = pipeline_root()){
  path <- pipeline_find(name, root_path)
  Sys.setenv("RAVE_PIPELINE" = path)
}
