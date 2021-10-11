# dest <- file.path(raveio:::R_user_dir('raveio', "data"), "pipelines")
# raveio::dir_create2(dest)
# directory <- '~/Dropbox/projects/rave-pipelines/'
# pipeline_install_bare(directory, dest, upgrade = FALSE)

pipeline_install_directory <- function(
  directory, dest, upgrade = TRUE, force = FALSE, ...){

  directory <- normalizePath(directory, mustWork = TRUE)

  # find RAVE-CONFIG
  config_path <- file.path(directory, "DESCRIPTION")

  if(!file.exists(config_path)){
    stop("A RAVE pipeline must contains a RAVE-CONFIG or DESCRIPTION file")
  }

  desc <- pipeline_description(config_path)

  if(!length(desc$Type)){
    stop("Cannot find `type` in the configuration file. ")
  }
  type <- desc$Type[[1]]
  remotes::install_deps(directory, upgrade = upgrade, force = force, ...)

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
      if(!force){
        stop("Pipeline ", desc$Package, " - version ", desc$Version,
             ' already exists. Please use `force=TRUE` to force install')
      }
      unlink(pipeline_root, recursive = TRUE, force = TRUE)
    }
    dir_create2(pipeline_root)

    fs <- list.files(directory, all.files = TRUE, full.names = FALSE, recursive = FALSE, include.dirs = FALSE, no.. = TRUE)

    file.copy(file.path(directory, fs), pipeline_root, recursive = TRUE, copy.date = TRUE)

    # Update version file
    version_file <- file.path(dest, desc$Package, "versions.yaml")
    save_yaml(desc, version_file)
  }

}

#' @rdname rave-pipeline
#' @export
pipeline_root <- local({
  root <- NULL
  function(root_path){
    if(!missing(root_path)){
      if(is.na(root_path)){ stop("pipeline root cannot be NA") }
      if('.' %in% root_path){
        root_path <- root_path[root_path != '.']
        root <<- c(".", normalizePath(root_path))
      } else {
        root <- normalizePath(root_path)
      }
    } else {
      if(is.null(root)){
        root <<- c(".", file.path(R_user_dir('raveio', "data"), "pipelines"))
      }
    }
    root
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

