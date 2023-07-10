load_target <- function(src, local = TRUE){
  s <- source(src, local = local, echo = FALSE, verbose = FALSE, chdir = TRUE)
  target <- s$value
  if(!is.list(target)){
    stop("Script ", src, " must need to end with a list.")
  }
  target
}



#' @rdname rave-pipeline
#' @export
load_targets <- function(..., env = NULL){
  if(!is.environment(env)) {
    env <- TRUE
  }
  targets <- lapply(c(...), load_target, local = env)
  do.call("c", targets)
}

activate_pipeline <- function(pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")) {
  if(!dir.exists(pipe_dir)){
    stop("`pipe_dir` must be the directory of pipeline files.")
  }

  pipe_dir <- normalizePath(pipe_dir)
  common_script <- file.path(pipe_dir, "common.R")
  entry_point_path <- file.path(pipe_dir, "make-main.R")

  if(!file.exists(common_script)){
    stop("Cannot find `common.R` in the pipeline.")
  }

  if(!file.exists(entry_point_path)){
    stop("Cannot find entry point (`make-main.R`) in the pipeline.")
  }

  parent_frame <- parent.frame()
  current <- Sys.getenv("TAR_PROJECT", "main")
  wd <- normalizePath(getwd())
  do.call(
    on.exit, list(bquote({
      setwd(.(wd))
      Sys.setenv("TAR_PROJECT" = .(current))
    }), add = TRUE, after = TRUE),
    envir = parent_frame
  )
  setwd(pipe_dir)

  tmpenv <- new.env(parent = globalenv())
  source("common.R", local = tmpenv)

  # Sys.setenv("RAVE_PIPELINE" = pipe_dir)

  attr(pipe_dir, "target_name") <- tmpenv$target_name
  attr(pipe_dir, "target_script") <- tmpenv$target_script
  attr(pipe_dir, "target_directory") <- tmpenv$target_directory
  pipe_dir
}

#' @rdname rave-pipeline
#' @export
pipeline_target_names <- function(pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")){
  pipe_dir <- activate_pipeline(pipe_dir)

  # find targets that are not in the main
  script <- attr(pipe_dir, "target_script")

  all_targets <- load_target("make-main.R")
  target_names <- unlist(lapply(all_targets, function(x){
    x$settings$name
  }))
  target_names
}

#' @rdname rave-pipeline
#' @export
pipeline_debug <- function(
  quick = TRUE,
  env = parent.frame(),
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  skip_names
){
  pipe_dir <- activate_pipeline(pipe_dir)

  # find targets that are not in the main
  script <- attr(pipe_dir, "target_script")

  main_targets <- load_target(script)
  all_targets <- load_target("make-main.R")

  if(quick){
    if(missing(skip_names)){
      main_target_names <- unlist(lapply(main_targets, function(x){
        x$settings$name
      }))
      target_names <- unlist(lapply(all_targets, function(x){
        x$settings$name
      }))
      skip_names <- unname(target_names[!target_names %in% main_target_names])
    }
  } else {
    skip_names <- NULL
  }

  if(quick){
    # build with targets
    do.call(targets::tar_make, list(
      callr_function = NULL,
      envir = env, names = as.call(
        list(quote(targets::all_of), skip_names)
      )
    ))
  }

  nms <- names(all_targets)

  w <- getOption("width", 80)

  started <- Sys.time()

  for(ii in seq_along(all_targets)){
    if(length(nms) < ii || nms[[ii]] == ""){
      nm <- "(No name)"
    } else {
      nm <- stringr::str_split(nms[[ii]], "_")[[1]]
      nm[[1]] <- stringr::str_to_sentence(nm[[1]])
      nm <- paste(nm, collapse = " ")
    }

    ...t <- all_targets[[ii]]
    name <- ...t$settings$name
    if(name %in% skip_names){
      v <- targets::tar_read_raw(name)
      assign(name, v, envir = env)
    } else {

      r <- w - stringr::str_length(nm) - 14
      if( r <= 2 ){ r <- 2 }
      nm <- paste(c(
        sprintf(" (%.2f s) ", dipsaus::time_delta(started, Sys.time())),
        rep("-", r), " ", nm, "\n"), collapse = "")
      catgl(nm, level = "INFO")

      counter <- Sys.time()

      {
        message("Evaluating -> ", name, "\r", appendLF = FALSE)
        v <- eval(...t$command$expr, new.env(parent = env))
        assign(name, v, envir = env)
        str <- deparse1(v)
        str <- stringr::str_replace_all(str, "\t|\n", "  ")
        r <- w - stringr::str_length(name) - 25
        if(r < 0){
          r <- w - 5
          s <- "`{name}` <- \n    "
        } else {
          s <- "`{name}` <- "
        }
        str <- stringr::str_sub(str, end = r)
        delta <- dipsaus::time_delta(counter, Sys.time())
        catgl(sprintf("[+%6.2f s] ", delta), s, str, "\n")
        counter <- Sys.time()
      }
    }
  }
}


#' @rdname rave-pipeline
#' @export
pipeline_eval <- function(names, env = new.env(parent = parent.frame()),
                          pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
                          settings_path = file.path(pipe_dir, "settings.yaml")) {
  force(env)
  pipe_dir <- activate_pipeline(pipe_dir)

  # find targets that are not in the main
  script <- attr(pipe_dir, "target_script")

  main_targets <- load_target(script)
  all_targets <- load_target("make-main.R")

  nms <- names(all_targets)
  tnames <- unname(unlist(lapply(all_targets, function(t){ t$settings$name })))
  names <- names[names %in% tnames]

  # Load shared functions into env
  shared_libs <- list.files(file.path(pipe_dir, "R"), pattern = "^shared-.*\\.R",
                            full.names = TRUE, ignore.case = TRUE)
  shared_libs <- sort(shared_libs)

  lapply(shared_libs, function(f) {
    source(file = f, local = env, chdir = TRUE)
  })


  # if(dir.exists(file.path(pipe_dir, "py"))) {
  #   pipeline_py_module(pipe_dir = pipe_dir,
  #                      convert = FALSE)
  # }

  if(file.exists(settings_path)) {
    input_settings <- yaml::read_yaml(settings_path)
    input_settings <- input_settings[names(input_settings) %in% tnames]
    if(length(input_settings)) {
      lapply(names(input_settings), function(nm) {
        env[[nm]] <- resolve_pipeline_settings_value( input_settings[[nm]], pipe_dir = pipe_dir )
      })
    }
  }
  # print(ls(env))

  missing_names <- tnames[!tnames %in% c(names, attr(as.list(env), "names"))]
  if(length(missing_names)) {
    list2env(
      pipeline_read(var_names = missing_names),
      envir = env
    )
  }

  w <- getOption("width", 80)

  all_starts <- Sys.time()

  lapply(names, function(name) {

    ii <- which(tnames == name)
    if(length(nms) < ii || nms[[ii]] == ""){
      nm <- sprintf("[%s]", name)
    } else {
      nm <- strsplit(nms[[ii]], "_")[[1]]
      nm[[1]] <- stringr::str_to_sentence(nm[[1]])
      nm <- paste(nm, collapse = " ")
      nm <- sprintf("[%s] (%s)", name, nm)
    }

    nm <- paste(c(
      sprintf(" (%.2f s) ", dipsaus::time_delta(all_starts, Sys.time())),
      rep("-", 2), " ", nm, "\n"), collapse = "")
    catgl(nm, level = "INFO")

    tar_obj <- all_targets[[ii]]
    started <- Sys.time()
    v <- eval(tar_obj$command$expr, new.env(parent = env))
    assign(name, v, envir = env)
    ended <- Sys.time()

    msg <- sprintf(
      "%s [%.2f sec, %s] - %s",
      name, dipsaus::time_delta(started, ended, units = "secs"),
      dipsaus::to_ram_size(utils::object.size(v)),
      paste(class(v), collapse = ", ")
    )
    catgl(msg, .envir = emptyenv(), level = "DEFAULT")
    NULL
  })
  env
}

# May be removed later if not really useful
pipeline_run_interactive <- function(
  names, skip_names, env = parent.frame(),
  pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
){
  pipe_dir <- activate_pipeline(pipe_dir)

  # find targets that are not in the main
  script <- attr(pipe_dir, "target_script")

  main_targets <- load_target(script)
  all_targets <- load_target("make-main.R")

  main_target_names <- unlist(lapply(main_targets, function(x){
    x$settings$name
  }))
  target_names <- unlist(lapply(all_targets, function(x){
    x$settings$name
  }))
  if(missing(skip_names)){
    skip_names <- unname(target_names[!target_names %in% main_target_names])
  }

  if(length(skip_names)){
    # build with targets
    do.call(targets::tar_make, list(
      callr_function = NULL,
      envir = env, names = skip_names
    ))
    for(nm in skip_names){
      assign(nm, pipeline_read(nm, pipe_dir = pipe_dir), envir = env)
    }
  }

  w <- getOption("width", 80)
  started <- Sys.time()

  nms <- names(all_targets)

  for(nm in names){
    ii <- which(target_names == nm)
    if(length(ii)){

      desc <- nms[[ii]]
      if(desc == "") {
        desc <- "(No name)"
      } else {
        desc <- stringr::str_split(nms[[ii]], "_")[[1]]
        desc[[1]] <- stringr::str_to_sentence(desc[[1]])
        desc <- paste(desc, collapse = " ")
      }

      ...t <- all_targets[[ii]]
      name <- ...t$settings$name
      r <- w - stringr::str_length(nm) - 14
      if( r <= 2 ){ r <- 2 }
      nm <- paste(c(
        sprintf(" (%.2f s) ", dipsaus::time_delta(started, Sys.time())),
        rep("-", r), " ", nm, "\n"), collapse = "")
      catgl(nm, level = "INFO")

      counter <- Sys.time()
      {
        message("Evaluating -> ", name, "\r", appendLF = FALSE)
        expr <- ...t$command$expr
        tryCatch({
          v <- eval(expr, new.env(parent = env))
          assign(name, v, envir = env)
          str <- deparse1(v)
          str <- stringr::str_replace_all(str, "\t|\n", "  ")
          r <- w - stringr::str_length(name) - 25
          if(r < 0){
            r <- w - 5
            s <- "`{name}` <- \n    "
          } else {
            s <- "`{name}` <- "
          }
          str <- stringr::str_sub(str, end = r)
          delta <- dipsaus::time_delta(counter, Sys.time())
          catgl(sprintf("[+%6.2f s] ", delta), s, str, "\n")
        }, error = function(e){
          e$call <- expr
          stop(e)
        })
        counter <- Sys.time()
      }
    }

  }
}

#' @rdname rave-pipeline
#' @export
pipeline_visualize <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  glimpse = FALSE, targets_only = TRUE,
  shortcut = FALSE, zoom_speed = 0.1, ...
){
  pipe_dir <- activate_pipeline(pipe_dir)
  if(glimpse) {
    targets::tar_glimpse(targets_only = targets_only, shortcut = shortcut,
                         zoom_speed = zoom_speed, ...)
  } else {
    targets::tar_visnetwork(targets_only = targets_only, shortcut = shortcut,
                            zoom_speed = zoom_speed, ...)
  }
}


pipeline_dependency_graph <- function(pipeline_path, targets_only = TRUE, shortcut = FALSE,
                                      zoom_speed = 0.1, aspect_ratio = 1.5, main = "",
                                      node_size = 30, label_size = node_size, glimpse = FALSE) {

  require_package("visNetwork")

  widget <- callr::r(
    function(pipeline_path, targets_only, shortcut, zoom_speed, aspect_ratio, main, label_size, node_size, glimpse) {
      raveio <- asNamespace("raveio")
      targets <- asNamespace("targets")
      visNetwork <- asNamespace("visNetwork")

      pipeline_path <- raveio$activate_pipeline(pipeline_path)

      target_names <- raveio$pipeline_target_names(pipeline_path)
      target_descr <- sapply(strsplit(names(target_names), "_"), function(x){
        x <- x[x != ""]
        if(!length(x)) { return(NA) }
        substr(x[[1]], start = 1, stop = 1) <- toupper(
          substr(x[[1]], start = 1, stop = 1)
        )
        paste(x, collapse = " ")
      })
      descr <- data.frame(
        name = target_names,
        rave_description = target_descr
      )

      target_script <- attr(pipeline_path, "target_script")
      # load & combine pipelines
      target <- raveio$load_targets(target_script)
      target <- targets$tar_as_pipeline(target)

      store <- targets$tar_config_get("store")
      names <- targets$pipeline_get_names(target)
      reporter <- targets$tar_config_get("reporter_outdated")


      if( glimpse ) {
        network <- targets$glimpse_init(
          pipeline = target, meta = targets$meta_init(path_store = store),
          progress = targets$progress_init(path_store = store), targets_only = targets_only,
          names = names, shortcut = shortcut, allow = NULL,
          exclude = ".Random.seed")
      } else {
        network <- targets$inspection_init(
          pipeline = target, meta = targets$meta_init(path_store = store),
          progress = targets$progress_init(path_store = store), targets_only = targets_only,
          names = names, shortcut = shortcut, allow = NULL,
          exclude = ".Random.seed", outdated = TRUE, reporter = reporter)
      }
      visual <- targets$visnetwork_init(
        network = network, label = NULL,
        level_separation = 100, degree_from = 1L,
        degree_to = 1L, zoom_speed = zoom_speed)


      level <- visual$network$vertices$level
      height <- max(c(table(level), 1))
      width <- max(c(level, 0)) + 1

      visual$level_separation <- height / width * 150 * aspect_ratio
      visual$update_network()
      visual$update_labels()
      visual$update_colors()
      visual$update_extra()
      visual$update_legend()

      vertices <- merge(visual$network$vertices, descr, by = "name", all.x = TRUE, all.y = FALSE)
      vertices$shape <- "hexagon"
      vertices$title <- sprintf(
        "Variable: %s%s%s",
        vertices$label,
        ifelse(is.na(vertices$rave_description), "", sprintf("<br>Description: %s", vertices$rave_description)),
        ifelse(is.na(vertices$bytes), "", sprintf("<br>Size:     %.1f MB", vertices$bytes / 1024))
      )
      edges <- visual$network$edges
      out <- visNetwork$visNetwork(nodes = vertices, edges = edges, main = main)
      out <- visNetwork$visNodes(out, physics = FALSE, size = node_size, font = list(size = label_size))
      out <- visNetwork$visEdges(out, smooth = list(type = "cubicBezier",
                                                     forceDirection = "horizontal"))
      out <- visNetwork$visOptions(
        graph = out, collapse = TRUE,
        highlightNearest = list(
          enabled = TRUE, algorithm = "hierarchical",
          degree = list(from = min(visual$degree_from, nrow(vertices)),
                        to = min(visual$degree_to, nrow(vertices)))))
      out <- visNetwork$visLegend(graph = out, useGroups = FALSE, enabled = !glimpse,
                                   addNodes = visual$legend, ncol = 1L, position = "right", width = 0.1,
                                   zoom = FALSE)
      out <- visNetwork$visPhysics(graph = out, stabilization = FALSE)
      out <- visNetwork$visInteraction(graph = out, zoomSpeed = visual$zoom_speed)
      widget <- visNetwork$visHierarchicalLayout(
        edgeMinimization = TRUE,
        graph = out, direction = "LR", levelSeparation = visual$level_separation, sortMethod = "directed"
      )
      visual$visual <- widget
      return( widget )
    },
    args = list(
      pipeline_path = pipeline_path, targets_only = targets_only,
      shortcut = shortcut, zoom_speed = zoom_speed, aspect_ratio = aspect_ratio,
      main = main, node_size = node_size, label_size = label_size, glimpse = glimpse
    )
  )

  return(widget)
}


#' @rdname rave-pipeline
#' @export
pipeline_progress <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  method = c("summary", "details", "custom"),
  func = targets::tar_progress_summary
){
  method <- match.arg(method)
  activate_pipeline(pipe_dir)

  switch (
    method,
    "summary" = targets::tar_progress_summary(),
    "details" = targets::tar_progress(),
    {
      func()
    }
  )
}

#' @rdname rave-pipeline
#' @export
pipeline_fork <- function(
  src = Sys.getenv("RAVE_PIPELINE", "."),
  dest = tempfile(pattern = "rave_pipeline_"),
  filter_pattern = PIPELINE_FORK_PATTERN,
  activate = FALSE
){
  if(!dir.exists(src)){
    stop("pipeline_fork: `src` must be a pipeline directory")
  }
  if(!file.exists(file.path(src, "common.R"))){
    stop("pipeline_fork: `src/common.R` is missing")
  }
  if(!file.exists(file.path(src, "make-main.R"))){
    stop("pipeline_fork: `src/make-main.R` is missing")
  }


  fs <- list.files(src, include.dirs = TRUE, full.names = FALSE, pattern = filter_pattern, ignore.case = TRUE)

  dir_create2(dest)
  dest <- normalizePath(dest, mustWork = TRUE)
  file.copy(from = file.path(src, fs), to = dest, overwrite = TRUE,
            recursive = TRUE, copy.date = TRUE)

  if( activate ){
    pipeline_build(dest)
    Sys.setenv("RAVE_PIPELINE" = dest)
  }
  dest
}

#' @rdname rave-pipeline
#' @export
pipeline_build <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
){
  pipe_dir <- activate_pipeline(pipe_dir)
  configure_src <- file.path(pipe_dir, "configure.R")

  # build _targets.yaml
  source(configure_src, local = TRUE, chdir = TRUE)

  TRUE
}

#' @rdname rave-pipeline
#' @export
pipeline_read <- function(
  var_names,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  branches = NULL,
  ifnotfound = NULL
) {
  pipe_dir <- activate_pipeline(pipe_dir)
  if(length(var_names) > 1){
    re <- structure(lapply(var_names, function(vn){
      tryCatch({
        do.call(targets::tar_read,
                list(name = str2lang(vn), branches = branches))
      }, error = function(e){
        ifnotfound
      })
    }), names = var_names)
  } else {
    re <- tryCatch({
      do.call(targets::tar_read,
              list(name = str2lang(var_names), branches = branches))
    }, error = function(e){
      ifnotfound
    })
  }
  return(re)
}

#' @rdname rave-pipeline
#' @export
pipeline_vartable <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  targets_only = TRUE,
  complete_only = FALSE,
  ...
) {
  pipe_dir <- activate_pipeline(pipe_dir)
  tryCatch({
    targets::tar_meta(..., targets_only = targets_only,
                      complete_only = complete_only)
  }, error = function(e){
    NULL
  })
}

#' @rdname rave-pipeline
#' @export
pipeline_hasname <- function(
  var_names,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")
) {
  tbl <- pipeline_vartable(pipe_dir = pipe_dir)
  var_names %in% tbl$name
}

#' @rdname rave-pipeline
#' @export
pipeline_watch <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  targets_only = TRUE, ...
){
  pipe_dir <- activate_pipeline(pipe_dir)
  targets::tar_watch(..., targets_only = targets_only, display = 'graph')
}

#' @rdname rave-pipeline
#' @export
pipeline_create_template <- function(
  root_path, pipeline_name, overwrite = FALSE,
  activate = TRUE, template_type = c("rmd", 'r', 'rmd-bare', 'rmd-scheduler')
) {
  template_type <- match.arg(template_type)
  pipeline_name <- tolower(pipeline_name)
  stopifnot2(!pipeline_name %in% c("main", "imports", "initialize", "template"),
             msg = "Pipeline name cannot be `main`, `imports`, `template`, or `initialize`")
  pipe_path <- file.path(root_path, pipeline_name)
  if(dir.exists(pipe_path)){
    if(!overwrite){
      stop("Pipeline ", pipeline_name, " already exists at\n  ", pipe_path)
    } else {
      unlink(pipe_path, recursive = TRUE)
    }
  }
  dir_create2(pipe_path)
  pipe_path <- normalizePath(pipe_path)

  # create a skeleton template
  template_path <- system.file("rave-pipelines", sprintf("template-%s", template_type), package = 'raveio', mustWork = TRUE)
  fs_src <- list.files(template_path, recursive = FALSE,
                       include.dirs = TRUE, no.. = TRUE,
                       full.names = FALSE)

  fs_src <- fs_src[!grepl("\\.Rproj$", fs_src, perl = TRUE)]

  for(f in fs_src) {
    f_src <- file.path(template_path, f)
    if(dir.exists(f_src)) {
      file.copy(f_src, pipe_path, overwrite = overwrite,
                recursive = TRUE, copy.date = TRUE)
    } else {
      f_dst <- stringr::str_replace_all(f, "TEMPLATE", pipeline_name)
      file.copy(f_src, file.path(pipe_path, f_dst),
                overwrite = overwrite, copy.date = TRUE)
    }
  }
  fs_src <- list.files(template_path, recursive = TRUE, include.dirs = FALSE)
  fs_dst <- stringr::str_replace_all(fs_src, "TEMPLATE", pipeline_name)
  fs <- file.path(pipe_path, fs_dst)
  for(f in fs){
    s <- readLines(f)
    s <- stringr::str_replace_all(s, "TEMPLATE_PATH", pipe_path)
    s <- stringr::str_replace_all(s, "TEMPLATE", pipeline_name)
    s <- stringr::str_replace_all(s, "PROJECT_NAME", "demo")
    s <- stringr::str_replace_all(s, "SUBJECT_CODE", "DemoSubject")
    writeLines(s, f)
  }
  # settings <- yaml::read_yaml(file.path(pipe_path, "settings.yaml"))
  # settings$epoch <- "default"
  # settings$electrodes <- dipsaus::deparse_svec(14L)
  # settings$reference <- "default"

  # save_yaml(settings, file.path(pipe_path, "settings.yaml"))

  # build the pipeline
  pipeline_build(pipe_path)

  if(activate){
    Sys.setenv("RAVE_PIPELINE" = pipe_path)
  }
  return(pipe_path)
}

#' @rdname rave-pipeline
#' @export
pipeline_create_subject_pipeline <- function(
  subject, pipeline_name, overwrite = FALSE,
  activate = TRUE, template_type = c("rmd", 'r')
){
  template_type <- match.arg(template_type)
  pipeline_name <- tolower(pipeline_name)
  stopifnot2(!pipeline_name %in% c("main", "imports", "initialize", "template"),
             msg = "Pipeline name cannot be `main`, `imports`, `template`, or `initialize`")
  subject <- as_rave_subject(subject, strict = FALSE)
  pipe_path <- file.path(subject$pipeline_path, pipeline_name)
  if(!overwrite && dir.exists(pipe_path)){
    stop("Pipeline ", pipeline_name, " already exists at\n  ", pipe_path)
  }
  # create a skeleton template
  template_path <- system.file("rave-pipelines", sprintf("template-%s", template_type), package = 'raveio', mustWork = TRUE)
  fs_src <- list.files(template_path)
  fs_dst <- stringr::str_replace_all(fs_src, "TEMPLATE", pipeline_name)
  dir_create2(pipe_path)
  pipe_path <- normalizePath(pipe_path)
  file.copy(file.path(template_path, fs_src), file.path(pipe_path, fs_dst), overwrite = overwrite, copy.date = TRUE)

  fs <- file.path(pipe_path, fs_dst)
  for(f in fs){
    s <- readLines(f)
    s <- stringr::str_replace_all(s, "TEMPLATE_PATH", pipe_path)
    s <- stringr::str_replace_all(s, "TEMPLATE", pipeline_name)
    s <- stringr::str_replace_all(s, "PROJECT_NAME", subject$project_name)
    s <- stringr::str_replace_all(s, "SUBJECT_CODE", subject$subject_code)
    writeLines(s, f)
  }
  settings <- yaml::read_yaml(file.path(pipe_path, "settings.yaml"))
  if(length(subject$epoch_names)){
    settings$epoch <- subject$epoch_names[[1]]
  } else {
    settings$epoch <- "default"
  }

  if(length(subject$electrodes)){
    settings$electrodes <- dipsaus::deparse_svec(subject$electrodes)
  }

  if(length(subject$reference_names)){
    settings$reference <- subject$reference_names[[1]]
  } else {
    settings$reference <- "default"
  }

  save_yaml(settings, file.path(pipe_path, "settings.yaml"), sorted = TRUE)

  # build the pipeline
  pipeline_build(pipe_path)

  if(activate){
    Sys.setenv("RAVE_PIPELINE" = pipe_path)
  }
  return(pipe_path)
}

#' @rdname rave-pipeline
#' @export
pipeline_description <- function (file) {
  # file <- file.path(pipeline, 'DESCRIPTION')
  dcf <- read.dcf(file = file)
  if (nrow(dcf) < 1L) {
    stop(sprintf("DESCRIPTION file '%s' is corrupt",
                 file), domain = NA)
  }
  desc <- as.list(dcf[1, ])
  if ((length(desc) == 0)) {
    stop(sprintf("DESCRIPTION file '%s' is missing or broken", file), domain = NA)
  }
  attr(desc, "file") <- file

  class(desc) <- "packageDescription"
  desc
}


#' @name pipeline_settings_get_set
#' @title Get or change pipeline input parameter settings
#' @param key,... the character key(s) to get or set
#' @param default the default value is key is missing
#' @param constraint the constraint of the resulting value; if not \code{NULL},
#' then result must be within the \code{constraint} values, otherwise the
#' first element of \code{constraint} will be returned. This is useful to make
#' sure the results stay within given options
#' @param pipeline_path the root directory of the pipeline
#' @param pipeline_settings_path the settings file of the pipeline, must be
#' a 'yaml' file; default is \code{'settings.yaml'} in the current pipeline
#' @returns \code{pipeline_settings_set} returns a list of all the settings.
#' \code{pipeline_settings_get} returns the value of given key.
#' @export
pipeline_settings_set <- function(
  ...,
  pipeline_path = Sys.getenv("RAVE_PIPELINE", "."),
  pipeline_settings_path = file.path(pipeline_path, "settings.yaml")
){
  if(!file.exists(pipeline_settings_path)){
    stop("Cannot find settings file:\n  ", pipeline_settings_path)
  }
  settings <- load_yaml(pipeline_settings_path)
  args <- list(...)
  if( !length(args) ) { return(settings) }

  argnames <- names(args)
  if(!length(argnames) || "" %in% argnames) {
    stop("`pipeline_set`: all input lists must have names")
  }


  lapply(argnames, function(nm) {

    opts <- resolve_pipeline_settings_opt(settings[[nm]], strict = FALSE)

    if(!is.null(opts)) {
      # external settings
      # save external data
      pipeline_save_extdata(
        data = args[[nm]],
        name = opts$name,
        format = opts$format,
        overwrite = TRUE,
        pipe_dir = pipeline_path
      )
      return()
    }

    # otherwise replace settings directly
    settings[[nm]] <- args[[nm]]

  })
  # dipsaus::list_to_fastmap2(args, map = settings)
  tf <- tempfile()
  on.exit({ unlink(tf) })
  save_yaml(x = settings, file = tf, sorted = TRUE)
  file.copy(from = tf, to = pipeline_settings_path,
            overwrite = TRUE, recursive = FALSE)
  settings

}

`%OF%` <- function(lhs, rhs){
  if(length(rhs)){ de <- rhs[[1]] } else { de <- rhs }
  lhs <- lhs[!is.na(lhs)]
  if(!length(lhs)){ return(de) }
  sel <- lhs %in% rhs
  if(any(sel)){ return(lhs[sel][[1]]) }
  return(de)
}

resolve_pipeline_settings_opt <- function(value, strict = TRUE) {

  if(isTRUE(is.character(value)) && length(value) == 1 && !is.na(value) &&
     grepl("^\\$\\{EXTDATA\\-SETTINGS\\|(.*)\\}$", value)) {

    # this value should be stored as external data
    value <- gsub(pattern = "(^\\$\\{EXTDATA\\-SETTINGS\\||\\}$)", "", x = value, ignore.case = FALSE)
    value <- strsplit(value, "\\|")[[1]]
    data_name <- value[[1]]

    if(nchar(data_name) && grepl("[a-zA-Z0-9]{1,}[a-zA-Z0-9_\\.-]{0,}", data_name)) {

      data_format <- "rds"
      if(length(value >= 2) && tolower(value[[2]]) %in% c("json", "yaml", "csv", "fst", "rds")) {
        data_format <- tolower(value[[2]])
      }
      return(list(
        name = data_name,
        format = data_format
      ))
    } else {
      if( strict ) {
        stop("Cannot resolve the pipeline external settings: invalid data name: ", data_name)
      }
      return(NULL)
    }
  } else {
    if( strict ) {
      stop("Cannot resolve the pipeline external settings: invalid settings: ", value)
    }
    return(NULL)
  }
}

resolve_pipeline_settings_value <- function(value, pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")) {

  opts <- resolve_pipeline_settings_opt(value, strict = FALSE)
  if(is.null(opts) || !is.list(opts)) {
    return( value )
  }

  opts$error_if_missing <- FALSE
  opts$default_if_missing <- structure(list(), class = "key_missing")
  opts$pipe_dir <- pipe_dir

  value <- do.call(pipeline_load_extdata, opts)

  if(!is.null(value)) {
    cls <- class(value)
    if( !"raveio-pipeline-extdata" %in% cls ) {
      class(value) <- c("raveio-pipeline-extdata", cls)
    }
    attr(value, "raveio-pipeline-extdata-opts") <- opts[c("name", "format")]
  }
  return( value )
}

#' @rdname pipeline_settings_get_set
#' @export
pipeline_settings_get <- function(
  key, default = NULL, constraint = NULL,
  pipeline_path = Sys.getenv("RAVE_PIPELINE", "."),
  pipeline_settings_path = file.path(pipeline_path, "settings.yaml")) {
  if(!file.exists(pipeline_settings_path)){
    stop("Cannot find settings file:\n  ", pipeline_settings_path)
  }

  settings <- load_yaml(pipeline_settings_path)

  if(missing(key)) {
    nms <- names(settings)
  } else {
    nms <- key
  }


  if(missing(key)){
    lapply(names(settings), function(nm) {
      if(nm != "") {
        settings[[nm]] <- resolve_pipeline_settings_value( settings[[nm]], pipe_dir = pipeline_path )
      }
    })
    return( settings )
  }
  if(!settings$`@has`(key)){
    re <- default
  } else {
    re <- resolve_pipeline_settings_value( settings[[key]], pipe_dir = pipeline_path )
    if(inherits(re, "key_missing")) {
      re <- default
    }
  }

  if(length(constraint)){
    re <- re %OF% constraint
  }
  re

}


#' @rdname rave-pipeline
#' @export
pipeline_load_extdata <- function(
  name, format = c("auto", "json", "yaml", "csv", "fst", "rds"),
  error_if_missing = TRUE, default_if_missing = NULL,
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."), ...
) {
  path <- file.path(pipe_dir, "data")
  format <- match.arg(format)
  if(format == "auto") {
    fs <- list.files(path, recursive = FALSE)
    name2 <- sprintf("%s.%s", name, c("json", "yaml", "csv", "fst", "rds"))
    fs <- fs[fs %in% name2]
  } else {
    fs <- sprintf("%s.%s", name, format)
  }
  if(!length(fs)) {
    if( error_if_missing ) {
      stop("Pipeline: data [", name, "] is missing")
    } else {
      return(default_if_missing)
    }
  }
  fs <- fs[[1]]
  file <- file.path(path, fs)

  if(!file.exists(file)) {
    if( error_if_missing ) {
      stop("Pipeline: data [", name, "] is missing")
    } else {
      return(default_if_missing)
    }
  }

  ext <- strsplit(file, "\\.")[[1]]
  ext <- tolower(ext[[length(ext)]])

  re <- tryCatch({
    switch(
      ext,
      "json" = { load_json(con = file, ...) },
      "yaml" = { load_yaml(file = file, ...) },
      "csv" = { utils::read.csv(file = file, ...) },
      "fst" = { load_fst(path = file, ...) },
      "rds" = { readRDS(file = file, ...) },
      { stop("Unsupported file format") }
    )
  }, error = function(e) {
    if( error_if_missing ) {
      stop("Pipeline: cannot load data [", name, "] with given format [", ext, "]. The file format is inconsistent or file is corrupted.")
    } else {
      return(default_if_missing)
    }
  })

  return(re)
}

#' @rdname rave-pipeline
#' @export
pipeline_save_extdata <- function(
  data, name, format = c("json", "yaml", "csv", "fst", "rds"),
  overwrite = FALSE, pipe_dir = Sys.getenv("RAVE_PIPELINE", "."), ...
) {
  format <- match.arg(format)

  # pipe_dir <- activate_pipeline(pipe_dir)
  path <- file.path(pipe_dir, "data")
  path <- dir_create2(path)
  paths <- file.path(path,  sprintf("%s.%s", name, c("json", "yaml", "csv", "fst", "rds")))

  if(any(file.exists(paths))) {
    if( overwrite ) {
      paths <- paths[file.exists(paths)]
      for(f in paths) {
        unlink(f)
      }
    } else {
      stop("Pipeline: Cannot save data because the data name [", name, "] already exists.")
    }
  }
  path <- file.path(path, sprintf("%s.%s", name, format))
  switch(
    format,
    "json" = {
      save_json(x = data, con = path, serialize = TRUE, ...)
    },
    "yaml" = {
      save_yaml(x = data, file = path, sorted = TRUE, ...)
    },
    "csv" = {
      utils::write.csv(x = data, file = path, ...)
    },
    "fst" = {
      save_fst(x = data, path = path, ...)
    },
    "rds" = {
      saveRDS(object = data, file = path, ...)
    },
    { stop("Unsupported file format") }
  )
  invisible(path)
}


#' @rdname rave-pipeline
#' @export
pipeline_shared <- function(pipe_dir = Sys.getenv("RAVE_PIPELINE", ".")) {

  # try to get environment from targets
  env <- callr::r(
    function(pipe_dir) {
      shared_env <- new.env()
      runtime_env <- new.env()
      runtime_env$pipe_dir <- pipe_dir
      runtime_env$shared_env <- shared_env

      with(runtime_env, {
        raveio <- asNamespace("raveio")
        pipe_dir <- raveio$activate_pipeline(pipe_dir)
        target_script <- attr(pipe_dir, "target_script")

        # shared_libs <-
        #   list.files(
        #     file.path(pipe_dir, "R"),
        #     pattern = "^shared-.*\\.R",
        #     full.names = TRUE,
        #     ignore.case = TRUE
        #   )
        #
        #
        # lapply(sort(shared_libs), function(f) {
        #   source(file = f,
        #          local = shared_env,
        #          chdir = TRUE)
        # })

        # load & combine pipelines
        raveio$load_targets(target_script, env = shared_env)

      })

      return(shared_env)
    },
    args = list(pipe_dir = pipe_dir),
    cmdargs = c("--slave", "--no-save", "--no-restore")
  )
  return( env )

}


pipeline_py_info <- function(pipe_dir = Sys.getenv("RAVE_PIPELINE", "."), must_work = NA) {

  pipe_dir <- normalizePath(pipe_dir, mustWork = TRUE)
  env_yaml <- file.path(pipe_dir, "py", c("rave-py-submodule.yaml", "rave-py-submodule.yml"))
  env_yaml <- env_yaml[file.exists(env_yaml)]
  if(!length(env_yaml)) {
    msg <- sprintf("Unable to find python sub-module for the pipeline: no `rave-py-submodule.yaml` found. (pipeline: %s)", pipe_dir)
    if(is.na(must_work)) {
      warning(msg)
    } else if (must_work) {
      stop(msg)
    }
    return()
  }

  py_pkg_name <- NULL
  tryCatch({
    env_yaml <- load_yaml(env_yaml[[1]])
    py_pkg_name <- env_yaml$name
  }, error = function(e) {
    stop("Unable to load python sub-module for the pipeline: cannot parse `rave-py-submodule.yaml`")
  })

  if(!length(py_pkg_name)) {
    stop("Unable to find name for python sub-module from `rave-py-submodule.yaml`")
  }

  module_path <- file.path(pipe_dir, "py", py_pkg_name)
  if(!dir.exists(module_path)) {
    stop("Unable to load python sub-module: module [",
         py_pkg_name, "] is not found under the `py` folder!")
  }

  list(
    pipeline_path = pipe_dir,
    module_path = module_path,
    target_path = file.path(module_path, "rave_pipeline_adapters"),
    module_name = py_pkg_name
  )
}

pipeline_py_module <- function(
    pipe_dir = Sys.getenv("RAVE_PIPELINE", "."), must_work = NA,
    convert = FALSE) {

  pipe_dir <- normalizePath(pipe_dir, mustWork = TRUE)
  info <- pipeline_py_info(pipe_dir = pipe_dir, must_work = must_work)

  py_pkg_name <- info$module_name

  cwd <- getwd()
  on.exit({ setwd(cwd) }, add = TRUE, after = TRUE)
  pydir <- file.path(pipe_dir, "py")
  setwd(pydir)

  py_module <- rpymat::import(py_pkg_name, convert = convert, delay_load = FALSE)
  setwd(cwd)

  py_module
}
