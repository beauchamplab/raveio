# Markdown engines

RAVE_KNITR_SUPPORTED_LANG <- c("R", "python")

check_knit_packages <- function(languages = c("R", "python")){
  pkgs <- c('knitr', 'rmarkdown')
  if("python" %in% languages){
    pkgs <- c(pkgs, 'rpymat')
  }
  # check knitr, rmarkdown, reticulate
  pkgs <- pkgs[!dipsaus::package_installed(pkgs)]

  if(length(pkgs)){
    if(!interactive()){
      stop("Package(s) ", paste(pkgs, collapse = ", "), " are required. Please run install.packages(c(", paste0('"', pkgs, '"', collapse = ", "), ")) to install them")
    }
    message("Package(s) ", paste(pkgs, collapse = ", "), " are required. ")
    ans <- utils::askYesNo("Do you want to install them?")
    if(!isTRUE(ans)){
      stop("User abort.")
    }
    install_cran(pkgs = pkgs, upgrade = FALSE)
  }


}

resolve_pipeline_error <- function(name, condition, expr = NULL) {
  if(interactive() || dipsaus::shiny_is_running()) {
    expr0 <- substitute(expr)
    if( !identical(expr0, str2lang(".__target_expr__.")) ) {
      expr <- expr0
    }
    if(!is.null(expr)) {
      expr <- deparse1(expr, collapse = "\n")
      catgl("Pipeline code: \n{expr}", level = "ERROR")
    }
  }

  entrace <- get0("entrace", envir = asNamespace("dipsaus"),
                  mode = "function", ifnotfound = stop, inherits = TRUE)
  entrace(condition)
  # condition <- rlang::cnd_entrace(condition)
  # rlang::cnd_signal(condition)

  # rlang::abort(
  #   message = sprintf("Cannot resolve pipeline target [%s]", name),
  #   parent = condition,
  #   trace = condition$trace,
  #   rave_error = list(
  #     name = name,
  #     message = condition$message,
  #     expression = expr
  #   )
  # )

  # in case
  stop(condition)
}

rave_knit_r <- function(export, code, deps = NULL, cue = "thorough", pattern = NULL,
                        format = NULL, ..., target_names = NULL){
  # code <- options$code
  code <- paste(c("{", code, "}"), collapse = "\n")
  expr <- parse(text = code)[[1]]

  if(is.null(deps) && !is.null(target_names)){
    # Fix the following issue before `targets`
    # package is willing to consider this edge-cases
    # https://github.com/ropensci/targets/issues/663
    env <- knitr::knit_global()
    deps <- globals::findGlobals(expr, envir = env)
    deps <- deps[deps %in% target_names]
  }

  if(is.character(pattern)){
    pattern <- parse(text = pattern)
  }

  generate_target(
    expr = expr, export = export, format = format, deps = deps,
    cue = cue, pattern = pattern, quoted = TRUE)
  # bquote(
  #   targets::tar_target_raw(
  #     name = .(export),
  #     command = quote({
  #       tryCatch({
  #         .(expr)
  #         return(.(str2lang(export)))
  #       }, error = function(e) {
  #         asNamespace("raveio")$resolve_pipeline_error(.(export), e, quote(.(expr)))
  #       })
  #     }),
  #     format = asNamespace("raveio")$target_format_dynamic(.(format), .(export)),
  #     deps = .(deps),
  #     cue = targets::tar_cue(.(cue)),
  #     pattern = .(pattern),
  #     iteration = "list"
  #   )
  # )
}

rave_knit_python <- function(export, code, deps = NULL, cue = "thorough", pattern = NULL, local = TRUE, ..., target_names = NULL){
  if(is.character(pattern)){
    pattern <- parse(text = pattern)
  }

  # save to python submodule
  indent_info <- guess_py_indent(code)

  # get python module information, the pipeline directory should be "."
  py_info <- pipeline_py_info(".", must_work = TRUE)

  # get shared path
  shared_scripts <- NULL
  common_script_path <- file.path(py_info$pipeline_path, "py", "knitr-common.py")
  if(file.exists(common_script_path)) {
    shared_scripts <- readLines(common_script_path)
  }


  rave_path_py <- file.path(py_info$module_path, "rave_pipeline_adapters")
  dir_create2(rave_path_py)
  path_target_py <- file.path(rave_path_py, sprintf("pipeline_target_%s.py", export))

  indent <- paste(rep(indent_info$char, indent_info$count), collapse = "")
  indented_code <- strsplit(paste(code, collapse = "\n"), "\n")[[1]]
  indented_code <- paste(sprintf("%s%s%s", indent, indent, indented_code), collapse = "\n")

  wrapped_code <- sprintf("\n%s\n\n", glue(r"(
from .. import shared
from . import RAVERuntimeException

def pipeline_target_{export}({paste(deps, collapse = ', ')}):
{indent}try:
{indented_code}
{indent}{indent}return {export}
{indent}except Exception as e:
{indent}{indent}return RAVERuntimeException(e)
)"))
  # write to `path_target_py`
  writeLines(c(shared_scripts, wrapped_code), path_target_py)

  bquote(
    targets::tar_target_raw(
      name = .(export),
      command = quote({

        .py_error_handler <- function(e, use_py_last_error = TRUE) {
          if( use_py_last_error ) {
            e2 <- asNamespace("reticulate")$py_last_error()
            if(!is.null(e2)) {
              e <- e2
            }
          }

          code <- .(code)
          stop(sprintf(
            "Target [%s] (python) encountered the following error: \n%s\nAnalysis pipeline code:\n# ---- Target python code: %s -----\n%s\n# ---------------------------------------",
            .(export), paste(e$message, collapse = "\n"),
            .(export), paste(code, collapse = "\n")
          ))
        }

        re <- tryCatch(
          expr = {
            .env <- environment()
            if(length(.(deps))) {
              args <- structure(
                names = .(deps),
                lapply(.(deps), get, envir = .env)
              )
            } else {
              args <- list()
            }
            module <- asNamespace("raveio")$pipeline_py_module(convert = FALSE, must_work = TRUE)
            target_function <- module$rave_pipeline_adapters[.(export)]
            re <- do.call(target_function, args)
            cls <- class(re)
            if(length(cls) && any(endsWith(cls, "rave_pipeline_adapters.RAVERuntimeException"))) {
              error_message <- rpymat::py_to_r(re$`__str__`())
              .py_error_handler(simpleError(error_message), use_py_last_error = FALSE)
            }
            return(re)
          },
          python.builtin.BaseException = .py_error_handler,
          python.builtin.Exception = .py_error_handler,
          py_error = .py_error_handler,
          error = function(e) {
            traceback(e)
            stop(e$message, call. = FALSE)
          }
        )

        return(re)
      }),
      deps = .(deps),
      cue = targets::tar_cue(.(cue)),
      pattern = .(pattern),
      iteration = "list",
      format = asNamespace("raveio")$target_format_dynamic(
        "user-defined-python", target_export = .(export))
    )
  )
}

rave_knitr_engine <- function(targets){

  python_engine <- knitr::knit_engines$get("python")
  if(inherits(python_engine, "knit_engine_rave_python")) {
    python_engine2 <- python_engine
    python_engine <- attr(python_engine2, "original_engine")
  } else {
    python_engine2 <- structure(
      function(options) {
        if(isTRUE(options$use_rave)) {
          options$engine <- "python"
          options$language <- "python"
          knitr::knit_engines$get("rave")(options)
        } else {
          python_engine(options)
        }
      },
      class = c("knit_engine_rave_python", "function"),
      original_engine = python_engine
    )
  }
  knitr::knit_engines$set(python = python_engine2)

  knitr::knit_engines$set("rave" = function(options) {
    # for R
    if(startsWith(options$export, "unnamed-chunk")){
      stop("RAVE pipeline target chunk must be named. For example:\n",
           '{r target_name, engine="RAVE-target" ...')
    }
    if(grepl("^[^A-Za-z0-9\\-_.]+$", options$export)){
      stop("Chunk label (target name) must be valid variable name that ONLY contains letters, digits, `_`, `-`, or `.`")
    }
    settings_names <- NULL
    if(file.exists("settings.yaml")){
      try({
        settings_names <- names(load_yaml("settings.yaml"))
      }, silent = TRUE)
    }

    if(startsWith(options$export, ".")){
      stop("Chunk export name cannot start with dot '.' (reserved). Violated export: `", options$export, "`. Please choose another variable name to export.")
    }

    if(options$export %in% settings_names){
      stop("Chunk with the same export target `", options$export, "` already exists in the `settings.yaml`. Please choose another variable name to export.")
    }

    existing_names <- sapply(targets, "[[", "export")
    if(options$export %in% existing_names){
      if(interactive()){
        old_options <- targets[[
          which(existing_names == options$export)[[1]]
        ]]
        if(!identical(options$label, old_options$label)){
          warning("Chunk with the same export target `", options$export, "` might have already existed. Cannot have two targets sharing the same export name. However, please ignore this warning if you are sure this is false positive.")
        }
      } else {
        stop("Chunk with the same export target `", options$export, "` already exists. Cannot have two targets sharing the same export name. Please consider renaming the exported variable name")
      }
    }

    lang <- options$language
    if(length(lang) != 1 || lang == "r"){
      lang <- "R"
    }

    if(! lang %in% RAVE_KNITR_SUPPORTED_LANG){
      stop("Chunk `", options$label, "` has invalid `language` options. Please choose from the following engines: \n  ", paste(RAVE_KNITR_SUPPORTED_LANG, collapse = ", "))
    }

    options$language <- lang
    if(length(options$depends)){
      options$deps <- unlist(strsplit(options$depends, "[, ]+"))
    }
    # assign('options', options, envir = globalenv())

    message("\nAdding target ", options$export, " [", lang, "]")
    targets$add(options)

    # real_engine <- knitr::knit_engines$get(lang)

    env <- knitr::knit_global()
    switch(
      lang,
      "R" = {
        real_engine <- knitr::knit_engines$get("R")
        # keep names
        nms <- c(ls(env, all.names = TRUE, sorted = FALSE), options$export)
        options$engine <- "r"
        res <- real_engine(options)
        nms2 <- ls(env, all.names = TRUE, sorted = FALSE)
        if(!options$export %in% nms2){
          stop("Cannot find variable to be exported in chunk ", options$label)
        }
        nms2 <- nms2[!nms2 %in% nms]
        if(length(nms2)){
          rm(list = nms2, envir = env, inherits = FALSE)
        }
      },
      "python" = {
        real_engine <- python_engine
        options$engine <- "python"
        py <- rpymat::import("__main__", convert = FALSE)
        for(nm in options$deps) {
          py[[ nm ]] <- get(nm, envir = env)
        }
        res <- real_engine(options)
        env[[options$export]] <- py[[options$export]]
      },
      {
        # not reach here
        stop("unsupported language")
      }
    )

    return(res)

  })

}

guess_py_indent <- function(code, default_count = 2L) {
  code <- paste(code, collapse = "\n")
  code <- strsplit(code, "\n")[[1]]
  white_space <- "[ \t]"

  indent_char <- " "

  code <- code[grepl(sprintf("^(%s+)", white_space), x = code)]

  indents <- default_count
  if(length(code)) {

    indent_char <- substr(code[[1]], start = 1L, stop = 1L)
    indents <- vapply(strsplit(code, white_space), function(x) {
      indents <- which(x == "")
      if(!1L %in% indents) { return(0L) }
      indents <- dipsaus::deparse_svec(indents, concatenate = FALSE)
      if(!length(indents)) { return(0L) }
      indents <- dipsaus::parse_svec(indents[[1]])
      return(as.integer(indents[[length(indents)]]))
    }, 0L)
    indents <- unique(indents[indents > 0])
    if(length(indents)) {
      indents <- min(indents)
    } else {
      indents <- default_count
    }
  }


  list(
    char = indent_char,
    count = indents
  )
}

rave_knitr_build <- function(targets, make_file){
  # generate targets
  targets <- as.list(targets)
  dependence_names <- unlist(lapply(targets, "[[", "export"))
  if(file.exists("settings.yaml")){
    settings <- as.list(load_yaml("settings.yaml"))
    dependence_names <- c(names(settings), dependence_names)
  }

  nms <- lapply(targets, "[[", "label")
  python_exports <- NULL

  exprs <- structure(
    lapply(targets, function(options){
      message("Building target ", options$export, " [", options$language, "]")
      switch(
        options$language,
        "R" = {
          options$target_names <- dependence_names
          quos <- do.call(rave_knit_r, options)
        },
        "python" = {
          python_exports <<- c(python_exports, options$export)
          quos <- do.call(rave_knit_python, options)
        },
        {
          stop("Unsupported programming language: ", options$language)
        }
      )
      quos
    }), names = nms
  )

  if(length(python_exports)) {
    py_info <- pipeline_py_info(".", must_work = TRUE)
    rave_path_py <- file.path(py_info$module_path, "rave_pipeline_adapters")
    dir_create2(rave_path_py)
    path_init_py <- file.path(rave_path_py, "__init__.py")

    py_script <- r"(
class RAVERuntimeException(object):
  original_exception = None
  def __init__(self, e):
    if isinstance(e, Exception):
      self.original_exception = e
    elif isinstance(e, str):
      self.original_exception = Exception(e)
    else:
      self.original_exception = Exception('Unknown error')
  def __str__(self):
    return '{}: {}'.format(type(self.original_exception).__name__, self.original_exception)

from .serializers import rave_serialize
from .serializers import rave_unserialize
)"
    writeLines(c(
      py_script,
      sprintf("from .pipeline_target_%s import pipeline_target_%s as %s", python_exports, python_exports, python_exports)
    ), con = path_init_py)

    path_serializer_py <- file.path(rave_path_py, "serializers.py")
    if(!file.exists(path_serializer_py)) {
      writeLines(
        con = path_serializer_py,
        text = r"(
def rave_serialize(x, path, name):
  '''
  Serialization function for serializing Python objects into a file
  parameters:
    - x:    python object to serialize
    - path: path prefix to store the object
    - name: pipeline target name
  '''
  raise NotImplementedError('Please implement `rave_serialize` in rave_pipeline_adapters.serializers')

def rave_unserialize(x, path, name):
  '''
  Unserialization function for restoring Python objects from a file
  parameters:
    - path: path prefix to store the object
    - name: pipeline target name
  '''
  raise NotImplementedError('Please implement `rave_unserialize` in rave_pipeline_adapters.serializers')

)"
      )
    }

  }

  if(file.exists("settings.yaml")){
    settings <- as.list(load_yaml("settings.yaml"))

    # to please CRAN check
    settings_path <- NULL
    nms <- names(settings)

    extras <- list()
    for(nm in nms) {

      opts <- resolve_pipeline_settings_opt(settings[[nm]], strict = FALSE)

      if(is.null(opts)) {

        # ordinary settings
        extras[[paste0("input_", nm)]] <- bquote(
          targets::tar_target_raw(
            .(nm),
            quote({
              settings[[.(nm)]]
            }),
            deps = "settings"
          )
        )

      } else {

        extras[[paste0("__extern_path_", nm)]] <- bquote(
          targets::tar_target_raw(
            .(sprintf("settings_path._%s_", nm)),
            .(sprintf("./data/%s.%s", opts$name, opts$format)),
            format = "file"
          )
        )

        extras[[paste0("input_", nm)]] <- bquote(
          targets::tar_target_raw(
            .(nm),
            quote({
              # settings[[.(nm)]]
              # asNamespace("raveio")$resolve_pipeline_settings_value( settings[[.(nm)]], "." )
              asNamespace("raveio")$pipeline_load_extdata(
                name = .(opts$name),
                format = .(opts$format),
                error_if_missing = FALSE,
                default_if_missing = structure(list(), class = "key_missing"),
                pipe_dir = "."
              )
            }),
            deps = .(sprintf("settings_path._%s_", nm))
          )
        )

      }

    }

    exprs <- c(
      list(
        "__Check_settings_file" = quote(
          targets::tar_target_raw(
            "settings_path",
            "settings.yaml",
            format = "file"
          )
        ),
        "__Load_settings" = quote(
          targets::tar_target_raw(
            "settings",
            quote({
              yaml::read_yaml(settings_path)
            }),
            deps = "settings_path",
            cue = targets::tar_cue("always")
          )
        )
      ), extras, exprs)
  }
  call <- as.call(c(list(quote(list)), exprs))
  call <- as.call(list(quote(`<-`), quote(...targets), call))
  # write to target file
  writeLines(c(
    "library(targets)",
    "library(raveio)",
    'source("common.R", local = TRUE, chdir = TRUE)',

    '._._env_._. <- environment()',
    'lapply(sort(list.files(',
    '  "R/", ignore.case = TRUE,',
    '  pattern = "^shared-.*\\\\.R", ',
    '  full.names = TRUE',
    ')), function(f) {',
    '  source(f, local = ._._env_._., chdir = TRUE)',
    '})',
    # 'if(dir.exists("py/")) {',
    # '  try({',
    # '    library(rpymat)',
    # '    rpymat::ensure_rpymat(verbose = FALSE)',
    # '    lapply(sort(',
    # '      list.files("py/", ignore.case = TRUE, ',
    # '                 pattern = "^shared-.*\\\\.py", ',
    # '                 full.names = TRUE)), ',
    # '      function(f) {',
    # '        f <- normalizePath(f, mustWork = TRUE)',
    # '        rpymat::run_script(f, work_dir = basename(f), local = FALSE, convert = FALSE)',
    # '      })',
    # '  })',
    # '}',
    'targets::tar_option_set(envir = ._._env_._.)',
    'rm(._._env_._.)',

    deparse(call)
  ), con = make_file)
  invisible(call)
}

#' @name pipeline-knitr-markdown
#' @title Configure \code{'rmarkdown'} files to build 'RAVE' pipelines
#' @description Allows building 'RAVE' pipelines from \code{'rmarkdown'} files.
#' Please use it in \code{'rmarkdown'} scripts only. Use
#' \code{\link{pipeline_create_template}} to create an example.
#' @param languages one or more programming languages to support; options are
#' \code{'R'} and \code{'python'}
#' @param module_id the module ID, usually the name of direct parent folder
#' containing the pipeline file
#' @param env environment to set up the pipeline translator
#' @param project_path the project path containing all the pipeline folders,
#' usually the active project folder
#' @param collapse,comment passed to \code{set} method of
#' \code{\link[knitr]{opts_chunk}}
#' @returns A function that is supposed to be called later that builds the
#' pipeline scripts
#' @export
configure_knitr <- function(languages = c("R", "python")){

  if(!all(languages %in% RAVE_KNITR_SUPPORTED_LANG)){
    stop("Only the following languages are supported: ", paste(RAVE_KNITR_SUPPORTED_LANG, collapse = ", "), ".")
  }
  if(file.exists("settings.yaml")){
    settings <- as.list(load_yaml("settings.yaml"))
    env <- knitr::knit_global()
    for(nm in names(settings)) {
      env[[nm]] <- resolve_pipeline_settings_value(settings[[nm]], pipe_dir = ".")
    }
    # list2env(settings, envir = knitr::knit_global())
  }

  check_knit_packages(languages)

  targets <- dipsaus::fastqueue2()

  rave_knitr_engine(targets)

  function(make_file){
    rave_knitr_build(targets, make_file)
  }
}


#' @rdname pipeline-knitr-markdown
#' @export
pipeline_setup_rmd <- function(
    module_id, env = parent.frame(),
    collapse = TRUE, comment = "#>", languages = c("R", "python"),
    project_path = dipsaus::rs_active_project(child_ok = TRUE, shiny_ok = TRUE)) {

  knitr::opts_chunk$set(collapse = collapse, comment = comment)
  env$build_pipeline <- configure_knitr(languages = languages)
  env$.module_id <- module_id

  module_path <- file.path(project_path, "modules", module_id)

  shared_scripts <- list.files(
    file.path(module_path, "R"),
    pattern = "^shared-.*\\.R$",
    ignore.case = TRUE,
    full.names = TRUE
  )

  lapply(shared_scripts, function(f) {
    source(f, local = env, chdir = TRUE)
    return()
  })


  py_dir <- file.path(module_path, "py")
  if(dir.exists(py_dir)) {
    py_info <- pipeline_py_info(pipe_dir = module_path, must_work = TRUE)
    cwd <- getwd()
    on.exit({ if(length(cwd)) { setwd(cwd) } }, add = TRUE, after = FALSE)

    setwd(py_dir)

    common_script_path <- file.path(py_info$pipeline_path, "py", "knitr-common.py")
    py <- rpymat::import_main(convert = FALSE)

    # import external libraries
    rpymat::run_pyscript(common_script_path, local = FALSE, convert = FALSE)

    # load shared modules
    py$shared <- rpymat::import(sprintf("%s.shared", py_info$module_name))
  }

  settings <- load_yaml(file.path( project_path, "modules",
                                   module_id, "settings.yaml"))

  pipe_dir <- file.path(project_path, "modules", module_id)
  lapply(names(settings), function(nm) {
    settings[[nm]] <- resolve_pipeline_settings_value(value = settings[[nm]],pipe_dir = pipe_dir)
  })

  env$.settings <- settings
  list2env(as.list(settings), envir = env)
  invisible(settings)
}
