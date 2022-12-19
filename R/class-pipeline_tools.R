#' Class definition for pipeline tools
#' @seealso \code{\link{pipeline}}
PipelineTools <- R6::R6Class(
  classname = "PipelineTools",
  portable = TRUE,
  cloneable = TRUE,

  private = list(
    .pipeline_path = character(),
    .pipeline_name = character(),
    .settings_file = character(),
    .settings = NULL,
    .settings_external_inputs = list()
  ),

  public = list(

    #' @description construction function
    #' @param pipeline_name name of the pipeline, usually in the pipeline
    #' \code{'DESCRIPTION'} file, or pipeline folder name
    #' @param settings_file the file name of the settings file, where the
    #' user inputs are stored
    #' @param paths the paths to find the pipeline, usually the parent folder
    #' of the pipeline; default is \code{pipeline_root()}
    initialize = function(pipeline_name,
                          settings_file = "settings.yaml",
                          paths = pipeline_root()) {

      default_paths <- c(".", file.path(R_user_dir('raveio', 'data'), "pipelines"))

      paths <- c(paths[dir.exists(paths)], default_paths)

      pipeline_root(paths)
      private$.pipeline_path <- pipeline_find(pipeline_name)
      private$.pipeline_name <- attr(private$.pipeline_path, "target_name")
      private$.settings_file <- settings_file

      pipeline_settings_path <- file.path(
        private$.pipeline_path,
        private$.settings_file
      )

      settings <- load_yaml(pipeline_settings_path)
      lapply(names(settings), function(nm) {
        if(nm == "") { return() }
        opts <- resolve_pipeline_settings_opt(settings[[nm]], strict = FALSE)
        if(is.null(opts) || !is.list(opts)) { return() }

        opts$raw_input <- settings[[nm]]
        private$.settings_external_inputs[[nm]] <- opts
        settings[[nm]] <- resolve_pipeline_settings_value( settings[[nm]], pipe_dir = private$.pipeline_path )
      })
      private$.settings <- settings

    },

    #' @description set inputs
    #' @param ...,.list named list of inputs; all inputs should be named,
    #' otherwise errors will be raised
    set_settings = function(..., .list = NULL) {
      args <- c(list(...), as.list(.list))
      argnames <- names(args)

      if(length(args)) {
        if(!length(argnames) || "" %in% argnames) {
          stop("`pipeline_set`: all input lists must have names")
        }

        external_inputs <- names(private$.settings_external_inputs)
        external_args <- argnames[argnames %in% external_inputs]
        internal_args <- argnames[!argnames %in% external_inputs]
        lapply(external_args, function(nm) {
          new_val <- args[[nm]]
          opts <- private$.settings_external_inputs[[nm]]

          pipeline_save_extdata(
            data = new_val,
            name = opts$name,
            format = opts$format,
            overwrite = TRUE,
            pipe_dir = private$.pipeline_path
          )
          cls <- class(new_val)
          if( !"raveio-pipeline-extdata" %in% cls ) {
            cls <- c("raveio-pipeline-extdata", cls)
          }
          private$.settings[[nm]] <- structure(
            new_val, class = cls,
            `raveio-pipeline-extdata-opts` = opts
          )
          return()
        })
        lapply(internal_args, function(nm) {
          private$.settings[[nm]] <- args[[nm]]
          return()
        })

        # TODO: check whether this should be put outside, i.e. save settings
        # no matter the settings have been changed or not
        pipeline_settings_path <- file.path(
          private$.pipeline_path,
          private$.settings_file
        )

        settings_copy <- as.list(private$.settings)
        if(length(external_inputs)) {
          settings_copy[external_inputs] <- lapply(private$.settings_external_inputs, "[[", "raw_input")
        }
        save_yaml(
          x = settings_copy,
          file = pipeline_settings_path,
          sorted = TRUE
        )
      }

      return(invisible(as.list(private$.settings)))
    },

    #' @description get current inputs
    #' @param key the input name; default is missing, i.e., to get all the
    #' settings
    #' @param default default value if not found
    #' @param constraint the constraint of the results; if input value is not
    #' from \code{constraint}, then only the first element of \code{constraint}
    #' will be returned.
    #' @return The value of the inputs, or a list if \code{key} is missing
    get_settings = function(key, default = NULL, constraint) {
      if(missing(key)){
        return(as.list(private$.settings))
      }
      if(!private$.settings$`@has`(key)){
        re <- default
      } else {
        re <- private$.settings[[key]]
      }
      if(!missing(constraint)){
        re <- re %OF% constraint
      }
      re
    },

    #' @description read intermediate variables
    #' @param var_names the target names, can be obtained via
    #' \code{x$target_table} member; default is missing, i.e., to read
    #' all the intermediate variables
    #' @param ifnotfound variable default value if not found
    #' @param ... other parameters passing to \code{\link{pipeline_read}}
    #' @return The values of the targets
    read = function(var_names, ifnotfound = NULL, ...) {
      if(missing(var_names)) {
        var_names <- pipeline_target_names(pipe_dir = private$.pipeline_path)
      } else {
        var_names_quoted <- substitute(var_names)
        if(typeof(var_names_quoted) == "language" &&
           identical(var_names_quoted[[1]], quote(`-`))) {
          all_names <- pipeline_target_names(pipe_dir = private$.pipeline_path)
          var_names <- all_names[!all_names %in% eval(var_names_quoted[[2]], envir = parent.frame())]
        }
      }

      pipeline_read(var_names = var_names, pipe_dir = private$.pipeline_path,
                    ifnotfound = ifnotfound, ...)

    },

    #' @description run the pipeline
    #' @param names pipeline variable names to calculate; default is to
    #' calculate all the targets
    #' @param async whether to run asynchronous in another process
    #' @param as_promise whether to return a \code{\link{PipelineResult}}
    #' instance
    #' @param scheduler,type,envir,callr_function,... passed to
    #' \code{\link{pipeline_run}} if \code{as_promise} is true, otherwise
    #' these arguments will be passed to \code{pipeline_run_bare}
    #' @return A \code{\link{PipelineResult}} instance if \code{as_promise}
    #' or \code{async} is true; otherwise a list of values for input \code{names}
    run = function(names = NULL, async = FALSE, as_promise = async,
                   scheduler = c("none", "future", "clustermq"),
                   type = c("smart", "callr", "vanilla"),
                   envir = new.env(parent = globalenv()),
                   callr_function = NULL,
                   ...) {
      if(!as_promise && async) {
        stop("If you run the pipeline asynchronous, then the result must be a `promise` object")
      }
      scheduler <- match.arg(scheduler)
      type <- match.arg(type)
      force(envir)
      force(callr_function)

      expr <- bquote(pipeline_run_bare(
        pipe_dir = .(private$.pipeline_path), scheduler = .(scheduler),
        type = .(type), envir = envir, callr_function = .(callr_function),
        names = .(names), ...))

      if( as_promise ) {
        expr[[1]] <- quote(pipeline_run)
        expr[["async"]] <- async
      }
      eval(expr)

    },

    #' @description run the pipeline in order; unlike \code{$run()}, this method
    #' does not use the \code{targets} infrastructure, hence the pipeline
    #' results will not be stored, and the order of \code{names} will be
    #' respected.
    #' @param names pipeline variable names to calculate; must be specified
    #' @param env environment to evaluate and store the results
    #' @param clean whether to evaluate without polluting \code{env}
    eval = function(names, env = parent.frame(), clean = TRUE) {
      if(clean) {
        envir <- new.env(parent = env)
      } else {
        envir <- env
      }
      # shared_path <- file.path(private$.pipeline_path, "R")
      # shared_libs <- list.files(shared_path, pattern = "^shared-.*\\.R",
      #                           full.names = TRUE, ignore.case = TRUE)
      # shared_libs <- sort(shared_libs)
      #
      # lapply(shared_libs, function(f) {
      #   source(file = f, local = envir, chdir = TRUE)
      # })
      # list2env(self$get_settings(), envir = envir)
      pipeline_eval(names = names, env = envir, pipe_dir = private$.pipeline_path,
                    settings_path = self$settings_path)
    },

    #' @description get progress of the pipeline
    #' @param method either \code{'summary'} or \code{'details'}
    #' @return A table of the progress
    progress = function(method = c("summary", "details")) {
      method <- match.arg(method)
      pipeline_progress(pipe_dir = private$.pipeline_path, method = method)
    },

    #' @description attach pipeline tool to environment (internally used)
    #' @param env an environment
    attach = function(env) {
      env$pipeline_set <- self$set_settings
      env$pipeline_get <- self$get_settings
      env$pipeline_settings_path <- self$settings_path
      env$pipeline_path <- private$.pipeline_path
    },

    #' @description run code with pipeline activated, some environment variables
    #' and function behaviors might change under such condition (for example,
    #' \code{targets} package functions)
    #' @param expr expression to evaluate
    #' @param quoted whether \code{expr} is quoted; default is false
    #' @param env environment to run \code{expr}
    with_activated = function(expr, quoted = FALSE, env = parent.frame()) {
      if(!quoted) {
        expr <- substitute(expr)
      }
      activate_pipeline(pipe_dir = private$.pipeline_path)
      # don't mess with self$eval
      basens <- baseenv()
      basens$eval(expr, envir = env)
    },


    #' @description clean all or part of the data store
    #' @param destroy,ask see \code{\link[targets]{tar_destroy}}
    clean = function(destroy = c("all", "cloud", "local", "meta", "process",
                                 "progress", "objects", "scratch", "workspaces"),
                     ask = FALSE) {
      destroy <- match.arg(destroy)
      pipeline_clean(pipe_dir = private$.pipeline_path, ask = ask, destroy = destroy)
    },

    #' @description save data to pipeline data folder
    #' @param data R object
    #' @param name the name of the data to save, must start with letters
    #' @param format serialize format, choices are \code{'json'},
    #' \code{'yaml'}, \code{'csv'}, \code{'fst'}, \code{'rds'}; default is
    #' \code{'json'}. To save arbitrary objects such as functions or
    #' environments, use \code{'rds'}
    #' @param overwrite whether to overwrite existing files; default is no
    #' @param ... passed to saver functions
    #' @return the saved file path
    save_data = function(data, name, format = c("json", "yaml", "csv", "fst", "rds"),
                         overwrite = FALSE, ...) {
      format <- match.arg(format)
      pipeline_save_extdata(
        data = data, name = name, format = format,
        overwrite = overwrite, pipe_dir = self$pipeline_path, ...)
    },

    #' @description load data from pipeline data folder
    #' @param name the name of the data
    #' @param error_if_missing whether to raise errors if the name is missing
    #' @param default_if_missing default values to return if the name is missing
    #' @param format the format of the data, default is automatically obtained
    #' from the file extension
    #' @param ... passed to loader functions
    #' @return the data if file is found or a default value
    load_data = function(name, error_if_missing = TRUE, default_if_missing = NULL,
                         format = c("auto", "json", "yaml", "csv", "fst", "rds"), ...) {

      format <- match.arg(format)
      pipeline_load_extdata(name = name, format = format,
                            error_if_missing = error_if_missing,
                            default_if_missing = default_if_missing,
                            pipe_dir = self$pipeline_path, ...)
    }


  ),
  active = list(

    #' @field settings_path absolute path to the settings file
    settings_path = function() {
      file.path(
        private$.pipeline_path,
        private$.settings_file
      )
    },

    #' @field target_table table of target names and their descriptions
    target_table = function() {
      re <- pipeline_target_names(pipe_dir = private$.pipeline_path)
      des <- sapply(strsplit(names(re), "_"), function(x){
        x <- x[x != ""]
        if(!length(x)) { return("<No description>") }
        substr(x[[1]], start = 1, stop = 1) <- toupper(
          substr(x[[1]], start = 1, stop = 1)
        )
        paste(x, collapse = " ")
      })
      data.frame(
        Names = unname(re),
        Description = des
      )
    },

    #' @field result_table summary of the results, including
    #' signatures of data and commands
    result_table = function() {
      pipeline_vartable(pipe_dir = private$.pipeline_path)
    },

    #' @field pipeline_path the absolute path of the pipeline
    pipeline_path = function() {
      private$.pipeline_path
    },

    #' @field pipeline_name the code name of the pipeline
    pipeline_name = function() {
      private$.pipeline_name
    }

  )
)

#' @title Creates 'RAVE' pipeline instance
#' @description Set pipeline inputs, execute, and read pipeline outputs
#' @param pipeline_name the name of the pipeline, usually title field in the
#' \code{'DESCRIPTION'} file, or the pipeline folder name (if description
#' file is missing)
#' @param settings_file the name of the settings file, usually stores user
#' inputs
#' @param paths the paths to search for the pipeline, usually the parent
#' directory of the pipeline; default is \code{\link{pipeline_root}}, which
#' only search for pipelines that are installed or in current working directory.
#' @return A \code{\link{PipelineTools}} instance
#' @examples
#'
#' if(interactive()) {
#'
#' library(raveio)
#'
#' # ------------ Set up a bare minimal example pipeline ---------------
#' pipeline_path <- pipeline_create_template(
#'   root_path = tempdir(), pipeline_name = "raveio_demo",
#'   overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")
#'
#' save_yaml(list(
#'   n = 100, pch = 16, col = "steelblue"
#' ), file = file.path(pipeline_path, "settings.yaml"))
#'
#' pipeline_build(pipeline_path)
#'
#' rmarkdown::render(input = file.path(pipeline_path, "main.Rmd"),
#'                   output_dir = pipeline_path,
#'                   knit_root_dir = pipeline_path,
#'                   intermediates_dir = pipeline_path, quiet = TRUE)
#'
#' utils::browseURL(file.path(pipeline_path, "main.html"))
#'
#' # --------------------- Example starts ------------------------
#'
#' pipeline <- pipeline("raveio_demo", paths = tempdir())
#'
#' pipeline$run("plot_data")
#'
#' # Run again and you will see some targets are skipped
#' pipeline$set_settings(pch = 2)
#' pipeline$run("plot_data")
#'
#' head(pipeline$read("input_data"))
#'
#' # or use
#' pipeline[c("n", "pch", "col")]
#' pipeline[-c("input_data")]
#'
#' pipeline$target_table
#'
#' pipeline$result_table
#'
#' pipeline$progress("details")
#'
#' # --------------------- Clean up ------------------------
#' unlink(pipeline_path, recursive = TRUE)
#'
#' }
#' @export
pipeline <- function(pipeline_name,
                     settings_file = "settings.yaml",
                     paths = pipeline_root()) {
  PipelineTools$new(pipeline_name, settings_file, paths)
}


#' @export
`[.PipelineTools` <- function(x, ...) {
  # args <- deparse1(c(...))
  # as.call(quote(x$read), args)
  expr <- as.list(match.call(expand.dots = TRUE))
  expr[[1]] <- x$read
  expr[["x"]] <- NULL
  expr <- as.call(expr)
  eval(expr, envir = parent.frame())
}
