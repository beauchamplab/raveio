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
    .settings = NULL
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

      private$.settings <- load_yaml(pipeline_settings_path)

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

        dipsaus::list_to_fastmap2(args, map = private$.settings)
      }

      pipeline_settings_path <- file.path(
        private$.pipeline_path,
        private$.settings_file
      )
      save_yaml(x = private$.settings, file = pipeline_settings_path, sorted = TRUE)

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