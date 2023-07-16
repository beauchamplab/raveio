# DIPSAUS DEBUG START
# unlink("/Users/dipterix/Dropbox (Personal)/projects/raveio/adhoc/junk", recursive = TRUE)
# self <- PipelineCollections$new("/Users/dipterix/Dropbox (Personal)/projects/raveio/adhoc/junk")
# private <- self$.__enclos_env__$private
#
# a <- "asd"
# self$add_pipeline("ants_preprocessing", names = "brain_mask", pre_hook = function(...) {
#   print(a)
# })

#' Connect and schedule pipelines
PipelineCollections <- R6::R6Class(
  classname = "PipelineCollection",
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    .pipline_collections = NULL,
    .root_path = character(),
    .verbose = function(..., level = "DEFAULT", .envir = parent.frame()) {
      if(self$verbose) {
        catgl(..., level = level, .envir = .envir)
      }
    }
  ),
  public = list(

    #' @field verbose whether to verbose the build
    verbose = TRUE,

    #' @description Constructor
    #' @param root_path where to store the pipelines and intermediate results
    #' @param overwrite whether to overwrite if \code{root_path} exists
    initialize = function(root_path = NULL, overwrite = FALSE) {
      if(missing(root_path) || is.null(root_path)) {
        root_path <- tempfile(tmpdir = cache_root(), pattern = "pipeline-collection-")
      }
      if(length(root_path) != 1 || is.na(root_path) || !is.character(root_path) || !nzchar(root_path)) {
        stop("PipelineCollections$build_pipelines: Working directory `root_path` is invalid")
      }
      if(file.exists(root_path)) {
        if( overwrite ) {
          unlink(root_path, recursive = TRUE)
        } else {
          stop("Cannot create pipeline collection at `root_path` because the path has already existed and `overwrite` is disabled. Please remove `root_path` first at: ", normalizePath(root_path))
        }
      }
      private$.root_path <- root_path
      private$.pipline_collections <- dipsaus::fastqueue2()
    },

    #' @description Add pipeline into the collection
    #' @param x a pipeline name (can be found via \code{\link{pipeline_list}}),
    #' or a \code{\link{PipelineTools}}
    #' @param names pipeline targets to execute
    #' @param deps pipeline IDs to depend on; see 'Values' below
    #' @param pre_hook function to run before the pipeline; the function needs
    #' two arguments: input map (can be edit in-place), and path to a directory
    #' that allows to store temporary files
    #' @param post_hook function to run after the pipeline; the function needs
    #' two arguments: pipeline object, and path to a directory
    #' that allows to store intermediate results
    #' @param hook_envir where to look for global environments if \code{pre_hook}
    #' or \code{post_hook} contains global variables; default is the calling
    #' environment
    #' @param cue whether to always run dependence
    #' @param search_paths where to search for pipeline if \code{x} is a
    #' character; ignored when \code{x} is a pipeline object
    #' @param standalone whether the pipeline should be standalone, set to
    #' \code{TRUE} if the same pipeline added multiple times should run
    #' independently; default is true
    #' @returns A list containing
    #' \describe{
    #' \item{\code{id}}{the pipeline ID that can be used by \code{deps}}
    #' \item{\code{pipeline}}{forked pipeline instance}
    #' \item{\code{target_names}}{copy of \code{names}}
    #' \item{\code{depend_on}}{copy of \code{deps}}
    #' \item{\code{cue}}{copy of \code{cue}}
    #' \item{\code{standalone}}{copy of \code{standalone}}
    #' }
    add_pipeline = function(
      x, names = NULL, deps = NULL, pre_hook = NULL, post_hook = NULL,
      cue = c("always", "thorough", "never"), search_paths = pipeline_root(),
      standalone = TRUE, hook_envir = parent.frame()
    ) {
      cue <- match.arg(cue)

      if(!inherits(x, "PipelineTools")) {
        x <- pipeline(pipeline_name = x, temporary = TRUE, paths = search_paths)
      }

      private$.verbose(
        "Adding pipeline [{x$pipeline_name}] with update mode [{ cue }]. This pipeline contains the following targets:\n{ paste(
          sprintf('  * %s (%s)', x$target_table$Names, x$target_table$Description),
          collapse = '\n'
        ) }"
      )


      if(!is.null(pre_hook)) {
        stopifnot2(is.function(pre_hook), msg = "PipelineCollections$add_pipeline: `pre_hook` must be `NULL` or a function taking two function arguments (settings, shared_path).")
      }
      if(!is.null(post_hook)) {
        stopifnot2(is.function(post_hook), msg = "PipelineCollections$add_pipeline: `post_hook` must be `NULL` or a function taking two function arguments (pipeline & shared_path).")
      }
      if(length(names)) {
        sel <- !names %in% x$target_table$Names
        if(any(sel)) {
          names <- names[sel]
          stop(sprintf("PipelineCollections$add_pipeline: Unable to add pipeline [%s] due to missing targets: [%s]", x$pipeline_name, paste(names, collapse = ", ")))
        }
        private$.verbose("The following targets and all their depending targets will be scheduled: [{ paste(names, collapse = ', ') }] (pipeline: {x$pipeline_name}).")
      } else {
        names <- NULL
        private$.verbose("All targets will be scheduled (pipeline: {x$pipeline_name}).")
      }

      item_id <- sprintf("%s_%s", x$pipeline_name, rand_string(6))
      if(length(deps)) {
        missing_deps <- deps[!deps %in% self$pipeline_ids]
        if(length(missing_deps)) {
          stop("Cannot find pipeline ID: ", paste(missing_deps, collapse = ", "), ". Please specify `deps` using runtime ID instead of pipeline names.")
        }
      }

      if(!standalone) {
        # search for previous added pipelines to see if this one needs to wait
        for(item in private$.pipline_collections$as_list()) {
          if(identical(item$pipeline_name, x$pipeline_name)) {
            deps <- c(deps, item$id)
          }
        }
      }

      # fork the pipeline now
      if(!dir.exists(private$.root_path)) {
        private$.root_path <- dir_create2(private$.root_path, check = TRUE)
      }

      # Analysis contains: code, data, settings
      if(standalone) {
        pipeline_path <- file.path(self$collection_path, item_id)
      } else {
        pipeline_path <- file.path(self$collection_path, x$pipeline_name)
      }

      if(dir.exists(pipeline_path)) {
        forked_pipeline <- pipeline(
          pipeline_name = basename(pipeline_path),
          paths = dirname(pipeline_path),
          temporary = TRUE
        )
      } else {
        forked_pipeline <- x$fork(path = pipeline_path)
      }

      # save globals
      pre_hook_pak <- NULL
      if(is.function(pre_hook)) {
        pre_hook_pak <- list(
          func = pre_hook,
          globals = globals::globalsOf(pre_hook, envir = hook_envir)
        )
      }

      post_hook_pak <- NULL
      if(is.function(post_hook)) {
        post_hook_pak <- list(
          func = post_hook,
          globals = globals::globalsOf(post_hook, envir = hook_envir)
        )
      }

      configurations <- list(
        id = item_id,
        pipeline_name = forked_pipeline$pipeline_name,
        target_names = names,
        depend_on = deps,
        cue = cue,
        pre_hook_pak = pre_hook_pak,
        post_hook_pak = post_hook_pak,
        standalone = standalone
      )
      suppressWarnings({
        saveRDS(configurations, file = file.path(pipeline_path,
                                                 sprintf("configurations_%s.rds", item_id)))
      })

      private$.pipline_collections$add(configurations)

      private$.verbose("Pipeline [ID: {item_id}, name: {x$pipeline_name}] is ready at {pipeline_path}")
      return(invisible(list(
        id = item_id,
        pipeline = forked_pipeline,
        target_names = names,
        depend_on = deps,
        cue = cue,
        standalone = standalone
      )))
    },

    #' @description Build pipelines and visualize
    #' @param visualize whether to visualize the pipeline; default is true
    build_pipelines = function(visualize = TRUE) {
      module_label <- strftime(Sys.time(), "Scheduler %y%m%d-%H%M%S (created)")

      module_add(
        module_id = "raveio_scheduler",
        module_label = module_label,
        path = self$root_path,
        type = "scheduler",
        overwrite = TRUE
      )

      # inject code blocks
      path_scheduler <- file.path(self$root_path, "modules", "raveio_scheduler")
      scheduler <- pipeline(pipeline_name = "raveio_scheduler", paths = file.path(self$root_path, "modules"), temporary = TRUE)
      path_rmd <- file.path(path_scheduler, "main.Rmd")
      pipeline_rmd <- readLines(path_rmd)

      idx <- which(grepl("RAVE Scheduler Placeholder", pipeline_rmd))
      if(length(idx) != 1) {
        stop("Cannot find scheduler template placeholder. Please contact RAVE devs to report this bug.")
      }

      injections <- lapply(seq_len(private$.pipline_collections$size()), function ( ii ) {
        item <- private$.pipline_collections$at( ii )
        paste(collapse = "\n", c(
          '',
          sprintf('```{rave run_pipeline_ID_%s, cue = "%s", export = "%s"}', item$id, item$cue, item$id),

          # Help RAVE build dependence
          sprintf("force(%s)", item$depend_on),

          'raveio <- asNamespace("raveio")',
          sprintf('pipeline_id <- "%s"', item$id),
          sprintf('pipeline_name <- "%s"', item$pipeline_name),
          sprintf('standalone <- %s', item$standalone),

          'results <- raveio$run_collection_pipeline(',
          '  collection_path = normalizePath(collection_root_path),',
          '  pipeline_id = pipeline_id,',
          '  pipeline_name = ifelse(standalone, pipeline_id, pipeline_name),',
          '  error = error_action,',
          '  dry_run = dry_run,',
          '  scheduler = "none"',
          ')',
          '',
          'assign(pipeline_id, results, envir = environment())',
          '```',
          ''
        ))
      })

      pre <- pipeline_rmd[seq(1, idx - 1)]
      post <- pipeline_rmd[-seq(1, idx)]

      writeLines(
        con = path_rmd,
        text = c(
          pre, unlist(injections, use.names = FALSE), post
        )
      )

      # compile
      scheduler$set_settings(
        dry_run = TRUE,
        collection_root_path = "../../",
        error_action = "ignore"
      )
      require_package("rmarkdown")
      rmarkdown <- asNamespace("rmarkdown")
      rmarkdown$render(
        input = path_rmd,
        output_dir = path_scheduler,
        knit_root_dir = path_scheduler,
        intermediates_dir = path_scheduler,
        runtime = "static",
        output_format = "html_document",
        quiet = TRUE
      )

      # save configurations
      collection_info <- lapply(seq_len(private$.pipline_collections$size()), function ( ii ) {
        item <- private$.pipline_collections$at( ii )
        re <- item[c("id", "pipeline_name", "target_names", "depend_on", "cue", "standalone")]
        re$has_pre_hook <- !is.null(item$pre_hook_pak)
        re$has_post_hook <- !is.null(item$post_hook_pak)
        re$path_prefix_pipeline <- file.path("pipelines", item$id, fsep = "/")
        re$path_prefix_share <- file.path("shared", item$id, fsep = "/")
        re
      })


      save_json(
        list(
          pipelines = collection_info,
          scheduler_path = "modules/raveio_scheduler",
          last_built = strftime(Sys.time(), tz = "GMT", usetz = TRUE)
        ),
        serialize = FALSE,
        auto_unbox = TRUE,
        con = file.path(self$root_path, "build_info.json")
      )

      if( visualize ) {
        scheduler$visualize(aspect_ratio = 3)
      }
      return(invisible(scheduler))
    },

    #' @description Run the collection of pipelines
    #' @param error what to do when error occurs; default is \code{'error'}
    #' throwing errors; other choices are \code{'warning'} and \code{'ignore'}
    #' @param .scheduler,.type,.as_promise,.async,... passed to
    #' \code{\link{pipeline_run}}
    #' @param rebuild whether to re-build the pipeline; default is \code{NA} (
    #' if the pipeline has been built before, then do not rebuild)
    run = function(error = c("error", "warning", "ignore"),
                   .scheduler = c("none", "future", "clustermq"),
                   .type = c("callr", "smart", "vanilla"),
                   .as_promise = FALSE,
                   .async = FALSE,
                   rebuild = NA, ...) {
      error <- match.arg(error)
      .scheduler <- match.arg(.scheduler)
      .type <- match.arg(.type)

      if(is.na(rebuild)) {
        tryCatch({
          self$get_scheduler()
        }, error = function(e) {
          self$build_pipelines(visualize = FALSE)
        })
      } else if(rebuild) {
        self$build_pipelines(visualize = FALSE)
      }
      scheduler <- self$get_scheduler()
      scheduler$set_settings(
        dry_run = FALSE,
        collection_root_path = "../../",
        error_action = error
      )
      scheduler$run(scheduler = .scheduler, type = .type, as_promise = .as_promise, async = .async, ...)
    },

    #' @description Get \code{scheduler} object
    get_scheduler = function() {
      path_scheduler <- file.path(self$root_path, "modules", "raveio_scheduler")
      if(!dir.exists(path_scheduler)) {
        stop("Scheduler does not exists. Please build the scheduler & pipelines first.")
      }
      scheduler <- pipeline(pipeline_name = "raveio_scheduler", paths = file.path(self$root_path, "modules"), temporary = TRUE)
      scheduler
    }

  ),
  active = list(
    #' @field root_path path to the directory that contains pipelines and
    #' scheduler
    root_path = function() {
      private$.root_path
    },

    #' @field collection_path path to the pipeline collections
    collection_path = function() {
      file.path(private$.root_path, "pipelines")
    },

    #' @field pipeline_ids pipeline ID codes
    pipeline_ids = function() {
      vapply(seq_len(private$.pipline_collections$size()), function(ii) {
        private$.pipline_collections$at(ii)$id
      }, "")
    }
  )
)


run_collection_pipeline <- function(collection_path, pipeline_id, pipeline_name,
                                    error = c("error", "ignore", "warning"),
                                    dry_run = FALSE, ...) {
  # DIPSAUS DEBUG START
  # collection_path <- "/Users/dipterix/Dropbox (Personal)/projects/raveio/adhoc/junk"
  # pipeline_name <- "ants_preprocessing_07e310"

  error <- match.arg(error)

  # load pipeline
  pipeline <- pipeline(
    pipeline_name = pipeline_name, temporary = TRUE,
    paths = file.path(collection_path, "pipelines")
  )

  # pipeline config
  config <- readRDS(file.path(pipeline$pipeline_path, sprintf("configurations_%s.rds", pipeline_id)))

  # pipeline inputs
  inputs <- load_yaml(pipeline$settings_path)

  shared_path <- file.path(collection_path, "shared", pipeline_id)
  shared_path <- dir_create2(shared_path)

  if( dry_run ) { return(list(
    id = pipeline_id,
    standalone = !identical(pipeline_id, pipeline_name),
    last_build = Sys.time(),
    inputs = as.list(inputs, sorted = TRUE),
    dry_run = dry_run
  )) }

  run_pipeline <- function(...) {
    # Before running pipeline
    pre_hook <- config$pre_hook_pak
    if(!is.null(pre_hook)) {
      pre_hook_env <- new.env(parent = globalenv())
      list2env(pre_hook$globals, envir = pre_hook_env)
      environment(pre_hook$func) <- pre_hook_env
      pre_hook$func(inputs, shared_path)

      # free memory
      config$pre_hook_pak <- NULL
      rm(pre_hook_env, pre_hook)
    }

    # set inputs
    pipeline$set_settings(.list = inputs)


    # run pipeline
    pipeline$run(
      names = config$target_names,
      async = FALSE,
      as_promise = FALSE,
      return_values = FALSE,
      ...
    )

    # post hook
    post_hook <- config$post_hook_pak

    results <- NULL
    if(!is.null(post_hook)) {
      post_hook_env <- new.env(parent = globalenv())
      list2env(post_hook$globals, envir = post_hook_env)
      environment(post_hook$func) <- post_hook_env
      results <- post_hook$func(pipeline, shared_path)
    }

    results
  }


  post_hook_results <- NULL
  result_error <- FALSE
  if( error == "error" ) {
    post_hook_results <- run_pipeline(...)
  } else {
    post_hook_results <- tryCatch({
      run_pipeline(...)
    }, error = function(e) {
      if(error == "warning") {
        warning(e)
      }
      result_error <<- TRUE
      simpleError(e$message)
    })
  }


  list(
    id = pipeline_id,
    last_build = Sys.time(),
    standalone = !identical(pipeline_id, pipeline_name),
    actual_pipeline_name = pipeline$pipeline_name,
    inputs = as.list(inputs, sorted = TRUE),
    results = post_hook_results,
    error = result_error
  )

}

#' @title Combine and execute pipelines
#' @param root_path directory to store pipelines and results
#' @param overwrite whether to overwrite if \code{root_path} exists; default is
#' false, and raises an error when \code{root_path} exists
#' @returns A \code{\link{PipelineCollections}} instance
#' @export
pipeline_collection <- function(root_path = NULL, overwrite = FALSE) {
  PipelineCollections$new(root_path = root_path, overwrite = overwrite)
}
