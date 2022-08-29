#' @rdname rave-pipeline
#' @export
pipeline_run <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  scheduler = c("none", "future", "clustermq"),
  type = c("smart", "callr", "vanilla"),
  envir = new.env(parent = globalenv()),
  callr_function = NULL,
  names = NULL,
  async = FALSE,
  check_interval = 0.5,
  progress_quiet = !async,
  progress_max = NA,
  progress_title = "Running pipeline",
  ...){

  pipe_dir <- activate_pipeline(pipe_dir)

  type <- match.arg(type)
  scheduler <- match.arg(scheduler)
  callr_function <- substitute(callr_function)


  clustermq_scheduler <- getOption('clustermq.scheduler', NA)
  if(scheduler == "clustermq"){
    # if(!identical(clustermq_scheduler, "LOCAL")){
    #   callr_function <- NULL
    # }
    callr_function <- NULL
  }

  if(type == "vanilla"){
    callr_function <- NULL
  } else if (type == "callr") {
    callr_function <- quote(callr::r)
  }

  args <- list(
    names = names,
    envir = envir,
    callr_function = NULL,
    ...
  )

  subprocess <- TRUE
  fun <- function(subprocess = TRUE){}
  environment(fun) <- globalenv()
  body(fun) <- bquote({
    ns <- asNamespace('raveio')
    callr_function <- eval(.(callr_function))
    args <- .(args)
    if(!is.null(callr_function)){
      args$callr_function <- callr_function
    }
    clustermq_scheduler <- .(clustermq_scheduler)

    all_names <- ns$pipeline_target_names(pipe_dir = .(pipe_dir))
    if(length(args$names)) {
      if(!is.character(args$names)){
        stop("pipeline_run: `names` must be NULL or characters")
      }
      missing_names <- args$names[!args$names %in% all_names]
      if(length(missing_names)) {
        stop("pipeline_run: the following `names` cannot be found: ", paste(missing_names, collapse = ", "))
      }
    } else {
      args$names <- NULL
    }

    if(subprocess) {
      options(
        "future.fork.enable" = FALSE,
        "dipsaus.no.fork" = TRUE,
        "dipsaus.cluster.backup" = "multisession"
      )
    }
    if(.(type) == "smart"){
      local <- ns$with_future_parallel
    }
    make <- function(fun, use_local = TRUE) {
      tryCatch({
        if( use_local ) {
          local({ do.call(fun, args) })
        } else {
          do.call(fun, args)
        }

      }, `tar_condition_file` = function(e) {
        # destroy and try again, and throw all other errors
        targets::tar_destroy(ask = FALSE, destroy = "meta")
        if( use_local ) {
          local({ do.call(fun, args) })
        } else {
          do.call(fun, args)
        }
      })
    }

    if("none" == .(scheduler)){
      make( targets::tar_make )
    } else if("future" == .(scheduler)){
      args$workers <- ns$raveio_getopt("max_worker", default = 1L)
      make( targets::tar_make_future )
      # local({ do.call(targets::tar_make_future, args) })
    } else {

      if(is.na(clustermq_scheduler)) {
        clustermq_scheduler <- "multiprocess"
      }
      options('clustermq.scheduler' = clustermq_scheduler)
      if(identical(clustermq_scheduler, "LOCAL")){
        make( targets::tar_make_clustermq )
        # local({ do.call(targets::tar_make_clustermq, args) })
      } else {
        args$workers <- ns$raveio_getopt("max_worker", default = 1L)
        # do.call(targets::tar_make_clustermq, args)
        make( targets::tar_make_clustermq, use_local = FALSE )
      }

    }

  })


  res <- PipelineResult$new(path = pipe_dir, verbose = TRUE)
  res$check_interval <- check_interval
  res$names <- names

  if(!progress_quiet && is.na(progress_max)){
    if(length(names)){
      progress_max <- length(names)
    } else {
      progress_max <- length(pipeline_target_names(pipe_dir = pipe_dir))
    }
  }
  res$progressor <- dipsaus::progress2(
    progress_title, max = progress_max, shiny_auto_close = !async,
    quiet = progress_quiet
  )

  if(async){


    res$run(
      async = TRUE,
      expr = {
        callr::r_bg(func = fun, args = list(subprocess = TRUE),
                    package = FALSE, poll_connection = TRUE,
                    supervise = TRUE, error = "error")
      }
    )
    res
  } else {
    res$run(
      async = FALSE,
      expr = {
        fun(subprocess = FALSE)
      }
    )
  }

  res
}


#' @rdname rave-pipeline
#' @export
pipeline_clean <- function(
    pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
    destroy = c("all", "cloud", "local", "meta", "process",
                "progress", "objects", "scratch", "workspaces"),
    ask = FALSE
    ) {
  destroy <- match.arg(destroy)
  pipe_dir <- activate_pipeline(pipe_dir)
  targets::tar_destroy(ask = ask, destroy = destroy)
}

#' @rdname rave-pipeline
#' @export
pipeline_run_bare <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  scheduler = c("none", "future", "clustermq"),
  type = c("smart", "callr", "vanilla"),
  envir = new.env(parent = globalenv()),
  callr_function = NULL,
  names = NULL,
  ...) {
  pipe_dir <- activate_pipeline(pipe_dir)

  type <- match.arg(type)
  scheduler <- match.arg(scheduler)
  callr_function <- substitute(callr_function)


  clustermq_scheduler <- getOption('clustermq.scheduler', NA)
  if(scheduler == "clustermq"){
    # if(!identical(clustermq_scheduler, "LOCAL")){
    #   callr_function <- NULL
    # }
    callr_function <- NULL
  }

  if(type == "vanilla"){
    callr_function <- NULL
  } else if (type == "callr") {
    callr_function <- quote(callr::r)
  }

  all_names <- pipeline_target_names(pipe_dir = pipe_dir)

  if(length(names)) {
    if(!is.character(names)){
      stop("pipeline_run_bare: `names` must be NULL or characters")
    }
    missing_names <- names[!names %in% all_names]
    if(length(missing_names)) {
      stop("pipeline_run_bare: the following `names` cannot be found: ", paste(missing_names, collapse = ", "))
    }
  } else {
    names <- NULL
  }

  if(type == "smart") {
    local <- with_future_parallel
  }

  args <- list(
    names = names,
    envir = envir,
    callr_function = callr_function,
    ...
  )

  make <- function(fun, use_local = TRUE) {
    tryCatch({
      if( use_local ) {
        local({ do.call(fun, args) })
      } else {
        do.call(fun, args)
      }

    }, `tar_condition_file` = function(e) {
      # destroy and try again, and throw all other errors
      targets::tar_destroy(ask = FALSE, destroy = "meta")
      if( use_local ) {
        local({ do.call(fun, args) })
      } else {
        do.call(fun, args)
      }
    })
  }

  switch (
    scheduler,
    "none" = {
      make( targets::tar_make )
    },
    "future" = {
      args$workers <- raveio_getopt("max_worker", default = 1L)
      make( targets::tar_make_future )
    },
    {
      if(is.na(clustermq_scheduler)) {
        clustermq_scheduler <- "multiprocess"
      }
      options('clustermq.scheduler' = clustermq_scheduler)
      if(identical(clustermq_scheduler, "LOCAL")){
        make( targets::tar_make_clustermq )
        # local({ do.call(targets::tar_make_clustermq, args) })
      } else {
        args$workers <- raveio_getopt("max_worker", default = 1L)
        # do.call(targets::tar_make_clustermq, args)
        make( targets::tar_make_clustermq, use_local = FALSE )
      }
    }
  )

  # Read in names
  if(!length(names)) {
    names <- pipeline_target_names(pipe_dir = pipe_dir)
  }
  pipeline_read(var_names = names, pipe_dir = pipe_dir)

}
