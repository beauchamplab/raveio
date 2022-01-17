#' @rdname rave-pipeline
#' @export
pipeline_run <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  type = c("basic", "async", "vanilla", "custom"),
  envir = parent.frame(), callr_function = NULL,
  use_future = FALSE,
  ...){

  pipe_dir <- activate_pipeline(pipe_dir)

  type <- match.arg(type)
  if(type == "async") {
    stop("type == 'async' is deprecated now, consider using `pipeline_run_async` instead.")
  }
  if(use_future && type == "async"){
    stop("Cannot run async pipeline using future package.")
  }
  if(type == "custom"){
    if(is.null(callr_function)){
      stop("Please specify `callr_function`. Examples are `callr::r`, `callr::r_bg`, ...; see `?targets::tar_make`")
    }
  } else {
    callr_function <- switch (
      type,
      "basic" = NULL,
      "async" = callr::r_bg,
      "vanilla" = callr::r
    )
  }
  force(envir)

  if(use_future){
    targets::tar_make_future(
      callr_function = callr_function,
      envir = envir,
      workers = raveio_getopt("max_worker", default = 1L),
      ...
    )
  } else {
    targets::tar_make(
      callr_function = callr_function,
      envir = envir, ...
    )
  }

  invisible()
}


pipeline_run_basic <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  envir = parent.frame(), callr_function = NULL,
  use_future = FALSE, ...){

  pipe_dir <- activate_pipeline(pipe_dir)
  force(envir)

  if(use_future){
    targets::tar_make_future(callr_function = NULL, envir = envir,
                             workers = raveio_getopt("max_worker", default = 1L), ...)
  } else {
    targets::tar_make( callr_function = NULL, envir = envir, ...)
  }

  invisible()
}

pipeline_run_vanilla <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  envir = parent.frame(), callr_function = NULL,
  use_future = FALSE, ...){

  pipe_dir <- activate_pipeline(pipe_dir)
  force(envir)

  if(use_future){
    targets::tar_make_future(callr_function = callr::r, envir = envir,
                             workers = raveio_getopt("max_worker", default = 1L), ...)
  } else {
    targets::tar_make( callr_function = callr::r, envir = envir, ...)
  }

  invisible()
}

pipeline_run_custom <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  envir = parent.frame(), callr_function = NULL,
  use_future = FALSE, ...){

  pipe_dir <- activate_pipeline(pipe_dir)
  force(envir)

  if(use_future){
    targets::tar_make_future(callr_function = callr_function, envir = envir,
                             workers = raveio_getopt("max_worker", default = 1L), ...)
  } else {
    targets::tar_make( callr_function = callr_function, envir = envir, ...)
  }

  invisible()
}

#' @rdname rave-pipeline
#' @export
pipeline_run_async <- function(
  pipe_dir = Sys.getenv("RAVE_PIPELINE", "."),
  callr_function = NULL, use_future = TRUE,
  type = c("basic", "vanilla", "custom"),
  check_interval = 1, packages = NULL,
  use_rs_job = FALSE, rs_job_name = "rave-pipeline",
  progress_title = "Executing in Progress",
  progress_max = NA,
  progress_quiet = !(use_rs_job || dipsaus::shiny_is_running()),
  ...){
  type <- match.arg(type)
  callr_function <- substitute(callr_function)

  more_args <- list(...)
  more_args$use_future <- use_future
  more_args$pipe_dir <- pipe_dir
  # more_args$reporter <- "silent"

  check <- dipsaus::rs_exec(bquote({
    if(.(use_future)){
      dipsaus::make_forked_clusters(
        on_failure = "multisession",
        workers = raveio::raveio_getopt("max_worker", 1L)
      )
    }
    local({
      ns <- asNamespace('raveio')
      args <- .(more_args)
      args$callr_function <- .(callr_function)
      args$envir <- globalenv()
      do.call(ns[[.(paste0("pipeline_run_", type))]], args)
    })

  }), quoted = TRUE, wait = FALSE, focus_on_console = TRUE,
  rs = use_rs_job, packages = packages, name = rs_job_name,
  ignore.stdout = TRUE)
  tarnames <- pipeline_target_names(pipe_dir = pipe_dir)
  tarnames_readable <- names(tarnames)

  promises::promise(function(resolve, reject) {
    progress <- dipsaus::progress2(
      progress_title, max = progress_max, shiny_auto_close = FALSE,
      quiet = progress_quiet
    )
    callback <- function(){
      state <- check()
      if(state == 0) {
        progress$close()
        resolve(attr(state, 'rs_exec_result'))
      } else if(state < 0){
        progress$close()
        reject(attr(state, 'rs_exec_error'))
      } else {
        # ravedash::add_callback(callback)
        later::later(callback, delay = check_interval)

        try({
          tbl <- pipeline_progress(pipe_dir = pipe_dir, method = "details")
          sel <- tbl$progress %in% "started"
          if(any(sel)){
            names <- tbl$name[seq_len(max(which(sel)))]
            cur_val <- progress$get_value()
            if(cur_val > 0){
              names <- names[-seq_len(cur_val)]
            }
            for(nm in names){
              ii <- which(tarnames == nm)
              if(!length(ii)){
                ii <- which(startsWith(nm, tarnames))
              }
              ii <- ii[[1]]
              if(length(tarnames_readable) < ii || tarnames_readable[[ii]] == ""){
                msg <- sprintf('Calculating `%s`', nm)
              } else {
                msg <- unlist(strsplit(tarnames_readable[ii], "[_-]+"))
                msg <- msg[msg != ""]
                msg <- paste(msg, collapse = " ")
                msg <- sprintf("%s (calculating `%s`)", msg, nm)
              }
              progress$inc(msg)
            }
          }

        }, silent = TRUE)


      }
    }
    later::later(callback, delay = check_interval)
  })

}

