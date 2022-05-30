#' @title Pipeline result object
PipelineResult <- R6::R6Class(
  classname = "PipelineResult",
  portable = TRUE,
  cloneable = TRUE,
  private = list(
    .path = character(0L),
    .state = character(0L),
    .process_type = character(0L),
    .process = NULL,
    .vartable = NULL,
    .invalidated = FALSE,
    .current_progress = NULL,
    finalize = function(...){
      self$invalidate()
    },
    close_progressor = function(){
      try({
        if(length(self$progressor) && !self$progressor$is_closed()){
          self$progressor$close()
          # self$progressor <- NULL
        }
      }, silent = !self$verbose)
    }
  ),
  public = list(

    #' @field progressor progress bar object, usually generated from \code{\link[dipsaus]{progress2}}
    progressor = NULL,

    #' @field promise a \code{\link[promises]{promise}} instance that monitors
    #' the pipeline progress
    promise = NULL,

    #' @field verbose whether to print warning messages
    verbose = FALSE,

    #' @field names names of the pipeline to build
    names = NULL,

    #' @field async_callback function callback to call in each check loop;
    #' only used when the pipeline is running in \code{async=TRUE} mode
    async_callback = NULL,

    #' @field check_interval used when \code{async=TRUE} in
    #' \code{\link{pipeline_run}}, interval in seconds to check the progress
    check_interval = 0.1,

    #' @description check if result is valid, raises errors when invalidated
    validate = function(){
      if(private$.invalidated){
        stop("This result has been invalidated")
      }
      invisible()
    },

    #' @description invalidate the pipeline result
    invalidate = function(){
      private$.invalidated <- TRUE
      private$.state <- "invalidated"
      if(inherits(private$.process, 'process')){
        try({
          if(isTRUE(private$.process$is_alive())){
            private$.process$kill()
          }
          private$.process <- NULL
        }, silent = !self$verbose)
      }
      private$close_progressor()
    },

    #' @description get pipeline progress
    get_progress = function(){
      self$validate()
      tbl <- pipeline_progress(pipe_dir = private$.path, method = "details")

      self$variables

      tbl <- merge(private$.vartable[,c('name', 'description')], tbl, by = 'name', all.x = TRUE, sort = FALSE)
      tbl$progress[is.na(tbl$progress)] <- "initialize"

      tbl_bk <- tbl
      on.exit({
        private$.vartable <- tbl_bk
      }, add = TRUE, after = FALSE)

      # tbl$progress[tbl$progress == "skipped"] <- "built"

      previous <- private$.vartable$progress %in% 'started'
      # finished <- !tbl$progress %in% 'initialize'
      started <- tbl$progress %in% "started"
      sel <- started & !previous
      if(any(sel)){
        sel <- which(sel)
        sel <- sel[[length(sel)]]

        private$.current_progress <- sel

      } else {
        sel <- max(private$.current_progress, 1)
      }
      return(list(
        index = sel,
        name = tbl$name[[sel]],
        description = tbl$description[[sel]],
        progress = tbl$progress[[sel]]
      ))

    },

    #' @description constructor (internal)
    #' @param path pipeline path
    #' @param verbose whether to print warnings
    initialize = function(path = character(0L), verbose = FALSE){
      private$.path <- path
      private$.current_progress <- 0
      private$.state <- "initialize"
      self$verbose <- isTRUE(as.logical(verbose))
    },

    #' @description run pipeline (internal)
    #' @param expr expression to evaluate
    #' @param env environment of \code{expr}
    #' @param quoted whether \code{expr} has been quoted
    #' @param async whether the process runs in other sessions
    #' @param process the process object inherits \code{\link[callr]{process}},
    #' will be inferred from \code{expr} if \code{process=NULL},
    #' and will raise errors if cannot be found
    run = function(expr, env = parent.frame(), quoted = FALSE,
                   async = FALSE, process = NULL) {
      if(!quoted){
        expr <- substitute(expr)
      }
      # running, ready, errored
      private$.state <- "running"
      private$.vartable <- NULL
      # self$names <- names

      promise <- promises::promise(function(resolve, reject){
        tryCatch({
          resolve(eval(expr, env))
        }, error = function(e){
          reject(e)
        })
      })

      if(async){
        private$.process_type <- 'remote'
        if(inherits(process, "process")){
          private$.process <- process
        } else {
          promise <- promises::then(
            promise,
            onFulfilled = function(process, ...){
              if(inherits(process, "process")){
                private$.process <- process
              } else {
                stop("`PipelineResult`: `expr` must return a callr::r_process instance")
              }
            },
            onRejected = function(e) {
              private$.state <- "errored"
              private$close_progressor()
              stop(e)
            }
          )
        }

        self$promise <- promise$then(
          onFulfilled = function(...){

            self$promise <- promises::promise(function(resolve, reject){

              callback <- function(){

                continue <- tryCatch({
                  if(private$.invalidated){
                    private$.state <- "canceled"
                    self$invalidate()
                    e <- simpleCondition("Pipeline canceled")

                    tryCatch({
                      if(is.function(self$async_callback)) {
                        self$async_callback()
                      }
                    })

                    reject(e)
                    return()
                  }

                  progress <- self$get_progress()

                  if(!private$.process$is_alive()){
                    private$.state <- "finished"
                    private$close_progressor()
                    private$.process$get_result()
                    resolve(private$.vartable)
                    return()
                  }

                  # show progress
                  if(length(self$progressor)){
                    old_val <- self$progressor$get_value()
                    increment <- progress$index - old_val
                    if(increment > 0){
                      self$progressor$inc(
                        detail = progress$description,
                        amount = increment
                      )
                    }
                  }

                  nrow(private$.vartable)

                  TRUE
                }, error = function(e){
                  private$.state <- "errored"
                  private$close_progressor()
                  reject(e)
                  FALSE
                })

                if(continue){
                  later::later(callback, delay = self$check_interval)
                  tryCatch({
                    if(is.function(self$async_callback)) {
                      self$async_callback()
                    }
                  })
                } else {
                  tryCatch({
                    if(is.function(self$async_callback)) {
                      self$async_callback()
                    }
                  })
                  return()
                }
              }

              callback()

            })

          },
          onRejected = function(e) {
            private$.state <- "errored"
            private$close_progressor()
            stop(e)
          }
        )

      } else {
        private$.process_type <- 'native'
        promise <- promises::then(
          promise,
          onFulfilled = function(...){
            private$.state <- "finished"
            self$variables
            return(private$.vartable)
          },
          onRejected = function(e){
            private$.state <- "errored"
            private$close_progressor()
            stop(e)
          }
        )
        self$promise <- promise
      }

    },

    #' @description wait until some targets get finished
    #' @param names target names to wait, default is \code{NULL}, i.e. to
    #' wait for all targets that have been scheduled
    #' @param timeout maximum waiting time in seconds
    #' @return \code{TRUE} if the target is finished, or \code{FALSE} if
    #' timeout is reached
    await = function(names = NULL, timeout = Inf){
      if(!self$valid){ return(FALSE) }
      promise_impl <- attr(self$promise, "promise_impl")
      now <- Sys.time()
      if(length(names)){
        missing_names <- names[!names %in% self$variables]
        if(length(missing_names)){
          stop("Unable to watch the following names: ", paste(missing_names, collapse = ", "))
        }
      } else {
        names <- self$variables
      }
      sel <- which(private$.vartable$name %in% names)
      while(
        !promise_impl$status() %in% c("fulfilled", "rejected") &&
        !later::loop_empty()
      ){
        later::run_now(0.1)

        if(private$.current_progress >= max(sel) &&
           !any(private$.vartable$progress %in% c("initialize", "started"))) {
          return(TRUE)
        }

        if(timeout <= as.numeric(Sys.time() - now, units = 'secs')){
          return(FALSE)
        }
      }
      return(TRUE)
    },


    #' @description print method
    print = function(){
      cat("<Pipeline result container> ")
      if(private$.invalidated){
        cat("(Invalidated)\n")
      } else {
        cat("\nprocess:", private$.process_type)
        if(private$.state == 'running'){
          cat(sprintf(
            "\nstatus: %s (%d of %d)\n",
            private$.state,
            private$.current_progress,
            length(self$variables)
          ))
        } else {
          cat(sprintf(
            "\nstatus: %s\n",
            private$.state
          ))
        }

      }
    },


    #' @description get results
    #' @param names the target names to read
    #' @param ... passed to code{link{pipeline_read}}
    get_values = function(names = NULL, ...){
      self$validate()
      if(!length(names)){
        names <- self$variables
      }
      pipeline_read(var_names = names, pipe_dir = private$.path, ...)
    }
  ),
  active = list(

    #' @field variables target variables of the pipeline
    variables = function(){
      if(!is.data.frame(private$.vartable)){
        self$validate()
        variables <- pipeline_target_names(pipe_dir = private$.path)
        tarnames_readable <- names(variables)
        nvars <- length(variables)
        nactual <- length(tarnames_readable)
        if(nactual < nvars){
          tarnames_readable <- c(tarnames_readable, rep('', nvars - nactual))
        }
        descr <- sapply(seq_len(nvars), function(ii){
          nm <- tarnames_readable[[ii]]
          if(nm == ""){
            return(sprintf('Calculating `%s`', variables[[ii]]))
          } else {
            msg <- unlist(strsplit(nm, "[_-]+"))
            msg <- msg[msg != ""]
            msg <- paste(msg, collapse = " ")
            if(nchar(msg)){
              msg <- sub("^[a-z]", toupper(substr(msg, 1, 1)), msg)
            }
            return(msg)
          }
        })
        tbl <- data.frame(
          name = unname(variables),
          description = descr,
          progress = "initialize",
          stringsAsFactors = FALSE
        )
        # tbl$included <- TRUE
        if(length(self$names)){
          sel <- tbl$name %in% self$names
          if(any(sel)){
            # tbl$included <- sel
            tbl <- tbl[tbl$name %in% self$names, ]
          }
        }
        private$.vartable <- tbl
      }
      private$.vartable$name
    },

    #' @field variable_descriptions readable descriptions of the target variables
    variable_descriptions = function(){
      self$variables
      private$.vartable$description
    },

    #' @field valid logical true or false whether the result instance hasn't
    #' been invalidated
    valid = function(){
      !private$.invalidated
    },

    #' @field status result status, possible status are \code{'initialize'},
    #' \code{'running'}, \code{'finished'}, \code{'canceled'},
    #' and \code{'errored'}. Note that \code{'finished'} only means the pipeline
    #' process has been finished.
    status = function(){
      private$.state
    },

    #' @field process (read-only) process object if the pipeline is running in
    #' \code{'async'} mode, or \code{NULL}; see \code{\link[callr]{r_bg}}.
    process = function(){
      private$.process
    }

  )
)

#' @export
as.promise.PipelineResult <- function(x){
  x$promise
}
