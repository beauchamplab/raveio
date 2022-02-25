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

    progressor = NULL,
    promise = NULL,
    verbose = FALSE,
    names = NULL,
    check_interval = 0.1,

    validate = function(){
      if(private$.invalidated){
        stop("This result has been invalidated")
      }
      invisible()
    },

    invalidate = function(){
      private$.invalidated <- TRUE
      private$.state <- "invalidated"
      if(inherits(private$.process, 'process')){
        try({
          private$.process$kill()
          private$.process <- NULL
        }, silent = !self$verbose)
      }
      private$close_progressor()
    },

    get_progress = function(){
      self$validate()
      tbl <- raveio::pipeline_progress(pipe_dir = private$.path, method = "details")

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

    initialize = function(path = character(0L), verbose = FALSE){
      private$.path <- path
      private$.current_progress <- 0
      private$.state <- "initialize"
      self$verbose <- isTRUE(as.logical(verbose))
    },

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
            }
          )
        }

        self$promise <- promise$then(
          onFulfilled = function(...){

            self$promise <- promises::promise(function(resolve, reject){

              callback <- function(){
                continue <- tryCatch({
                  if(private$.invalidated){
                    self$invalidate()
                    e <- simpleCondition("Pipeline canceled")
                    reject(e)
                    return()
                  }

                  progress <- self$get_progress()

                  if(!private$.process$is_alive()){
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
                  private$close_progressor()
                  reject(e)
                  FALSE
                })

                if(continue){
                  later::later(callback, delay = self$check_interval)
                }
              }

              callback()

            })

          },
          onRejected = function(e) {
            private$close_progressor()
            stop(e)
          }
        )

      } else {
        private$.process_type <- 'native'
        promise <- promises::then(
          promise,
          onFulfilled = function(...){
            self$variables
            return(private$.vartable)
          }
        )
        self$promise <- promise
      }

    }

  ),
  active = list(

    variables = function(){
      if(!is.data.frame(private$.vartable)){
        self$validate()
        variables <- raveio::pipeline_target_names(pipe_dir = private$.path)
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
        if(length(self$names)){
          sel <- tbl$name %in% self$names
          if(any(sel)){
            tbl <- tbl[tbl$name %in% self$names, ]
          }
        }
        private$.vartable <- tbl
      }
      private$.vartable$name
    },
    variable_descriptions = function(){
      self$variables
      private$.vartable$description
    },
    valid = function(){
      !private$.invalidated
    }

  )
)
