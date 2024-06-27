
RAVEVariableConstraints <- R6::R6Class(
  classname = "RAVEVariableConstraints",
  portable = TRUE,
  private = list(
    validators = NULL
  ),
  public = list(
    type = character(0L),
    initialize = function(type, assertions = NULL) {
      self$type <- type
      # self$validators <- checkmate::makeAssertCollection()

      if( is.character(assertions) ) {
        nms <- assertions
        assertions <- as.list(assertions)
      } else {
        nms <- names(assertions)
      }

      checkmate::assert_named(assertions)
      if(!length(assertions)) {
        private$validators <- list()
      } else {
        checkmate <- asNamespace('checkmate')
        private$validators <- structure(
          names = nms,
          lapply(nms, function(nm) {
            config <- assertions[[nm]]
            if( is.function(config) ) {
              return( config )
            } else {
              assert_function <- checkmate[[sprintf("assert_%s", nm)]]
              if(!is.function(assert_function)) {
                stop(sprintf("`RAVEVariableConstraint`: Cannot find function `checkmate::assert_%s`.", nm))
              }
              config <- as.list(config)
              config$.var.name <- NULL
              config$x <- NULL
              config$add <- NULL
              return(config)
            }
          })
        )
      }
      if( !type %in% nms ) {
        assert_function <- checkmate[[sprintf("assert_%s", type)]]
        if(is.function(assert_function)) {
          li <- list()
          li[[type]] <- list()
          private$validators <- c(li, private$validators)
        }
      }
    },

    format = function(...) {
      nms <- names(private$validators)

      ret <- sprintf("<RAVEVariableConstraints>[type=`%s`,n=%d]", self$type, length(nms))
      if(length(nms)) {
        s <- paste(utils::capture.output(utils::str(private$validators)), collapse = "\n")
        ret <- sprintf("%s\nValidators - %s", ret, s)
      }
      ret
    },

    assert = function(x, .var.name = checkmate::vname(x),
                      on_error = c("stop_on_error", "all_errors", "warning", "message", "muffle"),
                      .collection = NULL) {
      on_error <- match.arg(on_error)
      stop_on_error <- on_error == "stop_on_error"
      force(x)
      force(.var.name)
      nms <- names(private$validators)
      if(!length(nms)) { return(TRUE) }
      checkmate <- asNamespace("checkmate")
      if(is.null(.collection)) {
        collection <- checkmate::makeAssertCollection()
      } else {
        collection <- .collection
      }
      lapply(nms, function(nm) {
        config <- private$validators[[nm]]
        if(is.function(config)) {
          assert_function <- checkmate::makeAssertionFunction(config)
          args <- list()
        } else {
          assert_function <- checkmate[[sprintf("assert_%s", nm)]]
          args <- config
        }
        call <- as.call(c(
          list(
            assert_function,
            x = quote(x), add = quote(collection), .var.name = .var.name
          ),
          args
        ))
        eval(call)
        if( stop_on_error && !collection$isEmpty() ) {
          checkmate::reportAssertions(collection)
        }
      })
      if( collection$isEmpty() ) { return(TRUE) }

      if( on_error == "all_errors" ) {
        checkmate::reportAssertions(collection)
      }
      msgs <- paste(sprintf("* %s", collection$getMessages()), collapse = "\n")

      if( on_error == "warning" ) {
        catgl("RAVE validation failed: \n{msgs}", level = "WARNING")
      } else if( on_error == "message" ) {
        message("RAVE validation failed: \n", msgs)
      }
      return( collection )
    }
  ),
  active = list(
    n_validators = function() {
      length(private$validators)
    }
  )
)

RAVEVariable <- R6::R6Class(
  classname = "RAVEVariable",
  portable = TRUE,
  private = list(
    .constraints = NULL,
    .value = NULL,
    .validated = FALSE,
    .validation_error = NULL,

    finalize = function(...) {
      private$.constraints <- NULL
      private$.value <- key_missing()
    }
  ),
  public = list(
    #' @field name name of the setting
    name = character(0L),

    initialize = function(name, initial_value) {
      self$name <- name
      if(missing(initial_value)) {
        private$.value <- key_missing()
      } else {
        private$.value <- initial_value
      }
    },

    format = function(class_name = TRUE, ...) {
      nvalidators <- 0
      if(length(private$.constraints)) {
        nvalidators <- private$.constraints$n_validators
      }
      if(class_name) {
        prefix <- "RAVEVariable: "
      } else {
        prefix <- ""
      }
      if(self$initialized) {
        if(private$.validated) {
          if( inherits(private$.validation_error, "error") ) {
            init <- " (validation failed)"
          } else {
            init <- sprintf(": %s", paste(format(private$.value), collapse = " "))
            if(nchar(init) > 10) {
              init <- sprintf("%s...", substring(init, 1, 10))
            }
            if(grepl(pattern = "\n", x = init)) {
              init <- sprintf("%s...", strsplit(init, "\n")[[1]][[1]])
            }
          }
        } else {
          init <- " (not yet validated)"
        }
      } else {
        init <- " (missing)"
      }
      sprintf("<%s%s>\t[type=%s,constrants=%d]%s", prefix, self$name, self$type, nvalidators, init)
    },

    use_constraints = function(constraints, .i, ...) {
      if(is.null(constraints)) {
        private$.constraints <- NULL
      } else if(is.character(constraints)) {
        if(!missing(.i) && is.function(.i)) {
          config <- list(.i)
        } else {
          if(missing(.i)) {
            config <- list(list(...))
          } else {
            config <- list(list(.i, ...))
          }
        }
        names(config) <- constraints
        private$.constraints <- RAVEVariableConstraints$new(constraints, config)
      } else {
        if(!inherits(constraints, "RAVEVariableConstraints")) {
          stop("RAVEVariable$use_constraints: `constraints` must be a character or a `RAVEVariableConstraints` instance.")
        }
        private$.constraints <- constraints
      }
      private$.validated <- FALSE
      private$.validation_error <- NULL
      invisible(self)
    },

    set_value = function(x, validate = TRUE, on_error = NULL) {
      if( !validate ) {
        private$.value <- x
        return(self)
      }
      old_v <- private$.value
      old_validated_flag <- private$.validated
      old_validation_error <- private$.validation_error
      suc <- FALSE
      on.exit({
        if(!suc) {
          private$.value <- old_v
          private$.validated <- old_validated_flag
          private$.validation_error <- old_validation_error
        }
      })
      private$.value <- x
      tryCatch({
        self$validate()
        suc <- TRUE
      }, error = function(e) {
        if( is.function(on_error) ) {
          private$.value <- on_error(e, old_v)
          self$validate()
          suc <- TRUE
        } else {
          stop(e)
        }
      })
      return(invisible())
    },

    get_value = function(validate = TRUE) {
      if( validate ) {
        if( !private$.validated ) {
          self$validate()
        }
        if( inherits(private$.validation_error, "error") ) {
          stop(private$.validation_error)
        }
      }
      return(private$.value)
    },

    validate = function(on_error = c("stop_on_error", "all_errors", "warning", "message", "muffle"), .collection = NULL) {
      on_error = match.arg(on_error)
      private$.validated <- TRUE
      private$.validation_error <- NULL
      collection <- TRUE
      if(!is.null(private$.constraints)) {
        tryCatch({
          collection <- private$.constraints$assert(
            private$.value, .var.name = self$name,
            on_error = on_error,
            .collection = .collection
          )
          if(!isTRUE(collection) && !collection$isEmpty()) {
            msgs <- paste(sprintf("* %s", collection$getMessages()),
                          collapse = "\n")
            private$.validation_error <- simpleError(message = msgs)
          }
        }, error = function(e) {
          private$.validation_error <- e
          stop(e)
        })
      }
      return(collection)
    }
  ),
  active = list(
    #' @field constraints instance of \code{\link{RAVEVariableConstraints}},
    #' used to validate the input
    constraints = function() {
      private$.constraints
    },

    type = function() {
      if(is.null(self$constraints)) { return("unknown") }
      return( self$constraints$type )
    },

    value = function(v) {
      if(!missing(v)) {
        self$set_value(v)
      }
      return(self$get_value())
    },

    initialized = function() {
      return(!inherits(private$.value, "key_missing"))
    }
  )
)

#' @title Create \code{'RAVE'} constrained variables
#' @description
#' Create a variable that automatically validates
#'
#' @param name \code{character(1)}, variable name
#' @param initial_value initial value, if missing, then variable will be
#' assigned with an empty list with class name \code{'key_missing'}
#' @param constraints,... when \code{constraints} is an instance of
#' \code{RAVEVariableConstraints}, \code{...} will be ignored. When
#' \code{constraints} is a string, then \code{constraints} will be passed to
#' \code{new_constraints} (see argument \code{type}), and \code{...} will be
#' packed as assertion parameters (see \code{assertions})
#' @param type variable type; \code{checkmate::assert_*} will be automatically
#' applied if applicable
#' @param assertions named list; each name stands for an assertion type, and
#' the corresponding item can be one of the follows; please see 'Examples'
#' for usages.
#' \describe{
#' \item{list of arguments or \code{NULL}}{name of the assertion must be a valid
#' assertion function in package \code{checkmate}. For example,
#' \code{list(numeric=NULL)} will call \code{checkmate::assert_numeric} when
#' value is validated}
#' \item{a function}{name of the assertion can be arbitrary, users are in
#' charge of the validation function. This function should take only one
#' argument and return either \code{TRUE} if the validation passes, or
#' a character of the error message.}
#' }
#'
#'
#'
#' @examples
#'
#' # ---- Basic usage ----------------------------------------
#' analysis_range <- new_constrained_variable("Analysis range")
#'
#' # Using checkmates::assert_numeric
#' analysis_range$use_constraints(
#'   constraints = "numeric",
#'   any.missing = FALSE,
#'   len = 2,
#'   sorted = TRUE,
#'   null.ok = FALSE
#' )
#'
#' analysis_range$initialized # FALSE
#' print(analysis_range)
#'
#' # set value
#' analysis_range$set_value(c(1, 2))
#'
#' # get value
#' analysis_range$value   # or $get_value()
#'
#' # ---- Fancy constraints ------------------------------------
#' # construct an analysis range between -1~1 or 4~10
#' time_window <- validate_time_window(c(-1, 1, 4, 10))
#' analysis_range <- new_constrained_variable("Analysis range")
#' analysis_range$use_constraints(
#'   constraints = new_constraints(
#'     type = "numeric",
#'     assertions = list(
#'       # validator 1
#'       "numeric" = list(
#'         any.missing = FALSE,
#'         len = 2,
#'         sorted = TRUE,
#'         null.ok = FALSE
#'       ),
#'
#'       # validator 2
#'       "range" = function(x) {
#'         check <- FALSE
#'         if(length(x) == 2) {
#'           check <- sapply(time_window, function(w) {
#'             if(
#'               x[[1]] >= w[[1]] &&
#'               x[[2]] <= w[[2]]
#'             ) { return (TRUE) }
#'             return( FALSE )
#'           })
#'         }
#'         if(any(check)) { return(TRUE) }
#'
#'         valid_ranges <- paste(
#'           sapply(time_window, function(w) {
#'             paste(sprintf("%.2f", w), collapse = ",")
#'           }),
#'           collapse = "] or ["
#'         )
#'         return(sprintf("Invalid range: must be [%s]", valid_ranges))
#'       }
#'     )
#'   )
#' )
#'
#' # validate and print out error messages
#' # remove `on_error` argument to stop on errors
#' analysis_range$validate(on_error = "message")
#'
#' # Try with values (-2,1) instead of c(0,1)
#' analysis_range$value <- c(0, 1)
#'
#' print(analysis_range)
#'
#'
#' @export
new_constraints <- function(type, assertions = NULL) {
  inst <- RAVEVariableConstraints$new(type = type, assertions = assertions)
  return( inst )
}

#' @rdname new_constraints
#' @export
new_constrained_variable <- function(
    name, initial_value, constraints = NULL, ...) {
  if(missing(initial_value)) {
    res <- RAVEVariable$new(name = name)
  } else {
    res <- RAVEVariable$new(name = name, initial_value = initial_value)
  }
  if(!is.null(constraints)) {
    res$use_constraints(constraints = constraints, ...)
  }
  res
}
