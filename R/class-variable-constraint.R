store_item <- function(value) {
  if(inherits(value, c("RAVEVariable", "RAVEVariableConstraints", "RAVEVariableCollection", "RAVEVariableCollectionWrapper"))) {
    return(value$store())
  }
  value
}
store_list <- function(value) {
  if(is.list(value) && !inherits(value, "raveio_store")) {
    vnames <- names(value)
    value <- structure(
      names = vnames,
      lapply(value, function(v) {
        store_list(v)
      })
    )
  } else {
    value <- store_item(value)
  }
  value
}

restore_list <- function(value, env = parent.frame()) {
  if(!is.list(value)) { return(value) }

  force(env)

  if( !inherits(value, "raveio_store") ) {
    vnames <- names(value)
    return(structure(
      names = vnames,
      lapply(value, function(v) {
        restore_list(v, env = env)
      })
    ))
  }

  # get generator
  if(length(value$generator)) {
    cls <- value$generator
  } else {
    cls <- class(value)
    cls <- cls[startsWith(cls, "raveio_store.")]
    cls <- gsub("^raveio_store.", "", cls)
  }
  if(length(cls)) {
    raveio <- asNamespace("raveio")
    def <- NULL
    for(cname in cls) {
      def <- get0(x = cname, envir = raveio,
                  ifnotfound = get0(cname, envir = env, ifnotfound = NULL))
      if(R6::is.R6Class(def)) {
        break
      }
    }
    if(R6::is.R6Class(def)) {
      value <- def$new()$restore(x = value, env = env)
    } else {
      stop("Cannot find any class definition for the following classes: ",
           paste(cls, collapse = ", "))
    }
  }

  return(value)

}

#' @title Class definition for 'RAVE' variable constraints
#' @description
#' See \code{\link{new_constraints}} for constructor function.
#'
#' @export
RAVEVariableConstraints <- R6::R6Class(
  classname = "RAVEVariableConstraints",
  portable = TRUE,
  lock_class = FALSE,
  private = list(
    validators = NULL
  ),
  public = list(

    #' @field type \code{character(1)}, type indicator
    type = character(0L),

    #' @description Constructor method
    #' @param type type of the variable; default is \code{'UnboundedConstraint'}
    #' @param assertions named list of the constraint parameters. The names of
    #' \code{assertions} will be used to indicate the constraint
    #' type, and the values are the constraint parameters.
    #' @returns Initialized instance
    initialize = function(type = "UnboundedConstraint", assertions = NULL) {
      self$type <- type
      # self$validators <- checkmate::makeAssertCollection()

      if( is.character(assertions) ) {
        nms <- assertions
        assertions <- as.list(assertions)
      } else {
        nms <- names(assertions)
      }

      checkmate::assert_named(assertions)
      checkmate <- asNamespace('checkmate')
      if(!length(assertions)) {
        private$validators <- list()
      } else {
        private$validators <- structure(
          names = nms,
          lapply(nms, function(nm) {
            config <- assertions[[nm]]
            if( is.language(config) ) {
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

    #' @description Format method
    #' @param ... ignored
    #' @returns Formatted characters
    format = function(...) {
      nms <- names(private$validators)

      prefix <- self$generator$classname

      ret <- sprintf("<%s>[type=`%s`,n=%d]", prefix, self$type, length(nms))
      if(length(nms)) {
        s <- paste(utils::capture.output(utils::str(private$validators)), collapse = "\n")
        ret <- sprintf("%s\nValidators - %s", ret, s)
      }
      ret
    },

    #' @description Validate the constraints
    #' @param x value to validate
    #' @param env environment of validation (used when assertions are
    #' expressions)
    #' @param data named list of additional data to be used for evaluation if
    #' constraint is an expression
    #' @param .var.name descriptive name of \code{x}
    #' @param on_error error handler, default is \code{'error'}:
    #' stop on first validation error
    #' @returns Either \code{TRUE} if passed or a collection of assertion
    #' failures (or errors)
    assert = function(
      x, .var.name = checkmate::vname(x),
      on_error = c("error", "warning", "message", "muffle"),
      env = parent.frame(), data = NULL
    ) {

      on_error <- match.arg(on_error)

      collection <- checkmate::makeAssertCollection()

      res <- self$check(x, env = env, data = data)
      checkmate::makeAssertion(x, res, .var.name, collection)

      if( collection$isEmpty() ) { return(TRUE) }

      msgs <- paste(sprintf("* %s", collection$getMessages()), collapse = "\n")

      switch(
        on_error,
        "error" = checkmate::reportAssertions(collection),
        "warning" = catgl("RAVE validation failed: \n{msgs}", level = "WARNING"),
        "message" = message("RAVE validation failed: \n", msgs)
      )

      return( collection )
    },

    #' @description Check if the value is valid with no error raised
    #' @param x valid to be validated
    #' @param env environment to evaluate validation expressions
    #' @param data named list of additional data to be used for evaluation if
    #' constraint is an expression
    #' @returns \code{TRUE} if valid, otherwise returns the error message
    check = function(x, env = parent.frame(), data = NULL) {
      force(x)
      nms <- names(private$validators)
      if(!length(nms)) { return(TRUE) }
      checkmate <- asNamespace("checkmate")
      data_env <- list2env(as.list(data), parent = env)
      for(nm in nms) {
        config <- private$validators[[nm]]
        if(is.language(config)) {
          f <- function(.x) {}
          body(f) <- config
          environment(f) <- data_env
          call <- quote(f(x))
        } else {
          check_function <- checkmate[[sprintf("check_%s", nm)]]
          args <- config
          call <- as.call(c(
            list(
              quote(check_function),
              x = quote(x)
            ),
            args
          ))
        }
        res <- tryCatch({
          eval(call)
        }, error = function(e) {
          sprintf("Error when checking `%s` (validator=%s): %s",
                  self$type, nm, paste(e$message, collapse = ""))
        })
        if(!isTRUE(res)) { return(res) }
      }
      return( TRUE )
    },

    #' @description Convert constraint to atomic list, used for serializing
    #' @param ... ignored
    #' @returns A list of constraint data that can be passed into
    #' \code{$restore} method
    store = function(...) {
      validators <- private$validators
      vnames <- names(validators)
      validators <- unname(lapply(vnames, function(nm) {
        config <- validators[[nm]]
        if(is.language(config)) {
          return(list(
            name = nm,
            type = "expression",
            data = deparse(config)
          ))
        } else {
          return(list(
            name = nm,
            type = "list",
            data = config
          ))
        }
      }))

      structure(
        class = c("raveio_store.RAVEVariableConstraints", "raveio_store"),
        list(
          type = self$type,
          validators = validators,
          generator = self$generator$classname
        )
      )
    },

    #' @description Restores from atomic list generated by \code{$store()}
    #' @param x atomic list
    #' @param ... ignored
    #' @returns \code{RAVEVariableConstraints} instance
    restore = function(x, ...) {
      stopifnot(inherits(x, "raveio_store.RAVEVariableConstraints"))
      self$type <- x$type

      vnames <- vapply(X = as.list(x$validators), FUN = "[[", FUN.VALUE = "", "name", USE.NAMES = FALSE)
      if(length(vnames)) {
        private$validators <- structure(
          names = vnames,
          lapply(seq_along(vnames), function(ii) {
            config <- x$validators[[ ii ]]
            if(identical(config$type, "expression")) {
              if(!length(config$data)) {
                return(quote({ TRUE }))
              } else {
                re <- tryCatch({
                  parse(text = config$data)
                }, error = function(e) {
                  return(quote({ TRUE }))
                })
                return(re)
              }
            }
            re <- as.list(config$data)
            re$.var.name <- NULL
            re$x <- NULL
            re$add <- NULL
            return(re)
          })
        )
      }
      invisible(self)
    }

  ),
  active = list(
    #' @field n_validators Number of validators
    n_validators = function() {
      length(private$validators)
    },

    #' @field isRAVEVariableConstraints always true
    isRAVEVariableConstraints = function() {
      TRUE
    },

    #' @field generator class definition
    generator = function() {
      RAVEVariableConstraints
    }
  )
)

#' @title Class definition of 'RAVE' constrained variable
#' @description
#' See \code{\link{new_constrained_variable}} for constructor function.
#'
#' @export
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
    },
    deep_clone = function(name, value) {
      switch(
        name,
        ".constraints" = {
          if(!is.null(value)) {
            value <- value$clone(deep = TRUE)
          }
        }
      )
      value
    }
  ),
  public = list(
    #' @field name Description of the variable
    name = character(0L),

    #' @description Constructor function
    #' @param name description of the variable
    #' @param initial_value initial value; default is an empty list of class
    #' \code{"key_missing"}
    initialize = function(name = "Unnamed", initial_value) {
      self$name <- name
      if(missing(initial_value)) {
        private$.value <- key_missing()
      } else {
        private$.value <- initial_value
      }
    },

    #' @description Format method
    #' @param prefix prefix of the string
    #' @param ... ignored
    #' @returns Formatted characters
    format = function(prefix = NULL, ...) {
      nvalidators <- 0
      if(length(private$.constraints)) {
        nvalidators <- private$.constraints$n_validators
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
      if(is.null(prefix)) {
        prefix <- sprintf("<%s>", self$generator$classname)
      } else {
        prefix <- paste(prefix, collapse = " ")
      }
      sprintf("%s[type=%s,constrants=%d,label=%s]%s", prefix, self$type, nvalidators, self$name, init)
    },

    #' @description Set variable validation
    #' @param constraints either a \code{character(1)} or a
    #' \code{\link{RAVEVariableConstraints}} instance. When \code{constraints}
    #' is a string, the value will be the \code{type} of the constraint (
    #' see \code{\link{new_constraints}})
    #' @param .i,... used when \code{constraints} is a string, either \code{.i}
    #' is an expression, or \code{list(.i,...)} forms a list of control
    #' parameters; see \code{assertions} in \code{\link{new_constraints}}.
    #' @returns Self instance
    use_constraints = function(constraints, .i, ...) {
      if(is.null(constraints)) {
        private$.constraints <- NULL
      } else if(is.character(constraints)) {
        if(!missing(.i) && is.language(.i)) {
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

    #' @description Set value
    #' @param x value of the variable
    #' @param env environment in which the validations will be evaluated
    #' @param validate whether to validate if \code{x} is legit; if set to
    #' \code{TRUE} and \code{x} is invalid, then the values will not be set.
    #' @param on_error a function takes two arguments: the error instance and
    #' old value; the returned value will be used to re-validate.
    #' Default is \code{NULL}, which is identical to returning the old value
    #' and stop on error.
    #' @returns Self
    set_value = function(x, env = parent.frame(), validate = TRUE, on_error = NULL) {
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
        self$validate(env = env)
        suc <- TRUE
      }, error = function(e) {
        if( is.function(on_error) ) {
          private$.value <- on_error(e, old_v)
          self$validate(env = env)
          suc <<- TRUE
        } else {
          stop(e)
        }
      })
      return(invisible(self))
    },

    #' @description Get value
    #' @param ... ignored
    #' @returns Current value
    get_value = function(...) {
      return(private$.value)
    },

    #' @description Check if the value is valid
    #' @param env,on_error passed to
    #' \code{RAVEVariableConstraints$assert}.
    #' @returns See \code{\link{RAVEVariableConstraints}}
    validate = function(
      env = parent.frame(),
      on_error = c("error", "warning", "message", "muffle")
    ) {
      on_error = match.arg(on_error)

      collection <- checkmate::makeAssertCollection()

      force(env)
      res <- self$check(env = env)
      checkmate::makeAssertion(self$get_value(env = env), res, self$name, collection)

      private$.validated <- TRUE
      private$.validation_error <- NULL
      if( collection$isEmpty() ) { return(TRUE) }

      msgs <- paste(sprintf("* %s", collection$getMessages()), collapse = "\n")
      private$.validation_error <- simpleError(msgs)

      switch(
        on_error,
        "error" = checkmate::reportAssertions(collection),
        "warning" = catgl("RAVE validation failed: \n{msgs}", level = "WARNING"),
        "message" = message("RAVE validation failed: \n", msgs)
      )

      return( collection )
    },

    #' @description Check if the value is valid with no error raised
    #' @param env environment to evaluate validation expressions
    #' @returns \code{TRUE} if valid, otherwise returns the error message
    check = function(env = parent.frame()) {
      # private$.validated <- TRUE
      # private$.validation_error <- NULL

      if( !length(private$.constraints) ) { return(TRUE) }

      res <- private$.constraints$check(
        self$value, env = env,
        data = list(.self = self, .private = private)
      )

      return(res)
    },

    #' @description Convert constraint to atomic list, used for serializing
    #' @param ... ignored
    #' @returns A list of constraint data that can be passed into
    #' \code{$restore} method
    store = function(...) {
      constraints <- private$.constraints
      if(!is.null(constraints)) {
        constraints <- constraints$store()
      }
      err <- private$.validation_error
      if(inherits(err, "error")) {
        err <- list(message = err$message, call_str = deparse(err$call), class = class(err))
      }

      structure(
        class = c("raveio_store.RAVEVariable", "raveio_store"),
        list(
          name = self$name,
          value = store_list(private$.value),
          constraints = constraints,
          generator = self$generator$classname
        )
      )
    },

    #' @description Restores from atomic list generated by \code{$store()}
    #' @param x atomic list
    #' @param env environment where to query the class definitions
    #' @param ... ignored
    #' @returns \code{RAVEVariable} instance
    restore = function(x, env = parent.frame(), ...) {
      stopifnot(inherits(x, "raveio_store.RAVEVariable"))
      if(!is.null(x$constraints)) {
        constraints <- RAVEVariableConstraints$new()$restore(x = x$constraints)
        private$.constraints <- constraints
      }
      self$name <- x$name
      private$.value <- restore_list(x$value, env = env)
      private$.validated <- FALSE
      private$.validation_error <- NULL
      invisible(self)
    }
  ),
  active = list(
    #' @field constraints instance of \code{\link{RAVEVariableConstraints}},
    #' used to validate the input
    constraints = function() {
      private$.constraints
    },

    #' @field isRAVEVariable always true
    isRAVEVariable = function() {
      TRUE
    },

    #' @field type constraint type
    type = function() {
      if(is.null(self$constraints)) { return("unknown") }
      return( self$constraints$type )
    },

    #' @field value value of the variable
    value = function(v) {
      if(!missing(v)) {
        env <- parent.frame()
        self$set_value(v, env = env)
      }
      return(self$get_value(env = env))
    },

    #' @field initialized whether value is missing (value might not be valid)
    initialized = function() {
      return(!inherits(private$.value, "key_missing"))
    },

    #' @field generator class definition
    generator = function() {
      RAVEVariable
    }
  )
)

RAVEVariableBinding <- R6::R6Class(
  classname = "RAVEVariableBinding",
  inherit = RAVEVariable,
  portable = TRUE,
  lock_class = FALSE,
  public = list(

    initialize = function(name = "Unnamed", initial_value = NULL, quoted = FALSE) {
      if(!quoted) {
        initial_value <- substitute(initial_value)
      }
      super$initialize(name = name, initial_value = initial_value)
    },

    set_value = function(x, ...) {
      if(!is.language(x)) {
        stop("RAVEVariableBinding$set_value: `x` must be quoted language")
      }
      try(
        silent = TRUE,
        {
          x <- utils::removeSource(x)
          environment(x) <- NULL
        }
      )
      super$set_value(x, validate = FALSE)
      return(invisible(self))
    },

    get_value = function(env = parent.frame()) {
      if(is.null(private$.value)) { return(NULL) }
      return(eval(private$.value, envir = new.env(parent = env)))
    },

    validate = function(
      env = parent.frame(),
      on_error = c("error", "warning", "message", "muffle")
    ) {
      on_error = match.arg(on_error)

      collection <- checkmate::makeAssertCollection()

      force(env)
      res <- self$check(env = env)
      checkmate::makeAssertion(self$get_value(env = env), res, self$name, collection)

      private$.validated <- TRUE
      private$.validation_error <- NULL
      if( collection$isEmpty() ) { return(TRUE) }

      msgs <- paste(sprintf("* %s", collection$getMessages()), collapse = "\n")
      private$.validation_error <- simpleError(msgs)

      switch(
        on_error,
        "error" = checkmate::reportAssertions(collection),
        "warning" = catgl("RAVE validation failed: \n{msgs}", level = "WARNING"),
        "message" = message("RAVE validation failed: \n", msgs)
      )

      return( collection )
    },

    check = function(env = parent.frame()) {
      # private$.validated <- TRUE
      # private$.validation_error <- NULL

      if( !length(private$.constraints) ) { return(TRUE) }

      res <- private$.constraints$check(
        self$get_value(env = env), env = env,
        data = list(.self = self, .private = private)
      )

      return(res)
    }
  ),
  active = list(
    expr = function() {
      private$.value
    },
    generator = function() {
      RAVEVariableBinding
    },
    value = function(v) {
      stop("Please use x$get_value(env=) instead for binding variable")
      env <- parent.frame()
      if(!missing(v)) {
        self$set_value(v, env = env)
      }
      return(self$get_value(env = env))
    }
  )
)

#' @export
`[.RAVEVariable` <- function(x, ...) {
  x$get_value()
}

#' @export
`[.RAVEVariableBinding` <- function(x, ..., env = parent.frame()) {
  x$get_value(env = env)
}

#' @export
`[<-.RAVEVariable` <- function(x, ..., value) {
  stop("Please use `x$set_value` to assign data.")
}

#' @title Create \code{'RAVE'} constrained variables
#' @description
#' Create a variable that automatically validates
#'
#' @param name \code{character(1)}, variable name
#' @param initial_value initial value, if missing, then variable will be
#' assigned with an empty list with class name \code{'key_missing'}
#' @param expr expression for binding
#' @param quoted whether \code{expr} is quoted, default is false
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
#'       "range" = quote({
#'         check <- FALSE
#'         if(length(.x) == 2) {
#'           check <- sapply(time_window, function(w) {
#'             if(
#'               .x[[1]] >= w[[1]] &&
#'               .x[[2]] <= w[[2]]
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
#'       })
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
#' analysis_range[]
#'
#' # Change the context
#' time_window <- validate_time_window(c(0, 0.5))
#'
#' # re-validate will error out
#' analysis_range$validate(on_error = "message")
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

#' @rdname new_constraints
#' @export
new_constrained_binding <- function(name, expr, quoted = FALSE, constraints = NULL, ...) {
  if(!quoted) {
    expr <- substitute(expr)
  }
  res <- RAVEVariableBinding$new(name = name, initial_value = expr, quoted = TRUE)
  if(!is.null(constraints)) {
    res$use_constraints(constraints = constraints, ...)
  }
  res
}
