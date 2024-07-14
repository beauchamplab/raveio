add_mask_collection <- function(env, self) {
  needs_enclos <- FALSE
  self2 <- get0(".collection", envir = env, ifnotfound = NULL)
  if(!inherits(self2, "RAVEVariableCollectionWrapper")) {
    needs_enclos <- TRUE
  } else if ( !identical(.subset2(self2, "impl"), self) ) {
    needs_enclos <- TRUE
  }
  if( needs_enclos ) {
    env <- new.env(parent = env)
    env$.collection <- self$.wrapper
  }
  env
}

#' @title Class definition of 'RAVE' constrained variable collection
#' @description
#' See \code{\link{new_variable_collection}} for construction
#'
#' @export
RAVEVariableCollection <- R6::R6Class(
  classname = "RAVEVariableCollection",
  portable = TRUE,
  private = list(
    .variables = NULL,
    .constraints = NULL,
    .name = character(0L),

    finalize = function(...) {
      private$.constraints <- NULL
    },
    prefix = function() {
      p <- self$generator$classname
      if(nzchar(self$name)) {
        return(sprintf("%s (%s)", p, self$name))
      } else {
        return(p)
      }
    },
    deep_clone = function(name, value) {
      if(!is.null(value) && !is.function(value)) {
        switch(
          name,
          ".constraints" = {
            value <- value$clone(deep = TRUE)
          },
          ".variables" = {
            vnames <- names(value)
            value <- dipsaus::list_to_fastmap2(
              structure(
                names = vnames,
                lapply(vnames, function(nm) {
                  inst <- value[[nm]]
                  if( R6::is.R6(inst) ) {
                    inst <- tryCatch({
                      inst$clone(deep = TRUE)
                    }, error = function(e) { inst })
                  }
                  inst
                })
              )
            )
          }
        )
      }
      value
    }
  ),
  public = list(
    #' @field explicit whether getting and setting values should be explicit.
    #' If true, then all variables must be defined (see \code{$add_variable})
    #' before used.
    explicit = TRUE,

    #' @description Constructor
    #' @param name descriptive name of the collection
    #' @param explicit see field \code{explicit}
    initialize = function(name = "", explicit = TRUE) {
      private$.name <- name
      self$explicit <- explicit
      private$.variables <- dipsaus::fastmap2()
    },

    #' @description Format method
    #' @param ... ignored
    #' @returns Formatted characters
    format = function(...) {
      nms <- names(private$.variables)
      s <- unlist(lapply(nms, function(nm) {
        inst <- private$.variables[[nm]]
        if(inherits(inst, "RAVEVariable")) {
          return(sprintf(
            "%s: %s",
            nm,
            paste(private$.variables[[nm]]$format(prefix = ""), collapse = "")
          ))
        }
        return(sprintf("%s: %s", nm, paste(format(inst), collapse = " ")))
      }))

      if(length(s)) {
        s <- sprintf(
          "<%s>\n%s",
          private$prefix(),
          paste(sprintf("  - %s", s), collapse = "\n")
        )
      } else {
        s <- sprintf("<%s> (empty)", private$prefix())
      }
      s
    },

    #' @description Registers a variable, must run if the collection is explicit
    #' @param id variable 'ID'
    #' @param var a \code{\link{RAVEVariable}} or \code{RAVEVariableCollection}
    #' instance if the variable is bounded, or simply normal R object (
    #' then the variable will have no constraint)
    #' @returns Self
    add_variable = function(id, var) {
      if(length(id) != 1 || grepl("^[^a-zA-Z]", id)) {
        stop("add_variable: `id` must be a single string that starts with letters")
      }
      if(id %in% ls(self)) {
        stop(sprintf("add_variable: `id` [%s] is reserved.", id))
      }
      if(missing(var)) {
        var <- key_missing()
      }
      private$.variables[[ id ]] <- var
      invisible(self$.wrapper)
    },

    #' @description Remove a variable
    #' @param id variable 'ID'
    #' @returns The removed variable
    remove_variable = function(id) {
      private$.variables[["@remove"]](id)
    },

    #' @description Check whether a variable exists
    #' @param id variable 'ID'
    #' @returns \code{TRUE} if found, otherwise \code{FALSE}
    has_variable = function(id) {
      private$.variables[["@has"]](id)
    },

    #' @description Set value of a variable
    #' @param id variable 'ID'
    #' @param value the value to be set
    #' @param env,... passed to \code{RAVEVariable$set_value}
    #' @seealso \code{\link{RAVEVariable}}
    #' @returns Self
    set_value = function(id, value, env = parent.frame(), ...) {
      exists <- private$.variables[["@has"]](id)
      if(!exists) {
        if(self$explicit) {
          stop(sprintf("%s: cannot set missing variable `%s`", private$prefix(), id))
        } else {
          if(length(id) != 1 || grepl("^[^a-zA-Z]", id)) {
            stop("set_value: `id` must be a single string that starts with letters")
          }
          if(id %in% ls(self)) {
            stop(sprintf("set_value: `id` [%s] is reserved.", id))
          }
        }
      }

      force(value)

      inst <- private$.variables[[ id ]]
      if(inherits(inst, "RAVEVariable")) {

        env <- add_mask_collection(env, self)

        inst$set_value(value, env = env, ...)
      } else if(inherits(inst, c("RAVEVariableCollection", "RAVEVariableCollectionWrapper"))) {
        vnames <- names(value)
        for(nm in vnames) {
          inst$set_value(nm, value[[nm]], env = env, ...)
        }
      } else {
        private$.variables[[ id ]] <- value
      }
      invisible(self$.wrapper)
    },

    #' @description Get value of a variable
    #' @param id variable 'ID'
    #' @param env environment of evaluation
    #' @param get_definition whether to return the variable definition instance
    #' (\code{RAVEVariable} or \code{RAVEVariableCollection}) instead
    #' of the value; default is false
    #' @param ifnotfound default value if not found; default is \code{NULL}
    #' @returns The variable value if variable if found and
    #' \code{get_definition} is false; or the variable definition if variable
    #' is found and is \code{RAVEVariable} or \code{RAVEVariableCollection};
    #' or \code{ifnotfound} if the variable does not exist
    get_value = function(id, env = parent.frame(), get_definition = FALSE, ifnotfound = NULL) {
      if(!self$has_variable(id)) { return( ifnotfound ) }
      inst <- private$.variables[[ id ]]
      if( !get_definition ) {

        if(inherits(inst, c(
          "RAVEVariable", "RAVEVariableCollectionWrapper", "RAVEVariableCollection"
        ))) {
          env <- add_mask_collection(env, self)

          if( inherits(inst, "RAVEVariable") ) {
            inst <- inst$get_value(env = env)
          } else if ( inherits(inst, "RAVEVariableCollection") ) {
            inst <- inst$as_list(env = env)
          } else if ( inherits(inst, "RAVEVariableCollectionWrapper") ) {
            inst <- inst$as_list(env = env)
          }
        }

      }
      return( inst )
    },

    #' @description Convert to list
    #' @param env environment of evaluation
    #' @returns The variable values in list
    as_list = function(env = parent.frame()) {
      nms <- names(private$.variables)
      structure(
        names = nms,
        lapply(nms, function(nm) {
          self$get_value(id = nm, get_definition = FALSE, env = env)
        })
      )
    },

    #' @description Set collection validation
    #' @param x either a \code{NULL} or an expression with global variables
    #' \code{x}, \code{self}, \code{private}, and \code{defs}
    #' Mainly used to validate the values of multiple variables (some
    #' variables are dependent or bounded by other variables)
    #' @returns Nothing
    use_constraints = function(x) {
      if(is.null(x) || is.language(x)) {
        private$.constraints <- RAVEVariableConstraints$new(
          type = "CollectionValidator",
          assertions = list("CollectionValidator" = x)
        )
      } else if(inherits(x, "RAVEVariableConstraints")) {
        private$.constraints <- x
      } else {
        stop("RAVEVariableCollection$use_constraints: `x` must be a quoted expression, RAVEVariableConstraints instance, or `NULL`.")
      }
      invisible()
    },

    #' @description Run validation
    #' @param env environment to evaluate validation expressions
    #' @param on_error character, error handler
    #' @returns \code{TRUE} if valid, or raises errors by default
    validate = function(
      env = parent.frame(),
      on_error = c("error", "warning", "message", "muffle")
    ) {
      on_error <- match.arg(on_error)
      collection <- checkmate::makeAssertCollection()

      force(env)
      res <- self$check(env = env)
      checkmate::makeAssertion(NULL, res, self$name, collection)

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
    #' @param env environment to evaluate validation expressions
    #' @returns \code{TRUE} if valid, otherwise returns the error message
    check = function(env = parent.frame()) {

      env <- add_mask_collection(env, self)

      nms <- names(private$.variables)
      data <- list()
      res <- tryCatch({
        data <- structure(
          names = nms,
          lapply(nms, function(nm) {
            var <- private$.variables[[nm]]

            if(inherits(var, "RAVEVariable")) {
              var$validate(on_error = 'error', env = env)
              var <- var$get_value(env = env)
            }

            var
          })
        )
        TRUE
      }, error = function(e) {
        sprintf("\n  %s", e$message)
      })
      if(!isTRUE(res)) { return(res) }

      if(is.null(private$.constraints)) { return(TRUE) }

      res <- private$.constraints$check(x = data, env = env)

      return(res)
    },

    #' @description Convert constraint to atomic list, used for serializing
    #' @param ... ignored
    #' @returns A list of constraint data that can be passed into
    #' \code{$restore} method
    store = function(...) {
      vnames <- names(private$.variables)
      variables <- structure(
        names = vnames,
        lapply(vnames, function(nm) {
          inst <- private$.variables[[ nm ]]
          store_list(inst)
        })
      )

      constraints <- private$.constraints
      if(!is.null(constraints)) {
        constraints <- constraints$store()
      }
      structure(
        class = c("raveio_store.RAVEVariableCollection", "raveio_store"),
        list(
          name = private$.name,
          explicit = self$explicit,
          variables = variables,
          constraints = constraints,
          generator = self$generator$classname
        )
      )
    },

    #' @description Restores from atomic list generated by \code{$store()}
    #' @param x atomic list
    #' @param env environment where to query the class definitions
    #' @param clear whether to clear the current variables; default is false
    #' @param ... ignored
    #' @returns \code{RAVEVariableCollection} instance
    restore = function(x, env = parent.frame(), clear = FALSE, ...) {
      stopifnot(inherits(x, "raveio_store.RAVEVariableCollection"))

      self$explicit <- FALSE
      # vnames <- names(x$variables)
      variables <- restore_list(x$variables, env = env)
      #   structure(
      #   names = vnames,
      #   lapply(vnames, function(nm) {
      #     inst <- x$variables[[ nm ]]
      #     if(inherits(inst, "raveio_store")) {
      #       cls <- class(inst)
      #       cls <- cls[startsWith(cls, "raveio_store.")]
      #       if(length(cls)) {
      #         cls <- gsub("^raveio_store.", "", cls)
      #         raveio <- asNamespace("raveio")
      #         def <- NULL
      #         for(cname in cls) {
      #           def <- get0(x = cname, envir = raveio,
      #                       ifnotfound = get0(cname, envir = env, ifnotfound = NULL))
      #           if(R6::is.R6Class(def)) {
      #             break
      #           }
      #         }
      #         if(R6::is.R6Class(def)) {
      #           inst <- def$new()$restore(x = inst, ...)
      #         } else {
      #           stop("Cannot find any class definition for the following classes: ",
      #                paste(cls, collapse = ", "))
      #         }
      #       }
      #     }
      #     inst
      #     # self$add_variable(nm, inst)
      #     # NULL
      #   })
      # )

      if(!is.null(x$constraints)) {
        constraints <- RAVEVariableConstraints$new()$restore(x = x$constraints, env = env)
        private$.constraints <- constraints
      }

      if(clear) {
        private$.variables[["@reset"]]()
      }
      dipsaus::list_to_fastmap2(variables, map = private$.variables)

      self$name <- x$name
      self$explicit <- x$explicit

      return(self$.wrapper)
    }
  ),
  active = list(
    #' @field .wrapper wrapper instance of current variable collection
    .wrapper = function() {
      RAVEVariableCollectionWrapper$new(self)
    },

    #' @field generator class definition
    generator = function() {
      RAVEVariableCollection
    },

    #' @field isRAVEVariableCollection always true
    isRAVEVariableCollection = function() {
      TRUE
    },

    #' @field variables map containing the variable definitions
    variables = function() {
      private$.variables
    },

    #' @field varnames variable names
    varnames = function() { return( private$.variables[["@keys"]]() ) },

    #' @field name descriptive name of the collection
    name = function(v) {
      if(!missing(v)) {
        private$.name <- v
      }
      private$.name
    }
  )
)

RAVEVariableCollectionWrapper <- R6::R6Class(
  classname = "RAVEVariableCollectionWrapper",
  cloneable = TRUE,
  portable = FALSE,
  private = list(
    .impl = NULL,

    deep_clone = function(name, value) {
      if(name == ".impl") {
        value <- value$clone(deep = TRUE)
      }
    }
  ),
  public = list(
    initialize = function(impl) {
      stopifnot(inherits(impl, "RAVEVariableCollection"))
      private$.impl <- impl
    },
    format = function(...) {
      s <- private$.impl$format(...)
      s[[1]] <- sprintf("Wrapper of %s", s[[1]])
      s
    }
  ),
  active = list(
    impl = function() {
      private$.impl
    }
  )
)

#' @export
as.list.RAVEVariableCollectionWrapper <- function(x, env = parent.frame(), validate = FALSE, ...) {
  impl <- .subset2(x, "impl")
  if( validate ) {
    impl$validate(env = env)
  }
  return(impl$as_list(env = env))
}

#' @export
`[.RAVEVariableCollectionWrapper` <- function(x, i, ..., env = parent.frame()) {
  impl <- .subset2(x, "impl")
  if(missing(i)) {
    return(impl$as_list(env = env))
  }
  nms <- c(i, ...)
  return(structure(
    names = nms,
    lapply(nms, function(id) {
      impl$get_value(id = id, env = env)
    })
  ))
}

#' @export
`[<-.RAVEVariableCollectionWrapper` <- function(
    x, i, ..., validate = TRUE, env = parent.frame(), value) {
  impl <- .subset2(x, "impl")
  if(missing(i)) {
    nms <- impl$varnames
  } else {
    nms <- unique(unlist(c(i, ...)))
  }
  checkmate::assert_names(names(value), type = "named", must.include = nms)
  lapply(nms, function(id) {
    impl$set_value(id = id, value = value[[id]], env = env, validate = validate)
    NULL
  })
  x
}

#' @export
names.RAVEVariableCollectionWrapper <- function(x) {
  impl <- .subset2(x, "impl")
  c(impl$varnames, names(impl))
}

#' @export
`names<-.RAVEVariableCollectionWrapper` <- function(x, value) {
  stop("Cannot set names of `RAVEVariableCollectionWrapper`")
}

#' @export
`[[.RAVEVariableCollectionWrapper` <- function(x, i, ..., env = parent.frame()) {
  impl <- .subset2(x, "impl")
  if(i %in% impl$varnames) {
    return(impl$get_value(id = i, env = env))
  }
  NULL
}

#' @export
`[[<-.RAVEVariableCollectionWrapper` <- function(
    x, i, ..., validate = TRUE, env = parent.frame(), value) {
  impl <- .subset2(x, "impl")
  if(i %in% names(impl)) {
    impl[[i]] <- value
  } else {
    impl$set_value(id = i, value = value, env = env, validate = validate)
  }
  x
}

#' @export
`$.RAVEVariableCollectionWrapper` <- function(x, name) {
  impl <- .subset2(x, "impl")
  if(name %in% names(impl)) {
    return(impl[[name]])
  }
  if(name %in% impl$varnames) {
    env <- parent.frame()
    return(impl$get_value(id = name, env = env))
  }
  NULL
}

#' @export
`$<-.RAVEVariableCollectionWrapper` <- function(x, name, value) {
  impl <- .subset2(x, "impl")
  if(name %in% names(impl)) {
    impl[[name]] <- value
  } else {
    env <- parent.frame()
    impl$set_value(id = name, value = value, env = env, validate = TRUE)
  }
  x
}

#' @title Create a collection of constraint variables
#' @param name collection name, default is empty
#' @param explicit whether setting and getting variables should be explicit,
#' default is \code{TRUE}, which means trying to get undefined variables
#' will result in errors
#' @param r6_def \code{R6} class generator; default is
#' \code{\link{RAVEVariableCollection}}. This input is for class definitions
#' that are child classes of \code{RAVEVariableCollection}.
#' @returns A \code{RAVEVariableCollectionWrapper} instance
#'
#' @examples
#'
#' collection <- new_variable_collection()
#'
#' # Add unconstrained variables
#' collection$add_variable(id = "title", "Voltage traces")
#'
#' # Add a variable with placeholder
#' collection$add_variable(id = "time_points")
#'
#' # Add variable with constraints
#' collection$add_variable(
#'   id = "analysis_range",
#'   var = new_constrained_variable(
#'     name = "Analysis range",
#'     initial_value = c(0, 1),
#'     constraints = "numeric",
#'     any.missing = FALSE,
#'     len = 2,
#'     sorted = TRUE,
#'     null.ok = FALSE
#'   )
#' )
#'
#' collection$use_constraints(quote({
#'   # `x` is the list of values
#'   time_range <- range(.x$time_points, na.rm = TRUE)
#'   if(
#'     .x$analysis_range[[1]] >= time_range[[1]] &&
#'     .x$analysis_range[[2]] <= time_range[[2]]
#'   ) {
#'     # valid
#'     re <- TRUE
#'   } else {
#'
#'     # error message
#'     re <- sprintf(
#'       "Invalid analysis range, must be within [%.2f, %.2f]",
#'       time_range[[1]], time_range[[2]]
#'     )
#'   }
#'
#'   re
#' }))
#'
#'
#' collection$set_value("time_points", seq(-1, 10, by = 0.5))
#'
#' # validation will pass
#' collection$validate()
#'
#' # Get variable values
#' collection$as_list()
#' collection[]
#'
#' # get one variable
#' collection$get_value("analysis_range")
#'
#' # get unregistered variable
#' collection$get_value("unregistered_variable")
#'
#' # get partial variables with single `[`
#' collection["title", "analysis_range"]
#' collection[c("title", "analysis_range")]
#'
#' collection$set_value("analysis_range", c(-2, 5))
#'
#' \dontrun{
#' collection$validate()
#'
#' # error out when explicit, please either
#' # set explicit=TRUE or register the variable via $add_variable
#' collection$set_value("unregistered_variable", 1)
#'
#' }
#'
#' # turn off explicit variable option
#' collection$explicit <- FALSE
#' collection$set_value("unregistered_variable", 1)
#' collection$get_value("unregistered_variable")
#'
#'
#' @export
new_variable_collection <- function(name = "", explicit = TRUE, r6_def = NULL) {
  if( R6::is.R6Class(r6_def) ) {
    impl <- r6_def$new(name = name, explicit = explicit)
  } else {
    impl <- RAVEVariableCollection$new(name = name, explicit = explicit)
  }
  stopifnot(inherits(impl, "RAVEVariableCollection"))
  impl$.wrapper
}



