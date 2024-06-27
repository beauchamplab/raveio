RAVEVariableCollection <- R6::R6Class(
  classname = "RAVEVariableCollection",
  portable = TRUE,
  private = list(
    .variables = NULL,
    .constraints = NULL,

    finalize = function(...) {
      private$.constraints <- NULL
    },
    prefix = function() {
      if(nzchar(self$name)) {
        return(sprintf("RAVEVariableCollection (%s)", self$name))
      } else {
        return("RAVEVariableCollection")
      }
    }
  ),
  public = list(
    name = character(0L),
    explicit = TRUE,
    initialize = function(name = "", explicit = TRUE) {
      self$name <- name
      self$explicit <- explicit
      private$.variables <- dipsaus::fastmap2()
    },
    format = function(...) {
      nms <- names(private$.variables)
      s <- unlist(lapply(nms, function(nm) {
        inst <- private$.variables[[nm]]
        if(inherits(inst, "RAVEVariable")) {
          return(private$.variables[[nm]]$format(class_name = FALSE))
        }
        return(sprintf("<%s>\t%s", nm, paste(format(inst), collapse = " ")))
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

    add_variable = function(id, var) {
      if(missing(var)) {
        var <- key_missing()
      }
      private$.variables[[ id ]] <- var
    },

    remove_variable = function(id) {
      private$.variables[["@remove"]](id)
    },

    set_variable = function(id, value, missing_ok = NA, ...) {
      if(is.na(missing_ok)) {
        missing_ok <- !self$explicit
      }
      if(!missing_ok) {
        exists <- private$.variables[["@has"]](id)
        if(!exists) {
          stop(sprintf("%s: cannot set missing variable `%s`", private$prefix(), id))
        }
      }
      inst <- private$.variables[[ id ]]
      if(inherits(inst, "RAVEVariable")) {
        inst$set_value(value, ...)
      } else {
        private$.variables[[ id ]] <- value
      }
      invisible()
    },

    get_variable = function(id, get_value = TRUE, missing_ok = NA, validate = TRUE) {
      exists <- private$.variables[["@has"]](id)
      if(!exists) {
        if( is.na(missing_ok) ) {
          missing_ok <- !self$explicit
        }
        if( missing_ok ) { return( key_missing() ) }
        stop(sprintf("%s: cannot get missing variable `%s`", private$prefix(), id))
      }
      inst <- private$.variables[[ id ]]
      if( get_value && inherits(inst, "RAVEVariable") ) {
        inst <- inst$get_value(validate = validate)
      }
      return( inst )
    },

    as_list = function() {
      nms <- names(private$.variables)
      structure(
        names = nms,
        lapply(nms, function(nm) {
          self$get_variable(id = nm, get_value = TRUE,
                            missing_ok = TRUE, validate = FALSE)
        })
      )
    },

    use_constraints = function(x) {
      if(is.null(x) || is.function(x)) {
        private$.constraints <- RAVEVariableConstraints$new(
          type = "CollectionValidator",
          assertions = list("CollectionValidator" = x)
        )
      } else if(inherits(x, "RAVEVariableConstraints")) {
        private$.constraints <- x
      } else {
        stop("RAVEVariableCollection$use_constraints: `x` must be a function, RAVEVariableConstraints instance, or `NULL`.")
      }
      invisible()
    },

    validate = function(on_error = c("stop_on_error", "all_errors", "warning", "message", "muffle")) {
      on_error <- match.arg(on_error)
      collection <- checkmate::makeAssertCollection()

      nms <- names(private$.variables)
      data <- structure(
        names = nms,
        lapply(nms, function(nm) {
          var <- private$.variables[[nm]]

          if(inherits(var, "RAVEVariable")) {
            var$validate(on_error = on_error, .collection = collection)
            var <- var$get_value(validate = FALSE)
          }

          var
        })
      )

      if(collection$isEmpty() && !is.null(private$.constraints)) {
        private$.constraints$assert(
          data,
          .var.name = private$prefix(),
          on_error = on_error,
          .collection = collection
        )
      }

      if(collection$isEmpty()) { return(TRUE) }
      return(collection)
    }
  ),
  active = list(
    variables = function() {
      private$.variables
    },
    varnames = function() { return( x$variables[["@keys"]]() ) }
  )
)

#' @export
as.list.RAVEVariableCollection <- function(x, validate = FALSE, ...) {
  if( validate ) {
    x$validate()
  }
  return(x$as_list())
}

#' @export
`[.RAVEVariableCollection` <- function(x, i, validate = FALSE, ...) {
  if(missing(i)) {
    if( validate ) {
      x$validate()
    }
    return(x$as_list())
  }
  nms <- c(i, ...)
  return(structure(
    names = nms,
    lapply(nms, function(id) {
      x$get_variable(id = id, validate = validate)
    })
  ))
}

#' @export
`[<-.RAVEVariableCollection` <- function(x, i, ..., value) {
  if(missing(i)) {
    nms <- names(x)
  } else {
    nms <- unique(unlist(c(i, ...)))
  }
  checkmate::assert_names(names(value), type = "named", must.include = nms)
  lapply(nms, function(id) {
    x$set_variable(id = id, value = value[[id]])
    NULL
  })
  x
}

#' @export
`[[.RAVEVariableCollection` <- function(x, i, validate = FALSE, ...) {
  x$get_variable(id = i, ..., validate = validate)
}

#' @export
`[[<-.RAVEVariableCollection` <- function(x, name, value) {
  x$set_variable(id = name, value = value)
  x
}

#' @title Create a collection of constraint variables
#' @param name collection name, default is empty
#' @param explicit whether setting and getting variables should be explicit,
#' default is \code{TRUE}, which means trying to get undefined variables
#' will result in errors
#' @returns A \code{RAVEVariableCollection} instance
#'
#' @examples
#'
#' collection <- new_variable_collection()
#'
#' # Add unconstrained variables
#' collection$add_variable(id = "title", "Voltage traces")
#'
#' # Add a variable with placeholder
#' collection$add_variable(id = "baseline_window")
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
#' collection$use_constraints(function(x) {
#'   missing_values <- vapply(x, inherits, FALSE, "key_missing")
#'   missing_keys <- names(x)[ missing_values ]
#'   if(!length(missing_keys)) { return(TRUE) }
#'   sprintf("Variable [%s] missing", paste(missing_keys, collapse = ","))
#' })
#'
#'
#' # validation will fail
#' collection$validate(on_error = "message")
#'
#' # Fix the issue
#' collection$set_variable("baseline_window", c(-1,0))
#' collection$validate()
#'
#' # Get variable values
#' collection$as_list()
#' collection[]
#'
#' # get one variable
#' collection[["baseline_window"]]
#'
#' # get partial variables
#' collection["baseline_window", "analysis_range"]
#' collection[c("baseline_window", "analysis_range")]
#'
#' \dontrun{
#'
#' # error out when explicit
#' collection[["unregistered_variable"]]
#' collection[["unregistered_variable"]] <- 1
#'
#' }
#'
#' # turn off explicit variable option
#' collection$explicit <- FALSE
#' collection[["unregistered_variable"]]
#' collection[["unregistered_variable"]] <- 1
#'
#' @export
new_variable_collection <- function(name = "", explicit = TRUE) {
  RAVEVariableCollection$new(name = name, explicit = explicit)
}



