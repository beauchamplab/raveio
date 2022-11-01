# pipeline target formats

target_format <- function(name, serialize = TRUE) {
  if(length(name) != 1) { return(targets::tar_option_get("format")) }
  flist <- get(".target_formats")
  if(flist$has(name)) {
    re <- flist$get(name)
    if( serialize ) {
      re <- do.call(targets::tar_format, dipsaus::drop_nulls(re))
    }
    return(re)
  } else {
    return(name)
  }
}

target_format_dynamic <- function(name) {

  backup_format <- target_format(name, serialize = FALSE)
  if(is.character(backup_format)) { return(backup_format) }

  read <- dipsaus::new_function2(
    args = alist(path = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ns <- asNamespace("raveio")
      ser <- ns$target_format(.(name), serialize = FALSE)
      ser$read(path)
    })
  )
  write <- dipsaus::new_function2(
    args = alist(object =, path = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ns <- asNamespace("raveio")
      ser <- ns$target_format(.(name), serialize = FALSE)
      ser$write(object = object, path = path)
    })
  )

  marshal <- dipsaus::new_function2(
    args = alist(object = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ns <- asNamespace("raveio")
      ser <- ns$target_format(.(name), serialize = FALSE)
      if(is.function(ser$marshal)) {
        ser$marshal(object)
      } else {
        object
      }
    })
  )

  unmarshal <- dipsaus::new_function2(
    args = alist(object = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ns <- asNamespace("raveio")
      ser <- ns$target_format(.(name), serialize = FALSE)
      if(is.function(ser$unmarshal)) {
        ser$unmarshal(object)
      } else {
        object
      }
    })
  )

  targets::tar_format(
    read = read,
    write = write,
    marshal = marshal,
    unmarshal = unmarshal
  )

}

target_format_register <- function(name, read, write, marshal = NULL, unmarshal = NULL) {
  stopifnot(length(name) == 1 && nzchar(name))
  flist <- get(".target_formats")
  flist$set(
    key = name,
    value = list(
      read = read,
      write = write,
      marshal = marshal,
      unmarshal = unmarshal
    )
  )
  return(invisible(NULL))
}

target_format_unregister <- function(name) {
  stopifnot(length(name) == 1 && nzchar(name))
  flist <- get(".target_formats")
  if(flist$has(name)) {
    flist$remove(name)
  }
  return(invisible(NULL))
}

target_format_register_rave_subject <- function() {
  target_format_register(
    "rave-subject",
    read = function(path) {
      ns <- asNamespace("raveio")
      re <- ns$load_yaml(path)
      if(!isTRUE(re$instance_class %in% c("RAVESubject", "RAVEPreprocessSettings"))) {
        stop("Cannot restore RAVE subject as the instance class must be either `RAVESubject` or `RAVEPreprocessSettings`")
      }
      subject <- ns$as_rave_subject(re$subject_id, strict = FALSE, reload = TRUE)
      if(re$instance_class == "RAVEPreprocessSettings") {
        subject <- subject$preprocess_settings
      }
      return(subject)
    },
    write = function(object, path) {
      cls <- NULL
      subject_id <- NULL
      if(inherits(object, "RAVESubject")) {
        cls <- "RAVESubject"
        subject_id <- object$subject_id
      } else if(inherits(object, "RAVEPreprocessSettings")) {
        cls <- "RAVEPreprocessSettings"
        subject_id <- object$subject$subject_id
      } else {
        stop("To save/load as `rave-subject`, the class must be either `RAVESubject` or `RAVEPreprocessSettings`")
      }

      ns <- asNamespace("raveio")
      ns$save_yaml(list(
        subject_id = subject_id,
        instance_class = cls
      ), file = path, sorted = TRUE)
    }
  )
}


target_format_register_rave_brain <- function() {
  target_format_register(
    "rave-brain",
    read = function(path) {
      indata <- readRDS(path)
      if(!is.list(indata) || !isTRUE(indata$class %in% c("rave-brain", "multi-rave-brain")) ||
         length(indata$params) == 0) {
        return(NULL)
      }
      raveio <- asNamespace("raveio")

      restore_brain <- function(params) {
        if(inherits(params, "rave-brain")) {
          return(params)
        }
        if(!length(params)) { return(NULL) }
        tryCatch({
          subject <- raveio$RAVESubject$new(project_name = params$project_name,
                                        subject_code = params$subject_code, strict = FALSE)
          brain <- tryCatch({
            raveio$rave_brain(
              subject = subject,
              surfaces = params$surfaces,
              use_141 = params$use_141,
              usetemplateifmissing = params$usetemplateifmissing,
              include_electrodes = FALSE
            )
          }, error = function(e) {
            raveio$rave_brain(
              subject = subject,
              use_141 = params$use_141,
              usetemplateifmissing = params$usetemplateifmissing,
              include_electrodes = FALSE
            )
          })
          if(!inherits(brain, c("rave-brain", "multi-rave-brain"))) {
            warning(sprintf("Cannot import 3D model - [%s]", subject$subject_id))
            return(NULL)
          }
          if(is.data.frame(params$electrode_table)) {
            brain$set_electrodes(params$electrode_table)

            if(is.data.frame(params$electrode_values)) {
              brain$set_electrode_values(params$electrode_values)
            }
          }
          return(brain)
        }, error = function(e) {
          e$message <- sprintf("Cannot import 3D model - [%s]. Reason: %s",
                               paste(params$subject_code, collapse = ""),
                               paste(e$message, collapse = ""))
          warning(e)
          return(NULL)
        })
      }
      if(indata$class == "rave-brain") {
        return(restore_brain(indata$params))
      } else {
        # restore template
        template_params <- indata$params$template_params
        individual_params <- indata$params$individual_params

        # load subjects' brain
        blist <- lapply(individual_params, restore_brain)
        blist <- dipsaus::drop_nulls(blist)
        if(!is.list(blist)) {
          blist <- as.list(blist)
        }

        threeBrain <- asNamespace("threeBrain")
        brain <- tryCatch({
          threeBrain$merge_brain(
            .list = blist,
            template_surface_types = template_params$surfaces,
            template_subject = template_params$subject_code
          )
        }, error = function(e) {
          threeBrain$merge_brain(.list = blist)
        })

        etable <- template_params$electrode_table
        if(length(brain$template_object)) {
          if(is.data.frame(etable) && nrow(etable)) {
            if(length(etable$Subject)) {
              etable$Subject[etable$Subject == template_params$subject_code] <- brain$template_object$subject_code
            }
            brain$template_object$set_electrodes(etable)

            vtable <- template_params$electrode_values
            if(is.data.frame(vtable) && nrow(vtable)) {
              if(length(vtable$Subject)) {
                vtable$Subject[vtable$Subject == template_params$subject_code] <- brain$template_object$subject_code
              }
              brain$template_object$set_electrode_values(vtable)
            }
          }
        }


        brain
      }
    },
    write = function(object, path) {

      if(!inherits(object, c("rave-brain", "multi-rave-brain"))) {
        warning("To save/load as `rave-brain`, the object class must be either `rave-brain` or `multi-rave-brain`")
        saveRDS(object = NULL, file = path, version = 3L)
      }
      get_constructor <- function(brain) {
        if(!inherits(object, c("rave-brain", "multi-rave-brain"))) {
          return(NULL)
        }
        # check construction
        params <- brain$meta$constructor_params
        if(all(c("project_name", "subject_code") %in% names(params)) &&
           length(params$project_name) == 1 &&
           length(params$subject_code) == 1) {
          params$use_141 <- isTRUE(as.logical(params$use_141))
          params$usetemplateifmissing <- isTRUE(as.logical(params$usetemplateifmissing))
          params <- params[c("project_name", "subject_code", "use_141", "usetemplateifmissing")]
          params$surfaces <- names(brain$surfaces)
          params$electrode_table <- brain$electrodes$raw_table
          params$electrode_values <- brain$electrodes$value_table
          return(params)
        } else {
          return(brain)
        }
      }

      cls <- NULL
      if(inherits(object, "rave-brain")) {
        cls <- "rave-brain"
        params <- get_constructor(object)
      } else {
        cls <- "multi-rave-brain"
        params <- list(
          template_params = list(
            subject_code = object$template_subject,
            electrode_table = object$template_object$electrodes$raw_table,
            electrode_values = object$template_object$electrodes$value_table,
            surfaces = object$surface_types
          ),
          individual_params = dipsaus::drop_nulls(lapply(object$objects, get_constructor))
        )
      }

      saveRDS(object = list(class = cls, params = params), file = path, version = 3L)

    }
  )
}


# internally used at onload
target_format_register_onload <- function(verbose = TRUE) {

  on_exception <- function(e) {
    if(verbose) {
      warning(e)
    }
  }

  tryCatch({
    target_format_register_rave_subject()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    target_format_register_rave_brain()
  }, error = on_exception, warning = on_exception)

}
