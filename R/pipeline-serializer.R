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

}
