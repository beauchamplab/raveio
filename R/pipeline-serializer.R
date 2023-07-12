# pipeline target formats

target_user_path <- function(target_export = "", check = FALSE) {
  user_dir <- file.path(targets::tar_config_get("store"), "user")
  if(check && !dir.exists(user_dir)) {
    dir.create(user_dir, showWarnings = FALSE, recursive = TRUE)
  }
  file.path(user_dir, target_export)
}

target_format <- function(name) {
  if(length(name) != 1) { return(targets::tar_option_get("format")) }
  flist <- get(".target_formats")
  if(flist$`@has`(name)) {
    re <- flist[[name]]
    return(re)
  } else {
    return(name)
  }
}

target_format_dynamic <- function(
    name, target_export = NULL,
    target_expr = NULL, target_depends = NULL) {

  backup_format <- target_format(name)
  if(is.character(backup_format)) { return(backup_format) }

  read <- dipsaus::new_function2(
    args = alist(path = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ns <- asNamespace("raveio")
      ser <- ns$target_format(.(name))
      ser$read(path, target_export = .(target_export),
               target_expr = quote(.(target_expr)),
               target_depends = .(target_depends))
    })
  )
  write <- dipsaus::new_function2(
    args = alist(object =, path = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ns <- asNamespace("raveio")
      ser <- ns$target_format(.(name))
      ser$write(object = object, path = path,
                target_export = .(target_export))
    })
  )

  marshal <- dipsaus::new_function2(
    args = alist(object = ), quote_type = "quote", env = baseenv(),
    body = bquote({
      ns <- asNamespace("raveio")
      ser <- ns$target_format(.(name))
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
      ser <- ns$target_format(.(name))
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
  flist[[name]] <- list(
    read = read,
    write = write,
    marshal = marshal,
    unmarshal = unmarshal
  )
  return(invisible(NULL))
}

target_format_unregister <- function(name) {
  stopifnot(length(name) == 1 && nzchar(name))
  flist <- get(".target_formats")
  if(flist$`@has`(name)) {
    flist$`@remove`(name)
  }
  return(invisible(NULL))
}



tfmtreg_user_defined_python <- function() {
  target_format_register(
    name = "user-defined-python",
    read = function(path, target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {

      raveio <- asNamespace("raveio")
      config <- raveio$load_yaml(file = path)
      if(isTRUE(config$null_value)) {
        return(NULL)
      }

      py_error_handler <- function(e) {
        e2 <- asNamespace("reticulate")$py_last_error()
        if(!is.null(e2)) {
          e <- e2
        }
        stop(sprintf(
          "Unable to load user-defined python object [%s]. \nUnserializer reports this error:\n  %s", target_export, paste(e$message, collapse = "\n")
        ), call. = FALSE)
      }


      tryCatch(
        {
          py_module <- raveio$pipeline_py_module(convert = FALSE, must_work = TRUE)
          unserialize_func <- py_module$rave_pipeline_adapters$rave_unserialize
          if(!inherits(unserialize_func, "python.builtin.function")) {
            stop(sprintf("Unable to find unserialization function for user-defined python objects: %s", paste(target_export, collapse = ",")))
          }
          message("Unserializing [", target_export, "] using Python module [", py_module$`__name__`, "]")
          path2 <- raveio$target_user_path(target_export = target_export, check = TRUE)
          re <- unserialize_func(path2, target_export)
          py <- rpymat::import_main(convert = FALSE)
          py[[ target_export ]] <- re
          return(re)

        },
        python.builtin.BaseException = py_error_handler,
        python.builtin.Exception = py_error_handler,
        py_error = py_error_handler,
        error = function(e) {
          traceback(e)
          stop(e$message, call. = FALSE)
        }
      )

    },
    write = function(object, path, target_export = NULL) {

      raveio <- asNamespace("raveio")

      py_error_handler <- function(e) {
        e2 <- asNamespace("reticulate")$py_last_error()
        if(!is.null(e2)) {
          e <- e2
        }
        stop(sprintf(
          "Unable to save user-defined python object [%s]. \nSerializer reports this error:\n  %s",
          target_export, paste(e$message, collapse = "\n")
        ), call. = FALSE)
      }

      tryCatch(
        {
          info_module <- raveio$pipeline_py_info(must_work = TRUE)
          py_module <- raveio$pipeline_py_module(convert = FALSE, must_work = TRUE)
          serialize_func <- py_module$rave_pipeline_adapters$rave_serialize
          if(!inherits(serialize_func, "python.builtin.function")) {
            stop(sprintf("Unable to find serialization function for user-defined python objects: %s", paste(target_export, collapse = ",")))
          }
          script_signature <- dipsaus::digest(file = file.path(info_module$target_path, sprintf("pipeline_target_%s.py", target_export)))
          message("Serializing [", target_export, "] using Python module [", py_module$`__name__`, "]")
          path2 <- raveio$target_user_path(target_export = target_export, check = TRUE)
          message(path2)
          path3 <- serialize_func(object, normalizePath(path2, mustWork = FALSE),
                                  target_export)
          message(path3)
          if(!is.null(path3) && !inherits(path3, "python.builtin.NoneType")) {
            path3 <- rpymat::py_to_r(path3)
            if(is.character(path3) && length(path3) == 1 &&
               !is.na(path3) && file.exists(path3)) {
              path2 <- path3
            }
          }

          null_value <- FALSE
          if(dir.exists(path2)) {
            fs <- list.files(path2, all.files = FALSE, recursive = TRUE, full.names = TRUE, include.dirs = FALSE, no.. = TRUE)
            data_signature <- lapply(sort(fs), function(f) {
              dipsaus::digest(file = f)
            })
            data_signature <- dipsaus::digest(object = data_signature)
          } else if( file.exists(path2) ){
            data_signature <- dipsaus::digest(file = path2)
          } else {
            null_value <- TRUE
            data_signature <- NULL
          }
          raveio$save_yaml(list(
            null_value = null_value,
            script_signature = script_signature,
            data_signature = data_signature
          ), file = path, sorted = TRUE)
        },
        python.builtin.BaseException = py_error_handler,
        python.builtin.Exception = py_error_handler,
        py_error = py_error_handler,
        error = function(e) {
          traceback(e)
          stop(e$message, call. = FALSE)
        }
      )
      return()
    }
  )
}

tfmtreg_filearray <- function() {
  target_format_register(
    "filearray",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
      target_expr <- substitute(target_expr)
      data <- readRDS(path)
      if(!is.list(data) || !isTRUE(
        data$type %in% c("asis", "array", "filearray")
      )) {
        stop("Cannot restore filearray/array from the saved target file.")
      }

      if(data$type == "asis") {
        return(data$data)
      }
      abspath <- c(
        data$data$abspath,
        file.path(dirname(path), data$data$basename),
        target_user_path(paste0(target_export, ".FILEARRAY.DATA"))
      )
      abspath <- as.character(abspath[!is.na(abspath)])
      abspath <- abspath[dir.exists(abspath)]

      object <- tryCatch({
        if(length(abspath)) {
          abspath <- abspath[[1]]
          object <- filearray::filearray_load(
            filebase = abspath,
            mode = ifelse(identical(data$data$mode, "readwrite"),
                          "readwrite", "readonly")
          )
          if(data$type == "array" && !isTRUE(object$get_header("lazy_load"))) {
            object <- object[drop = isTRUE(data$data$auto_drop)]
          }
        } else {
          stop("Invalid filearray path")
        }
        object
      }, error = function(e) {
        message(sprintf("Cannot restore [%s]... Re-generate the target", target_export))
        # failed to load filearray, retry
        env <- new.env(parent = globalenv())
        lapply(target_depends, function(name) {
          delayedAssign(name, {
            targets::tar_read_raw(name = name)
          }, assign.env = env)
          return()
        })
        eval(target_expr, envir = env)
      })

      return(object)
    },
    write = function(object, path, target_export = NULL) {
      # object = NULL or zero length

      filebase <- normalizePath(
        target_user_path(paste0(target_export, ".FILEARRAY.DATA")),
        mustWork = FALSE
      )
      if(!length(object)) {
        data <- list(
          type = "asis",
          data = object
        )
        saveRDS(data, path)
        return()
      }
      if (inherits(object, "FileArray")) {
        # if(object$get_header("targets_nocopy", FALSE)) {
        #   # the cache is handled by RAVE, do not copy
        # }
        filebase_orig <- normalizePath(object$.filebase, mustWork = FALSE)
        if(filebase_orig != filebase) {
          filebase_dir <- dirname(filebase)
          if(!dir.exists(filebase_dir)) {
            dir.create(filebase_dir, recursive = TRUE,
                       showWarnings = FALSE)
          }
          if(file.exists(filebase)) {
            unlink(filebase, recursive = TRUE)
          }
          file.copy(from = filebase_orig, to = filebase_dir,
                    copy.date = TRUE, recursive = TRUE,
                    overwrite = TRUE)
          orig_name <- basename(filebase_orig)
          file.rename(
            file.path(filebase_dir, orig_name),
            filebase
          )
        }
        data <- list(
          type = "filearray",
          data = list(
            abspath = filebase,
            basename = basename(filebase),
            mode = object$.mode
          )
        )
        saveRDS(data, path)
        return()
      }

      if(!is.array(object) && !is.matrix(object) &&
         !is.vector(object)) {
        stop("To save/load as `filearray`, the object must be zero length, a vector/matrix/array, or a `filearray`")
      }

      mode <- storage.mode(object)
      if(!mode %in% c(
        "integer", "double", "complex", "logical", "raw")) {
        stop("To save/load as `filearray`, the object must be numeric/complex/logical/raw")
      }


      if(is.array(object) || is.matrix(object)) {
        dm <- dim(object)
        auto_drop <- FALSE
      } else {
        dm <- c(length(object), 1L)
        auto_drop <- TRUE
      }
      headers <- attr(object, "filearray_headers")
      headers2 <- NULL
      if(is.list(headers)) {
        nms <- names(headers)
        if(length(nms)) {
          headers2 <- headers[!nms %in% c(
            "", "filebase", "mode", "dimension", "type",
            "initialize", "symlink_ok")]
        }
      }
      signature <- headers2$signature
      if(!length(signature)) {
        signature <- dipsaus::digest(object)
        headers2$signature <- signature
      }
      if(!is.function(headers2$on_missing)) {
        headers2$on_missing <- function(arr) {
          arr[] <- object
          dimnames(arr) <- dimnames(object)
          arr
        }
      }
      args <- c(list(
        filebase = filebase, mode = "readwrite",
        dimension = dm, type = mode,
        initialize = FALSE, symlink_ok = FALSE
      ), headers2)
      do.call(filearray::filearray_load_or_create, args)

      data <- list(
        type = "array",
        data = list(
          abspath = filebase,
          basename = basename(filebase),
          auto_drop = auto_drop,
          mode = "readonly"
        )
      )
      saveRDS(data, path)
    }
  )
}

tfmtreg_rave_subject <- function() {
  target_format_register(
    "rave-subject",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
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
    write = function(object, path, target_export = NULL) {
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


tfmtreg_rave_brain <- function() {
  target_format_register(
    "rave-brain",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
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
    write = function(object, path, target_export = NULL) {

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

tfmtreg_rave_repository <- function() {
  target_format_register(
    "rave-repository",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {

      backup_path <- target_user_path(paste0(target_export, ".RAVE.REPOSITORY"))
      if(length(backup_path) == 1 && !is.na(backup_path) && file.exists(backup_path)) {
        return(readRDS(backup_path))
      }

      data <- readRDS(path)
      if(!is.list(data) || length(data$type) != 1 ||
         !length(data$args) || !is.list(data$args) ) {
        stop("Invalid `rave-repository` file.")
      }
      raveio <- asNamespace("raveio")
      switch(
        data$type,
        "rave_prepare_subject_voltage_with_epoch" = {
          return(do.call(
            raveio$prepare_subject_voltage_with_epoch,
            data$args
          ))
        },
        {
          stop(sprintf("Cannot restore from `rave-repository` file: unsupported format [%s]", paste(data$type, collapse = ", ")))
        }
      )
    },
    write = function(object, path, target_export = NULL) {
      # object <- raveio::prepare_subject_voltage_with_epoch(
      #   subject = "demo/DemoSubject",
      #   electrodes = 14,
      #   time_windows = c(-1,0),
      #   quiet = TRUE
      # )
      if(!inherits(object, "rave_repository")) {
        stop("To save/load as `rave-repository`, the class must contains either `rave_repository`")
      }

      backup_path <- target_user_path(
        paste0(target_export, ".RAVE.REPOSITORY"),
        check = TRUE
      )

      if(inherits(object, "rave_prepare_subject_voltage_with_epoch")) {
        data <- list(
          type = "rave_prepare_subject_voltage_with_epoch",
          args = list(
            subject = object$subject$subject_id,
            electrodes = object$electrode_list,
            time_windows = object$time_windows,
            epoch_name = object$epoch_name,
            reference_name = object$reference_name,
            repository_id = object$repository_id,
            quiet = TRUE
          )
        )
        saveRDS(data, file = path)
        saveRDS(object, file = backup_path)
        return()
      }
      .NotYetImplemented()
    }
  )
}

tfmtreg_rave_prepare_power <- function() {
  target_format_register(
    "rave_prepare_power",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
      ns <- asNamespace("raveio")
      re <- ns$load_yaml(path)
      if(!isTRUE(re$instance_class %in% c("rave_prepare_power"))) {
        stop("Cannot restore RAVE repository (power) as the instance class must be either `rave_prepare_power`")
      }
      repository <- ns$prepare_subject_power(
        subject = re$subject,
        electrodes = re$electrodes,
        epoch_name = re$epoch_name,
        reference_name = re$reference_name,
        time_windows = re$time_window,
        verbose = FALSE
      )
      return(repository)
    },
    write = function(object, path, target_export = NULL) {
      if(!inherits(object, "rave_prepare_power")) {
        stop("The object to save as target is not a valid RAVE repository (power)")
      }

      ns <- asNamespace("raveio")
      ns$save_yaml(list(
        instance_class = "rave_prepare_power",
        signal_data = "power",
        subject = object$subject$subject_id,
        electrodes = dipsaus::deparse_svec(object$electrode_list),
        epoch_name = object$epoch_name,
        reference_name = object$reference_name,
        time_window = object$time_windows
      ), file = path, sorted = TRUE)
    }
  )
}

tfmtreg_rave_prepare_phase <- function() {
  target_format_register(
    "rave_prepare_phase",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
      ns <- asNamespace("raveio")
      re <- ns$load_yaml(path)
      if(!isTRUE(re$instance_class %in% c("rave_prepare_phase"))) {
        stop("Cannot restore RAVE repository (phase) as the instance class must be either `rave_prepare_phase`")
      }
      repository <- ns$prepare_subject_phase(
        subject = re$subject,
        electrodes = re$electrodes,
        epoch_name = re$epoch_name,
        reference_name = re$reference_name,
        time_windows = re$time_window,
        verbose = FALSE
      )
      return(repository)
    },
    write = function(object, path, target_export = NULL) {
      if(!inherits(object, "rave_prepare_phase")) {
        stop("The object to save as target is not a valid RAVE repository (phase)")
      }

      ns <- asNamespace("raveio")
      ns$save_yaml(list(
        instance_class = "rave_prepare_phase",
        signal_data = "phase",
        subject = object$subject$subject_id,
        electrodes = dipsaus::deparse_svec(object$electrode_list),
        epoch_name = object$epoch_name,
        reference_name = object$reference_name,
        time_window = object$time_windows
      ), file = path, sorted = TRUE)
    }
  )
}

tfmtreg_rave_prepare_wavelet <- function() {
  target_format_register(
    "rave_prepare_wavelet",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
      ns <- asNamespace("raveio")
      re <- ns$load_yaml(path)
      if(!isTRUE(re$instance_class %in% c("rave_prepare_wavelet"))) {
        stop("Cannot restore RAVE repository (wavelet coefficients) as the instance class must be either `rave_prepare_wavelet`")
      }
      repository <- ns$prepare_subject_wavelet(
        subject = re$subject,
        electrodes = re$electrodes,
        epoch_name = re$epoch_name,
        reference_name = re$reference_name,
        time_windows = re$time_window,
        verbose = FALSE
      )
      return(repository)
    },
    write = function(object, path, target_export = NULL) {
      if(!inherits(object, "rave_prepare_wavelet")) {
        stop("The object to save as target is not a valid RAVE repository (wavelet coefficients)")
      }

      ns <- asNamespace("raveio")
      ns$save_yaml(list(
        instance_class = "rave_prepare_wavelet",
        signal_data = "wavelet-coefficient",
        subject = object$subject$subject_id,
        electrodes = dipsaus::deparse_svec(object$electrode_list),
        epoch_name = object$epoch_name,
        reference_name = object$reference_name,
        time_window = object$time_windows
      ), file = path, sorted = TRUE)
    }
  )
}


tfmtreg_rave_prepare_subject_voltage_with_epoch <- function() {
  target_format_register(
    "rave_prepare_subject_voltage_with_epoch",
    read = function(path,
                    target_export = NULL,
                    target_expr = NULL,
                    target_depends = NULL) {
      ns <- asNamespace("raveio")
      re <- ns$load_yaml(path)
      if(!isTRUE(re$instance_class %in% c("rave_prepare_subject_voltage_with_epoch"))) {
        stop("Cannot restore RAVE repository (phase) as the instance class must be either `rave_prepare_subject_voltage_with_epoch`")
      }
      repository <- ns$prepare_subject_voltage_with_epoch(
        subject = re$subject,
        electrodes = re$electrodes,
        epoch_name = re$epoch_name,
        reference_name = re$reference_name,
        time_windows = re$time_window,
        verbose = FALSE
      )
      return(repository)
    },
    write = function(object, path, target_export = NULL) {
      if(!inherits(object, "rave_prepare_subject_voltage_with_epoch")) {
        stop("The object to save as target is not a valid RAVE repository (voltage with epoch)")
      }

      ns <- asNamespace("raveio")
      ns$save_yaml(list(
        instance_class = "rave_prepare_subject_voltage_with_epoch",
        signal_data = "voltage",
        subject = object$subject$subject_id,
        electrodes = dipsaus::deparse_svec(object$electrode_list),
        epoch_name = object$epoch_name,
        reference_name = object$reference_name,
        time_window = object$time_windows
      ), file = path, sorted = TRUE)
    }
  )
}

# internally used at on load
target_format_register_onload <- function(verbose = TRUE) {

  on_exception <- function(e) {
    if(verbose) {
      warning(e)
    }
  }

  tryCatch({
    tfmtreg_filearray()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    tfmtreg_rave_subject()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    tfmtreg_rave_repository()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    tfmtreg_rave_prepare_power()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    tfmtreg_rave_prepare_phase()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    tfmtreg_rave_prepare_wavelet()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    tfmtreg_rave_prepare_subject_voltage_with_epoch()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    tfmtreg_rave_brain()
  }, error = on_exception, warning = on_exception)

  tryCatch({
    tfmtreg_user_defined_python()
  }, error = on_exception, warning = on_exception)



}
