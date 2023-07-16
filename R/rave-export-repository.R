#' @name rave_export
#' @title Export 'RAVE' data
#' @description Export portable data for custom analyses.
#' @param x R object or 'RAVE' repositories
#' @param path path to save to
#' @param format export format
#' @param zip whether to zip the files
#' @param ... passed to other methods
#' @returns Exported data path
#' @examples
#'
#' x <- "my data"
#' path <- tempfile()
#' rave_export(x, path)
#'
#' readRDS(path)
#'
#' \dontrun{
#'   # Needs demo subject
#'   path <- tempfile()
#'   x <- prepare_subject_power("demo/DemoSubject")
#'
#'   # Export power data to path
#'   rave_export(x, path)
#' }
#'
#' @export
rave_export <- function(x, path, ...) {
  UseMethod("rave_export")
}

#' @rdname rave_export
#' @export
rave_export.default <- function(x, path, format = c("rds", "yaml", "json"), ...) {
  format <- match.arg(format)
  switch(
    format,
    "yaml" = {
      save_yaml(x, path, sorted = TRUE)
    },
    "json" = {
      save_json(x, path, serialize = TRUE)
    }, {
      saveRDS(x, file = path)
    }
  )
  return(normalizePath(path))
}


export_summary <- function(x, root_path, data_type, rave_data_type) {
  h5_path <- file.path(root_path, "summary.h5")
  summary_data <- list()
  summary_data[["loaded_electrodes"]] <- as.integer(x$electrode_list)
  save_h5(x = summary_data[["loaded_electrodes"]], file = h5_path,
                  name = "loaded_electrodes", ctype = "integer",
                  replace = TRUE, quiet = TRUE)

  epoch_name <- x$epoch_name
  if(length(epoch_name) && is.character(epoch_name)) {
    summary_data[["epoch_name"]] <- epoch_name
    save_h5(x = summary_data[["epoch_name"]],
                    file = h5_path, name = "epoch_name", ctype = "character",
                    replace = TRUE, quiet = TRUE)
  }

  reference_name <- x$reference_name
  if(length(reference_name) && is.character(reference_name)) {
    summary_data[["reference_name"]] <- reference_name
    save_h5(x = summary_data[["reference_name"]],
                    file = h5_path, name = "reference_name", ctype = "character",
                    replace = TRUE, quiet = TRUE)
  }

  time_windows <- as.double(unlist(x$time_windows))
  if(length(time_windows)) {
    summary_data[["time_windows"]] <- time_windows
    save_h5(x = summary_data[["time_windows"]], file = h5_path,
                    name = "time_windows", replace = TRUE,
                    quiet = TRUE, ctype = "numeric")
  }

  if(length(epoch_name)) {
    try({
      condition <- x$epoch$table$Condition
      if(length(condition)) {
        summary_data[["condition"]] <- condition
        save_h5(x = summary_data[["condition"]], file = h5_path,
                        name = "condition", replace = TRUE, ctype = "character",
                        quiet = TRUE)
      }
    }, silent = TRUE)
  }


  summary_data[["data_type"]] <- data_type
  save_h5(x = data_type, file = h5_path,
                  name = "data_type", replace = TRUE, ctype = "character",
                  quiet = TRUE)

  summary_data[["rave_data_type"]] <- rave_data_type
  save_h5(x = rave_data_type, file = h5_path,
                  name = "rave_data_type", replace = TRUE, ctype = "character",
                  quiet = TRUE)

  etable <- x$electrode_table
  if(is.data.frame(etable)) {
    for(nm in names(etable)) {
      dat <- etable[[nm]]
      if(is.factor(dat) || is.character(dat)) {
        dat <- as.character(dat)
        dat[is.na(dat)] <- ""
      } else {
        dat <- as.double(dat)
      }
      summary_data[[sprintf("electrode_table_%s", nm)]] <- dat
      save_h5(x = dat, file = h5_path,
                      name = sprintf("electrode_table/%s", nm),
                      replace = TRUE, quiet = TRUE)
    }
  }

  summary_data$con <- normalizePath(file.path(root_path, "summary.mat"),
                                    mustWork = FALSE)
  do.call(R.matlab::writeMat, summary_data)

  return(normalizePath(root_path))
}

#' @rdname rave_export
#' @export
rave_export.rave_prepare_subject_raw_voltage_with_epoch <- function(x, path, zip = FALSE, ...) {
  if(missing(path)) {
    path <- file.path(x$subject$rave_path, "exports",
                      "rave-repository", "raw-voltage-epoched")
  }
  root_path <- file.path(path, strftime(Sys.time(), "export-raw_voltage-%y%m%d-%H%M%S"))
  data_path <- file.path(root_path, "data")
  dir_create2(data_path, check = TRUE)

  # Load sample data to get shape of the array
  e <- x$electrode_instances[[1]]
  sample_data <- e$load_data('raw-voltage')
  array_dim <- dim(sample_data)[c(1,2)]
  nelectrodes <- length(x$electrode_instances)
  catgl("There are {nelectrodes} electrodes", level = "info")
  catgl("Each electrode contains array data of dimension {paste(array_dim, collapse = 'x')}", level = "info")
  # Load data

  lapply_async(
    x$electrode_instances,
    function(e) {
      h5_path <- file.path(data_path, sprintf("%s.h5", e$number))
      export_data <- e$load_data('raw-voltage')
      export_data <- export_data[reshape = array_dim, dimnames = FALSE]
      save_h5(export_data, file = h5_path, name = "/raw_voltage", replace = TRUE, ctype = "numeric", quiet = TRUE)
    },
    callback = function(e) {
      sprintf("Exporting raw-voltage|Electrode %s", e$number)
    }
  )

  root_path <- export_summary(
    x, root_path = root_path, data_type = "raw_voltage",
    rave_data_type = "rave_prepare_subject_raw_voltage_with_epoch"
  )

  if(zip) {
    wd <- getwd()
    on.exit({ setwd(wd) }, add = TRUE, after = TRUE)
    setwd(dirname(root_path))
    fname <- basename(root_path)
    utils::zip(zipfile = sprintf("%s.zip", fname), files = fname)
    zip <- TRUE
  } else {
    zip <- FALSE
  }

  message("Done. Please check ", root_path)
  return(invisible(structure(root_path, zip = zip)))
}


#' @rdname rave_export
#' @export
rave_export.rave_prepare_subject_voltage_with_epoch <- function(x, path, zip = FALSE, ...) {
  if(missing(path)) {
    path <- file.path(x$subject$rave_path, "exports",
                      "rave-repository", "voltage-epoched")
  }
  root_path <- file.path(path, strftime(Sys.time(), "export-voltage-%y%m%d-%H%M%S"))
  data_path <- file.path(root_path, "data")
  dir_create2(data_path, check = TRUE)

  # Load sample data to get shape of the array
  e <- x$electrode_instances[[1]]
  sample_data <- e$load_data('voltage')
  array_dim <- dim(sample_data)[c(1,2)]
  nelectrodes <- length(x$electrode_instances)
  catgl("There are {nelectrodes} electrodes", level = "info")
  catgl("Each electrode contains array data of dimension {paste(array_dim, collapse = 'x')}", level = "info")
  # Load data

  lapply_async(
    x$electrode_instances,
    function(e) {
      h5_path <- file.path(data_path, sprintf("%s.h5", e$number))
      export_data <- e$load_data('voltage')
      export_data <- export_data[reshape = array_dim, dimnames = FALSE]
      save_h5(export_data, file = h5_path, name = "/voltage", replace = TRUE, ctype = "numeric", quiet = TRUE)
    },
    callback = function(e) {
      sprintf("Exporting voltage|Electrode %s", e$number)
    }
  )

  root_path <- export_summary(
    x, root_path = root_path, data_type = "voltage",
    rave_data_type = "rave_prepare_subject_voltage_with_epoch"
  )

  if(zip) {
    wd <- getwd()
    on.exit({ setwd(wd) }, add = TRUE, after = TRUE)
    setwd(dirname(root_path))
    fname <- basename(root_path)
    utils::zip(zipfile = sprintf("%s.zip", fname), files = fname)
    zip <- TRUE
  } else {
    zip <- FALSE
  }

  message("Done. Please check ", root_path)
  return(invisible(structure(root_path, zip = zip)))
}

#' @rdname rave_export
#' @export
rave_export.rave_prepare_power <- function(x, path, zip = FALSE, ...) {
  if(missing(path)) {
    path <- file.path(x$subject$rave_path, "exports",
                      "rave-repository", "power-epoched")
  }
  root_path <- file.path(path, strftime(Sys.time(), "export-voltage-%y%m%d-%H%M%S"))
  data_path <- file.path(root_path, "data")
  dir_create2(data_path, check = TRUE)

  # Load sample data to get shape of the array
  e <- x$electrode_instances[[1]]
  sample_data <- e$load_data('power')
  array_dim <- dim(sample_data)[c(1,2,3)]
  nelectrodes <- length(x$electrode_instances)
  catgl("There are {nelectrodes} electrodes", level = "info")
  catgl("Each electrode contains array data of dimension {paste(array_dim, collapse = 'x')}", level = "info")
  # Load data

  lapply_async(
    x$electrode_instances,
    function(e) {
      h5_path <- file.path(data_path, sprintf("%s.h5", e$number))
      export_data <- e$load_data('power')
      export_data <- export_data[reshape = array_dim, dimnames = FALSE]
      save_h5(export_data, file = h5_path, name = "/power", replace = TRUE, ctype = "numeric", quiet = TRUE)
    },
    callback = function(e) {
      sprintf("Exporting power|Electrode %s", e$number)
    }
  )

  root_path <- export_summary(
    x, root_path = root_path, data_type = "power",
    rave_data_type = "rave_prepare_power"
  )
  if(zip) {
    wd <- getwd()
    on.exit({ setwd(wd) }, add = TRUE, after = TRUE)
    setwd(dirname(root_path))
    fname <- basename(root_path)
    utils::zip(zipfile = sprintf("%s.zip", fname), files = fname)
    zip <- TRUE
  } else {
    zip <- FALSE
  }

  message("Done. Please check ", root_path)
  return(invisible(structure(root_path, zip = zip)))
}
