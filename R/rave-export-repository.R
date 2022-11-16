#' @name rave_export
#' @title Export 'RAVE' data
#' @description Export portable data for custom analyses.
#' @param x R object or 'RAVE' repositories
#' @param path path to save to
#' @param format export format
#' @param zip whether to zip the files
#' @param ... passed to other methods
#' @return Exported data path
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

  dipsaus::lapply_async2(
    x$electrode_instances,
    function(e) {
      h5_path <- file.path(data_path, sprintf("%s.h5", e$number))
      export_data <- e$load_data('raw-voltage')
      export_data <- export_data[reshape = array_dim, dimnames = FALSE]
      save_h5(export_data, file = h5_path, name = "/raw_voltage", replace = TRUE, ctype = "numeric", quiet = TRUE)
    },
    plan = FALSE,
    callback = function(e) {
      sprintf("Exporting raw-voltage|Electrode %s", e$number)
    }
  )
  h5_path <- file.path(root_path, "summary.h5")
  raveio::save_h5(x = as.integer(x$electrode_list), file = h5_path,
                  name = "loaded_electrodes", ctype = "integer",
                  replace = TRUE, quiet = TRUE)
  raveio::save_h5(x = x$epoch_name, file = h5_path, name = "epoch_name",
                  replace = TRUE, quiet = TRUE)
  raveio::save_h5(x = as.double(unlist(x$time_windows)), file = h5_path,
                  name = "time_windows", replace = TRUE,
                  quiet = TRUE, ctype = "numeric")
  raveio::save_h5(x = x$epoch$table$Condition, file = h5_path,
                  name = "condition", replace = TRUE, ctype = "character",
                  quiet = TRUE)
  raveio::save_h5(x = "raw_voltage", file = h5_path,
                  name = "data_type", replace = TRUE, ctype = "character",
                  quiet = TRUE)
  raveio::save_h5(x = "rave_prepare_subject_raw_voltage_with_epoch", file = h5_path,
                  name = "rave_data_type", replace = TRUE, ctype = "character",
                  quiet = TRUE)

  etable <- x$electrode_table
  for(nm in names(etable)) {
    dat <- etable[[nm]]
    if(is.factor(dat) || is.character(dat)) {
      dat <- as.character(dat)
      dat[is.na(dat)] <- ""
    } else {
      dat <- as.double(dat)
    }
    raveio::save_h5(x = dat, file = h5_path,
                    name = sprintf("electrode_table/%s", nm),
                    replace = TRUE, quiet = TRUE)
  }
  # h5read('output.h5', '/electrode_table/freesurferlabel')

  root_path <- normalizePath(root_path)
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

  dipsaus::lapply_async2(
    x$electrode_instances,
    function(e) {
      h5_path <- file.path(data_path, sprintf("%s.h5", e$number))
      export_data <- e$load_data('voltage')
      export_data <- export_data[reshape = array_dim, dimnames = FALSE]
      save_h5(export_data, file = h5_path, name = "/voltage", replace = TRUE, ctype = "numeric", quiet = TRUE)
    },
    plan = FALSE,
    callback = function(e) {
      sprintf("Exporting voltage|Electrode %s", e$number)
    }
  )
  h5_path <- file.path(root_path, "summary.h5")
  raveio::save_h5(x = as.integer(x$electrode_list), file = h5_path,
                  name = "loaded_electrodes", ctype = "integer",
                  replace = TRUE, quiet = TRUE)
  raveio::save_h5(x = x$epoch_name, file = h5_path, name = "epoch_name",
                  replace = TRUE, quiet = TRUE, ctype = "character")
  raveio::save_h5(x = x$reference_name, file = h5_path, name = "reference_name",
                  replace = TRUE, quiet = TRUE, ctype = "character")
  raveio::save_h5(x = as.double(unlist(x$time_windows)), file = h5_path,
                  name = "time_windows", replace = TRUE,
                  quiet = TRUE, ctype = "numeric")
  raveio::save_h5(x = x$epoch$table$Condition, file = h5_path,
                  name = "condition", replace = TRUE, ctype = "character",
                  quiet = TRUE)
  raveio::save_h5(x = "voltage", file = h5_path,
                  name = "data_type", replace = TRUE, ctype = "character",
                  quiet = TRUE)
  raveio::save_h5(x = "rave_prepare_subject_voltage_with_epoch", file = h5_path,
                  name = "rave_data_type", replace = TRUE, ctype = "character",
                  quiet = TRUE)

  etable <- x$electrode_table
  for(nm in names(etable)) {
    dat <- etable[[nm]]
    if(is.factor(dat) || is.character(dat)) {
      dat <- as.character(dat)
      dat[is.na(dat)] <- ""
    } else {
      dat <- as.double(dat)
    }
    raveio::save_h5(x = dat, file = h5_path,
                    name = sprintf("electrode_table/%s", nm),
                    replace = TRUE, quiet = TRUE)
  }
  # h5read('output.h5', '/electrode_table/freesurferlabel')

  root_path <- normalizePath(root_path)
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

  dipsaus::lapply_async2(
    x$electrode_instances,
    function(e) {
      h5_path <- file.path(data_path, sprintf("%s.h5", e$number))
      export_data <- e$load_data('power')
      export_data <- export_data[reshape = array_dim, dimnames = FALSE]
      save_h5(export_data, file = h5_path, name = "/power", replace = TRUE, ctype = "numeric", quiet = TRUE)
    },
    plan = FALSE,
    callback = function(e) {
      sprintf("Exporting power|Electrode %s", e$number)
    }
  )
  h5_path <- file.path(root_path, "summary.h5")
  raveio::save_h5(x = as.integer(x$electrode_list), file = h5_path,
                  name = "loaded_electrodes", ctype = "integer",
                  replace = TRUE, quiet = TRUE)
  raveio::save_h5(x = x$epoch_name, file = h5_path, name = "epoch_name",
                  replace = TRUE, quiet = TRUE, ctype = "character")
  raveio::save_h5(x = x$reference_name, file = h5_path, name = "reference_name",
                  replace = TRUE, quiet = TRUE, ctype = "character")
  raveio::save_h5(x = as.double(unlist(x$time_windows)), file = h5_path,
                  name = "time_windows", replace = TRUE,
                  quiet = TRUE, ctype = "numeric")
  raveio::save_h5(x = x$epoch$table$Condition, file = h5_path,
                  name = "condition", replace = TRUE, ctype = "character",
                  quiet = TRUE)
  raveio::save_h5(x = "power", file = h5_path,
                  name = "data_type", replace = TRUE, ctype = "character",
                  quiet = TRUE)
  raveio::save_h5(x = "rave_prepare_power", file = h5_path,
                  name = "rave_data_type", replace = TRUE, ctype = "character",
                  quiet = TRUE)

  etable <- x$electrode_table
  for(nm in names(etable)) {
    dat <- etable[[nm]]
    if(is.factor(dat) || is.character(dat)) {
      dat <- as.character(dat)
      dat[is.na(dat)] <- ""
    } else {
      dat <- as.double(dat)
    }
    raveio::save_h5(x = dat, file = h5_path,
                    name = sprintf("electrode_table/%s", nm),
                    replace = TRUE, quiet = TRUE)
  }
  # h5read('output.h5', '/electrode_table/freesurferlabel')

  root_path <- normalizePath(root_path)
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
