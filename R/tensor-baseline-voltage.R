
#' @name voltage_baseline
#' @title Calculate voltage baseline
#' @param x R array, \code{\link[filearray]{filearray}}, or
#' \code{'rave_prepare_power'} object created by
#' \code{\link{prepare_subject_raw_voltage_with_epoch}}.
#' @param baseline_windows list of baseline window (intervals)
#' @param method baseline method; choices are \code{'percentage'} and
#' \code{'zscore'}; see 'Details' in \code{\link[dipsaus]{baseline_array}}
#' @param units the unit of the baseline; see 'Details'
#' @param filebase where to store the output; default is \code{NULL} and is
#' automatically determined
#' @param electrodes the electrodes to be included in baseline calculation;
#' for power repository object produced by \code{\link{prepare_subject_power}}
#' only; default is all available electrodes in each of \code{signal_types}
#' @param baseline_mean,baseline_sd internally used by 'RAVE' repository,
#' provided baseline is not contained in the data. This is useful for
#' calculating the baseline with data from other blocks.
#' @param ... passed to other methods
#'
#' @return The same type as the inputs
#'
#' @details The arrays must be three-mode tensor and must have valid named
#' \code{\link{dimnames}}. The dimension names must be \code{'Trial'},
#' \code{'Time'}, \code{'Electrode'}, case sensitive.
#'
#' The \code{baseline_windows} determines the baseline windows that are used to
#' calculate time-points of baseline to be included. This can be one
#' or more intervals and must pass the validation function
#' \code{\link{validate_time_window}}.
#'
#' The \code{units} determines the unit of the baseline. It can be either or
#' both of \code{'Trial'}, \code{'Electrode'}. The default
#' value is both, i.e., baseline for each combination of trial and electrode.
#'
#' @examples
#'
#' \dontrun{
#' # The following code need to download additional demo data
#' # Please see https://rave.wiki/ for more details
#'
#' library(raveio)
#' repo <- prepare_subject_raw_voltage_with_epoch(
#'   subject = "demo/DemoSubject",
#'   time_windows = c(-1, 3),
#'   electrodes = c(14, 15))
#'
#' ##### Direct baseline on repository
#' voltage_baseline(
#'   x = repo, method = "zscore",
#'   baseline_windows = list(c(-1, 0), c(2, 3))
#' )
#'
#' voltage_mean <- repo$raw_voltage$baselined$collapse(
#'   keep = c(1,3), method = "mean")
#' matplot(voltage_mean, type = "l", lty = 1,
#'         x = repo$raw_voltage$dimnames$Time,
#'         xlab = "Time (s)", ylab = "Voltage (z-scored)",
#'         main = "Mean coltage over trial (Baseline: -1~0 & 2~3)")
#' abline(v = 0, lty = 2, col = 'darkgreen')
#' text(x = 0, y = -0.5, "Aud-Onset ", col = "darkgreen", cex = 0.6, adj = c(1,1))
#'
#' ##### Alternatively, baseline on each electrode channel
#' voltage_mean2 <- sapply(repo$raw_voltage$data_list, function(inst) {
#'   re <- voltage_baseline(
#'     x = inst, method = "zscore",
#'     baseline_windows = list(c(-1, 0), c(2, 3)))
#'   rowMeans(re[])
#' })
#'
#' # Same with floating difference
#' max(abs(voltage_mean - voltage_mean2)) < 1e-8
#'
#'
#' }
#'
#' @export
voltage_baseline <- function(
    x, baseline_windows,
    method = c("percentage", "zscore", "subtract_mean"),
    units = c("Trial", "Electrode"), ...
){
  UseMethod("voltage_baseline")
}


#' @rdname voltage_baseline
#' @export
voltage_baseline.rave_prepare_subject_raw_voltage_with_epoch <- function(
    x, baseline_windows,
    method = c("percentage", "zscore", "subtract_mean"),
    units = c("Trial", "Electrode"),
    electrodes, baseline_mean, baseline_sd, ...
){
  method <- match.arg(method)

  use_raw <- inherits(x, "rave_prepare_subject_raw_voltage_with_epoch")

  if(missing(electrodes)){
    electrodes <- x$electrode_list
  } else {
    electrodes <- electrodes[electrodes %in% x$electrode_list]
    if(!length(electrodes)) {
      stop("`voltage_baseline`: none of electrodes specified can be found in the loaded repository")
    }
  }

  baseline_provided <- !missing(baseline_mean)
  if(baseline_provided) {
    force(baseline_mean)
    if(length(baseline_mean) != length(x$electrode_list)) {
      stop("`voltage_baseline`: the provided `baseline_mean` must have the same length as the number of electrodes in the repository (whether baseline is requested or not).")
    }
    if( method == "zscore" ) {
      force(baseline_sd)
      if(length(baseline_sd) != length(x$electrode_list)) {
        stop("`voltage_baseline`: the provided `baseline_sd` must have the same length as the number of electrodes in the repository.")
      }
    } else {
      baseline_sd <- rep(1, length(electrodes))
    }
    baseline_mean <- as.double(baseline_mean)
    baseline_sd <- as.double(baseline_sd)
    baseline_windows <- NULL
  } else {
    baseline_mean <- NULL
    baseline_sd <- NULL
    baseline_windows <- validate_time_window(baseline_windows)
  }

  # Prepare global variables
  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% c("Trial", "Electrode"))){
    stop('`units` must contain 1-2 of the followings: "Trial", "Electrode" (case-sensitive)')
  }
  unit_dims <- c(2L, 3L)[c("Trial", "Electrode") %in% units]

  sel <- x$electrode_list %in% electrodes

  sub_elec <- x$electrode_list[sel]
  if( use_raw ) {
    sub_list <- x$raw_voltage$data_list[sel]
    dnames <- x$raw_voltage$dimnames
    input_signature <- x$raw_voltage$signature
    tdim <- x$raw_voltage$dim
  } else {
    sub_list <- x$voltage$data_list[sel]
    dnames <- x$voltage$dimnames
    input_signature <- x$voltage$signature
    tdim <- x$voltage$dim
  }

  dnames$Time <- as.numeric(dnames$Time)
  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(dnames$Time >= w[[1]] & dnames$Time <= w[[2]])
  })))

  # calculate signature

  digest_key <- list(
    input_signature = input_signature,
    # signal_type = signal_type,
    rave_data_type = ifelse(use_raw, "raw-voltage", "voltage"),
    method = method,
    unit_dims = unit_dims,
    time_index = dipsaus::deparse_svec(time_index),
    dimension = tdim,
    baseline_provided = baseline_provided,
    baseline_mean = baseline_mean,
    baseline_sd = baseline_sd
  )

  signature <- dipsaus::digest(digest_key)

  filebase <- file.path(cache_root(), "_baselined_arrays_", input_signature)
  res <- tryCatch({
    res <- filearray::filearray_checkload(
      filebase, mode = "readwrite", symlink_ok = FALSE,
      rave_signature = signature,
      # signal_type = signal_type,
      rave_data_type = ifelse(use_raw, "raw-voltage-baselined", "voltage-baselined"),
      baseline_provided = baseline_provided,
      ready = TRUE,  # The rest procedure might go wrong, in case failure
      RAVEIO_FILEARRAY_VERSION = RAVEIO_FILEARRAY_VERSION
    )
    catgl("Using existing cache", level = "DEBUG")
    res
  }, error = function(e){
    # message(e$message)
    if(dir.exists(filebase)){ unlink(filebase, recursive = TRUE, force = TRUE) }
    dir_create2(dirname(filebase))
    res <- filearray::filearray_create(
      filebase = filebase,
      dimension = tdim,
      type = "float",
      partition_size = 1
    )
    res$.mode <- "readwrite"
    res$.header$rave_signature <- signature
    # res$.header$signal_type <- signal_type
    res$.header$rave_data_type <- ifelse(use_raw, "raw-voltage-baselined", "voltage-baselined")
    res$.header$baseline_method <- method
    res$.header$unit_dims <- unit_dims
    res$.header$time_index <- dipsaus::deparse_svec(time_index)
    res$.header$baseline_windows <- baseline_windows
    res$.header$RAVEIO_FILEARRAY_VERSION <- RAVEIO_FILEARRAY_VERSION
    res$.header$ready <- FALSE
    res$.header$baseline_provided <- baseline_provided
    res$.header$baseline_mean <- baseline_mean
    res$.header$baseline_sd <- baseline_sd
    dimnames(res) <- dnames
    # # automatically run
    # res$.save_header()
    res
  })

  if("Electrode" %in% units){
    # Check electrode with baselines
    todo_elec <- sub_elec[!sub_elec %in% res$.header$electrodes]

    if(length(todo_elec)) {

      res$set_header("ready", FALSE)

      input_list <- lapply(todo_elec, function(e){
        idx <- which(x$electrode_list == e)
        if( use_raw ) {
          data_list <- x$raw_voltage$data_list
        } else {
          data_list <- x$voltage$data_list
        }

        if(baseline_provided) {
          mean <- baseline_mean[idx]
          sd <- baseline_sd[idx]
          return(list(
            index = idx, electrode = e, mean = mean, sd = sd,
            array = data_list[[idx]]
          ))
        } else {
          return(list(
            index = idx, electrode = e,
            array = data_list[[idx]]
          ))
        }
      })

      dipsaus::lapply_async2(
        input_list,
        FUN = function(el) {
          input <- el$array[drop = FALSE]
          if( baseline_provided ) {
            switch(
              method,
              "percentage" = {
                res[, , el$index] <- input / (el$mean / 100) - 100
              },
              "zscore" = {
                res[, , el$index] <- (input - el$mean) / el$sd
              },
              "subtract_mean" = {
                res[, , el$index] <- (input - el$mean)
              },
              {
                stop("Unsupported voltage baseline method.")
              }
            )
          } else {
            res[, , el$index] <- baseline_array(
              x = input,
              along_dim = 1L,
              baseline_indexpoints = time_index,
              unit_dims = unit_dims,
              method = method
            )
          }
          NULL
        },
        plan = FALSE,
        callback = function(el) {
          sprintf("Baseline correction | %s", el$electrode)
        }
      )

      res$.header$electrodes <- unique(c(
        res$.header$electrodes,
        todo_elec
      ))

      res$set_header("ready", TRUE)
    }


    res$.mode <- "readonly"

  } else {

    stop("Baseline across electrode not supported")

  }

  if( use_raw ) {
    x$raw_voltage$baselined <- res
  } else {
    x$voltage$baselined <- res
  }

  return(x)
}

#' @rdname voltage_baseline
#' @export
voltage_baseline.rave_prepare_subject_voltage_with_epoch <- voltage_baseline.rave_prepare_subject_raw_voltage_with_epoch


#' @rdname voltage_baseline
#' @export
voltage_baseline.FileArray <- function(
    x, baseline_windows,
    method = c("percentage", "zscore", "subtract_mean"),
    units = c("Trial", "Electrode"),
    filebase = NULL, ...
){
  method <- match.arg(method)
  baseline_windows <- validate_time_window(baseline_windows)
  dnames <- dimnames(x)
  dm <- dim(x)
  dnn <- c("Time", "Trial", "Electrode")
  if(!identical(names(dnames), dnn)){
    stop('The dimension names are inconsistent, should be c("Time", "Trial", "Electrode")')
  }
  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% dnn)){
    stop('`units` must contain 1-2 of the followings: "Trial", "Electrode" (case-sensitive)')
  }
  dnames$Time <- as.numeric(dnames$Time)

  unit_dims <- c(2L, 3L)[c("Trial", "Electrode") %in% units]
  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(dnames$Time >= w[[1]] & dnames$Time <= w[[2]])
  })))

  # calculate signatures
  signal_type <- x$get_header("signal_type")
  rave_data_type <- x$get_header("rave_data_type", default = "voltage")
  digest_key <- list(
    input_signature = x$get_header("rave_signature"),
    signal_type = signal_type,
    rave_data_type = rave_data_type,
    method = method,
    unit_dims = unit_dims,
    time_index = time_index,
    dimension = dm,
    x_header = x$.header
  )
  signature <- dipsaus::digest(digest_key)

  if(!length(filebase)){
    filebase <- file.path(cache_root(), "_baselined_arrays_", signature)
  }

  dir_create2(dirname(filebase))

  res <- filearray::filearray_load_or_create(
    filebase = filebase, mode = "readwrite", symlink_ok = FALSE,
    dimension = as.integer(dm), type = "float", partition_size = 1L,
    rave_signature = signature, signal_type = signal_type,
    rave_data_type = sprintf("%s-baselined", rave_data_type), baseline_method = method,
    unit_dims = unit_dims, time_index = time_index,
    baseline_windows = baseline_windows,
    RAVEIO_FILEARRAY_VERSION = RAVEIO_FILEARRAY_VERSION,
    on_missing = function(res) {
      dimnames(res) <- dnames
    }
  )
  if(!res$get_header("valid", FALSE)) {

    if("Electrode" %in% units){
      # Baseline per electrodes
      dipsaus::lapply_async2(seq_len(dm[[length(dm)]]), function(ii){
        res[, , ii] <-
          baseline_array(
            x[, , ii, drop = FALSE],
            along_dim = 1L,
            baseline_indexpoints = time_index,
            unit_dims = unit_dims,
            method = method
          )
        NULL
      }, plan = FALSE)

    } else {

      output <- baseline_array(x[drop = FALSE],
                                        along_dim = 1L,
                                        baseline_indexpoints = time_index,
                                        unit_dims = unit_dims,
                                        method = method)
      res[] <- output

    }


    res$set_header("ready", TRUE)
  }
  res$.mode <- "readonly"


  res

}


#' @rdname voltage_baseline
#' @export
voltage_baseline.array <- function(
    x, baseline_windows,
    method = c("percentage", "zscore", "subtract_mean"),
    units = c("Trial", "Electrode"), ...
){
  method <- match.arg(method)
  baseline_windows <- validate_time_window(baseline_windows)
  dm <- dim(x)
  dnames <- dimnames(x)
  dnn <- names(dnames)
  stopifnot2(all(dnn %in% c("Time", "Trial", "Electrode")) && length(dm) == 3,
             msg = 'The dimension names are inconsistent, must contain 3 modes: "Time", "Trial", "Electrode"')

  dnames$Time <- as.numeric(dnames$Time)
  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(dnames$Time >= w[[1]] & dnames$Time <= w[[2]])
  })))
  time_margin <- which(dnn == "Time")

  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% dnn)){
    stop('`units` must contain 1-2 of the followings: "Trial", "Electrode" (case-sensitive)')
  }
  unit_dims <- which(dnn %in% units)

  baseline_array(x, along_dim = time_margin, baseline_indexpoints = time_index, unit_dims = unit_dims, method = method)

}

