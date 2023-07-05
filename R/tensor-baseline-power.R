
#' @title Collapse high-dimensional tensor array
#' @param x R array, \code{\link[filearray]{FileArray-class}}, or
#' \code{\link{Tensor}} object
#' @param keep integer vector, the margins to keep
#' @param method character, calculates mean or sum of the array when collapsing
#' @param ... passed to other methods
#' @returns A collapsed array (or a vector or matrix), depending on \code{keep}
#' @seealso \code{\link[dipsaus]{collapse}}
#' @examples
#'
#' x <- array(1:16, rep(2, 4))
#'
#' collapse2(x, c(3, 2))
#'
#' # Alternative method, but slower when `x` is a large array
#' apply(x, c(3, 2), mean)
#'
#' # filearray
#' y <- filearray::as_filearray(x)
#'
#' collapse2(y, c(3, 2))
#'
#' collapse2(y, c(3, 2), "sum")
#'
#' # clean up
#' y$delete(force = TRUE)
#'
#' @export
collapse2 <- function(x, keep, method = c("mean", "sum"), ...){
  UseMethod("collapse2")
}

#' @rdname collapse2
#' @export
collapse2.FileArray <- function(x, keep, method = c("mean", "sum"), ...){
  method <- match.arg(method)
  dm <- dim(x)
  ndims <- length(dm)
  stopifnot(all(keep %in% seq_len(ndims)))
  if(setequal(keep, seq_len(ndims))){
    return(aperm(x[drop=FALSE], keep))
  }
  pdim <- dm
  pdim[[ndims]] <- 1
  is_mean <- method == "mean"

  if(ndims %in% keep){
    pdim <- pdim[-ndims]
    lidx <- which(keep == ndims)[[1]]
    keep_alt <- keep[-lidx]
    re <- filearray::fmap2(list(x), fun = function(v, ...){
      v <- array(v[[1]], dim = pdim)
      collapse(v, keep_alt, average = is_mean)
    }, .input_size = prod(pdim), .buffer_count = dm[[ndims]], .simplify = TRUE)
    redim <- dim(re)
    rendim <- length(redim)
    if(rendim > 1 && rendim != lidx){
      if(lidx == 1){
        od <- c(rendim, seq_along(keep_alt))
      } else {
        od <- seq_along(keep_alt)
        od <- c(od[seq_len(lidx - 1)], rendim, od[-seq_len(lidx - 1)])
      }
      re <- aperm(re, od)
    }
  } else {
    re <- filearray::fmap2(list(x), fun = function(v, ...){
      v <- array(v[[1]], dim = pdim)
      collapse(v, keep, average = is_mean)
    }, .input_size = prod(pdim), .buffer_count = dm[[ndims]], .simplify = TRUE)
    re <- collapse(re, seq_along(keep), average = is_mean)
  }

  dnames <- dimnames(x)
  if(length(keep) > 1){
    dim(re) <- dm[keep]
    if(length(dnames) == ndims){
      dimnames(re) <- dnames[keep]
    }
  } else if(length(dnames) == ndims){
    names(re) <- dnames[[keep]]
  }

  re
}

#' @rdname collapse2
#' @export
collapse2.Tensor <- function(x, keep, method = c("mean", "sum"), ...){
  method <- match.arg(method)
  x$collapse(keep = keep, method = method)
}

#' @rdname collapse2
#' @export
collapse2.array <- function(x, keep, method = c("mean", "sum"), ...){
  method <- match.arg(method)
  ndims <- length(dim(x))
  keep <- as.integer(keep)
  stopifnot(all(keep %in% seq_len(ndims)))
  if(setequal(keep, seq_len(ndims))){
    return(aperm(x, keep))
  }
  collapse(x, keep, average = method == "mean")
}

#' @name power_baseline
#' @title Calculate power baseline
#' @param x R array, \code{\link[filearray]{filearray}},
#' \code{\link{ECoGTensor}}, or \code{'rave_prepare_power'} object created by
#' \code{\link{prepare_subject_power}}.
#' @param baseline_windows list of baseline window (intervals)
#' @param method baseline method; choices are \code{'percentage'},
#' \code{'sqrt_percentage'}, \code{'decibel'}, \code{'zscore'},
#' \code{'sqrt_zscore'}; see 'Details' in \code{\link[dipsaus]{baseline_array}}
#' @param units the unit of the baseline; see 'Details'
#' @param filebase where to store the output; default is \code{NULL} and is
#' automatically determined
#' @param hybrid whether the array will be
#' @param electrodes the electrodes to be included in baseline calculation;
#' for power repository object produced by \code{\link{prepare_subject_power}}
#' only; default is all available electrodes in each of \code{signal_types}
#' @param ... passed to other methods
#'
#' @returns Usually the same type as the input: for arrays,
#' \code{\link[filearray]{filearray}},
#' or \code{\link{ECoGTensor}}, the outputs are
#' also the same type with the same dimensions; for \code{'rave_prepare_power'}
#' repositories, the results will be stored in its \code{'baselined'} element;
#' see 'Examples'.
#'
#' @details The arrays must be four-mode tensor and must have valid named
#' \code{\link{dimnames}}. The dimension names must be \code{'Trial'},
#' \code{'Frequency'}, \code{'Time'}, \code{'Electrode'}, case sensitive.
#'
#' The \code{baseline_windows} determines the baseline windows that are used to
#' calculate time-points of baseline to be included. This can be one
#' or more intervals and must pass the validation function
#' \code{\link{validate_time_window}}.
#'
#' The \code{units} determines the unit of the baseline. It can be one or
#' more of \code{'Trial'}, \code{'Frequency'}, \code{'Electrode'}. The default
#' value is all of them, i.e., baseline for each combination of trial,
#' frequency, and electrode. To share the baseline across trials, please
#' remove \code{'Trial'} from \code{units}. To calculate baseline that should
#' be shared across electrodes (e.g. in some mini-electrodes), remove
#' \code{'Electrode'} from the \code{units}.
#'
#' @examples
#'
#' \dontrun{
#' # The following code need to download additional demo data
#' # Please see https://rave.wiki/ for more details
#'
#' library(raveio)
#' repo <- prepare_subject_power(
#'   subject = "demo/DemoSubject",
#'   time_windows = c(-1, 3),
#'   electrodes = c(14, 15))
#'
#' ##### Direct baseline on the repository
#' power_baseline(x = repo, method = "decibel",
#'                baseline_windows = list(c(-1, 0), c(2, 3)))
#' power_mean <- repo$power$baselined$collapse(
#'   keep = c(2,1), method = "mean")
#' image(power_mean, x = repo$time_points, y = repo$frequency,
#'       xlab = "Time (s)", ylab = "Frequency (Hz)",
#'       main = "Mean power over trial (Baseline: -1~0 & 2~3)")
#' abline(v = 0, lty = 2, col = 'blue')
#' text(x = 0, y = 20, "Aud-Onset", col = "blue", cex = 0.6)
#'
#' ##### Alternatively, baseline on electrode instances
#' baselined <- lapply(repo$power$data_list, function(inst) {
#'   re <- power_baseline(inst, method = "decibel",
#'                        baseline_windows = list(c(-1, 0), c(2, 3)))
#'   collapse2(re, keep = c(2,1), method = "mean")
#' })
#' power_mean2 <- (baselined[[1]] + baselined[[2]]) / 2
#'
#' # Same with precision difference
#' max(abs(power_mean2 - power_mean)) < 1e-6
#'
#'
#' }
#'
#' @export
power_baseline <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Trial", "Frequency", "Electrode"), ...
){
  UseMethod("power_baseline")
}

#' @rdname power_baseline
#' @export
power_baseline.rave_prepare_power <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Frequency", "Trial", "Electrode"),
    electrodes, ...
){
  method <- match.arg(method)
  force(baseline_windows)

  if(missing(electrodes)){
    electrodes <- x$electrode_list
  } else {
    electrodes <- electrodes[electrodes %in% x$electrode_list]
    if(!length(electrodes)) {
      stop("`power_baseline`: none of electrodes specified can be found in the loaded repository")
    }
  }

  signal_type <- x$signal_type

  # if(!inherits(x$baselined, "fastmap2")){
  #   x$baselined <- dipsaus::fastmap2()
  # }

  # Prepare global variables
  baseline_windows <- validate_time_window(baseline_windows)
  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% c("Frequency", "Trial", "Electrode"))){
    stop('`units` must contain 1-3 of the followings: "Frequency", "Trial", "Electrode" (case-sensitive)')
  }
  unit_dims <- c(1L, 3L, 4L)[c("Frequency", "Trial", "Electrode") %in% units]

  sel <- x$electrode_list %in% electrodes

  sub_list <- x$power$data_list[sel]
  sub_elec <- x$electrode_list[sel]

  # dm <- dim(sub_list[[1]])
  # dm[[4]] <- length(sub_elec)

  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(x$power$dimnames$Time >= w[[1]] & x$power$dimnames$Time <= w[[2]])
  })))

  # calculate signature
  digest_key <- list(
    input_signature = x$power$signature,
    signal_type = signal_type,
    rave_data_type = "power",
    method = method,
    unit_dims = unit_dims,
    time_index = time_index,
    dimension = x$power$dim
  )

  signature <- dipsaus::digest(digest_key)

  filebase <- file.path(cache_root(), "_baselined_arrays_", x$power$signature)
  res <- tryCatch({
    res <- filearray::filearray_checkload(
      filebase, mode = "readwrite", symlink_ok = FALSE,
      rave_signature = signature,
      signal_type = signal_type,
      rave_data_type = "power-baselined",
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
      dimension = x$power$dim,
      type = "float",
      partition_size = 1
    )
    res$.mode <- "readwrite"
    res$.header$rave_signature <- signature
    res$.header$signal_type <- signal_type
    res$.header$rave_data_type <- "power-baselined"
    res$.header$baseline_method <- method
    res$.header$unit_dims <- unit_dims
    res$.header$time_index <- time_index
    res$.header$baseline_windows <- baseline_windows
    res$.header$RAVEIO_FILEARRAY_VERSION <- RAVEIO_FILEARRAY_VERSION
    res$.header$ready <- FALSE
    dimnames(res) <- x$power$dimnames
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
        list(
          index = idx,
          electrode = e,
          array = x$power$data_list[[idx]]
        )
      })

      lapply_async(
        input_list,
        FUN = function(el) {
          res[, , , el$index] <- baseline_array(
            x = el$array[drop = FALSE],
            along_dim = 2L,
            baseline_indexpoints = time_index,
            unit_dims = unit_dims,
            method = method
          )
          NULL
        },
        callback = function(el) {
          sprintf("Baseline correction | %s (signal type: %s)",
                  el$electrode,
                  signal_type)
        }
      )

      res$.header$electrodes <- c(
        res$.header$electrodes,
        todo_elec
      )

      res$set_header("ready", TRUE)
    }


    res$.mode <- "readonly"

  } else {

    stop("Baseline across electrode not supported")

  }

  x$power$baselined <- res
  return(x)
  #
  #   if("Electrode" %in% units){
  #
  #
  #
  #   } else {
  #
  #     dir_create2(dirname(bind_base))
  #     bind_array <- filearray::filearray_bind(
  #       .list = sub$data_list,
  #       symlink = symlink_enabled(),
  #       filebase = bind_base,
  #       overwrite = TRUE, cache_ok = TRUE)
  #
  #     res[] <- baseline_array(
  #       x = bind_array[,,, which(sel),drop=FALSE],
  #       along_dim = 2L,
  #       baseline_indexpoints = time_index,
  #       unit_dims = unit_dims,
  #       method = method
  #     )
  #   }
  #
  #
  #   res$set_header("ready", TRUE)
  #   x$power$baselined <- res
  #
  #   return(x)
}


#' @rdname power_baseline
#' @export
power_baseline.FileArray <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Frequency", "Trial", "Electrode"),
    filebase = NULL, ...
){
  method <- match.arg(method)
  # x <- filearray::filearray_load('/Users/dipterix/rave_data/cache_dir/_binded_arrays_/75131880730a1e599bbcd63c798f62b6/power/LFP'); baseline_windows <- c(-1,2); units = c("Trial", "Frequency", "Electrode"); data_only = FALSE; filebase = tempfile(); method = 'percentage'
  baseline_windows <- validate_time_window(baseline_windows)
  dnames <- dimnames(x)
  dm <- dim(x)
  dnn <- c("Frequency", "Time", "Trial", "Electrode")
  if(!identical(names(dnames), dnn)){
    stop('The dimension names are inconsistent, should be c("Frequency", "Time", "Trial", "Electrode")')
  }
  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% dnn)){
    stop('`units` must contain 1-3 of the followings: "Frequency", "Trial", "Electrode" (case-sensitive)')
  }

  unit_dims <- c(1L, 3L, 4L)[c("Frequency", "Trial", "Electrode") %in% units]
  dnames$Time <- as.numeric(dnames$Time)
  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(dnames$Time >= w[[1]] & dnames$Time <= w[[2]])
  })))

  # calculate signatures
  signal_type <- x$get_header("signal_type")
  rave_data_type <- x$get_header("rave_data_type")
  digest_key <- list(
    input_signature = x$get_header("rave_signature", default = "power"),
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

  res <- tryCatch({
    res <- filearray::filearray_checkload(
      filebase, mode = "readwrite", symlink_ok = FALSE,
      rave_signature = signature,
      signal_type = signal_type,
      rave_data_type = "power-baselined",
      ready = TRUE,  # The rest procedure might go wrong, in case failure
      RAVEIO_FILEARRAY_VERSION = RAVEIO_FILEARRAY_VERSION
    )
    # No need to baseline again, the settings haven't changed
    return(res)
  }, error = function(e){
    if(dir.exists(filebase)){ unlink(filebase, recursive = TRUE, force = TRUE) }
    res <- filearray::filearray_create(filebase, dm, type = "float", partition_size = 1)
    res$.mode <- "readwrite"
    res$.header$rave_signature <- signature
    res$.header$signal_type <- signal_type
    res$.header$rave_data_type <- "power-baselined"
    res$.header$baseline_method <- method
    res$.header$unit_dims <- unit_dims
    res$.header$time_index <- time_index
    res$.header$baseline_windows <- baseline_windows
    res$.header$RAVEIO_FILEARRAY_VERSION <- RAVEIO_FILEARRAY_VERSION
    res$.header$ready <- FALSE
    dimnames(res) <- dnames
    # # automatically run
    # res$.save_header()
    res
  })


  if("Electrode" %in% units){

    lapply_async(seq_len(dm[[length(dm)]]), function(ii){
      res[, , , ii] <-
        baseline_array(
          x = x[, , , ii, drop = FALSE],
          along_dim = 2L,
          baseline_indexpoints = time_index,
          unit_dims = unit_dims,
          method = method
        )
      NULL
    })

  } else {

    output <- baseline_array(x = x[drop = FALSE],
                            along_dim = 2L,
                            baseline_indexpoints = time_index,
                            unit_dims = unit_dims,
                            method = method)
    res[] <- output

  }

  res$set_header("ready", TRUE)

  res

}

#' @rdname power_baseline
#' @export
power_baseline.array <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Trial", "Frequency", "Electrode"), ...
){
  method <- match.arg(method)
  baseline_windows <- validate_time_window(baseline_windows)
  dm <- dim(x)
  dnames <- dimnames(x)
  dnn <- names(dnames)
  stopifnot2(all(dnn %in% c("Frequency", "Time", "Trial", "Electrode")) && length(dm) == 4,
             msg = 'The dimension names are inconsistent, must contain 4 modes: "Frequency", "Time", "Trial", "Electrode"')

  dnames$Time <- as.numeric(dnames$Time)
  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(dnames$Time >= w[[1]] & dnames$Time <= w[[2]])
  })))
  time_margin <- which(dnn == "Time")

  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% dnn)){
    stop('`units` must contain 1-3 of the followings: "Frequency", "Trial", "Electrode" (case-sensitive)')
  }
  unit_dims <- which(dnn %in% units)

  baseline_array(x = x, along_dim = time_margin, baseline_indexpoints = time_index, unit_dims = unit_dims, method = method)

}

#' @rdname power_baseline
#' @export
power_baseline.ECoGTensor <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Trial", "Frequency", "Electrode"), filebase = NULL, hybrid = TRUE, ...
){
  method <- match.arg(method)
  baseline_windows <- validate_time_window(baseline_windows)
  dm <- dim(x)
  dnames <- dimnames(x)
  dnn <- names(dnames)
  stopifnot2(identical(dnn, c("Trial", "Frequency", "Time", "Electrode")),
             msg = 'The dimension names are inconsistent, must contain 4 modes in sequence: "Trial", "Frequency", "Time", "Electrode"')

  dnames$Time <- as.numeric(dnames$Time)
  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(dnames$Time >= w[[1]] & dnames$Time <= w[[2]])
  })))

  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% dnn)){
    stop('`units` must contain 1-3 of the followings: "Frequency", "Trial", "Electrode" (case-sensitive)')
  }
  unit_dims <- which(dnn %in% units)
  time_margin <- which(dnn == "Time")

  digest_key <- list(
    method = method,
    unit_dims = unit_dims,
    time_index = time_index,
    baseline_windows = baseline_windows,
    dimension = dm,
    dimnames = dnames,
    session_string = get(".session_string")
  )
  signature <- dipsaus::digest(digest_key)

  if(!length(filebase)){
    filebase <- file.path(cache_root(), "_baselined_arrays_old_", signature)
  }
  dir_create2(filebase)

  if("Electrode" %in% units && hybrid){
    nelec <- dm[[length(dm)]]
    sel <- rep(FALSE, nelec)
    baselined_data <- lapply_async(seq_len(nelec), function(ii){
      sel[[ii]] <- TRUE
      dnames$Electrode <- dnames$Electrode[[ii]]
      slice <- x$subset(Electrode ~ sel, drop = FALSE, data_only = TRUE)
      slice <- baseline_array(slice, along_dim = time_margin, baseline_indexpoints = time_index, unit_dims = unit_dims, method = method)

      utils::capture.output({
        re <- ECoGTensor$new(data = slice, dim = dim(slice), varnames = x$varnames, swap_file = file.path(filebase, ii), temporary = FALSE, hybrid = FALSE, dimnames = dnames)
        re$to_swap_now()
      })
      re
    })

    lapply(baselined_data, function(re){
      re$temporary <- TRUE
    })

    baselined_data <- join_tensors(baselined_data, temporary = TRUE)
  } else {
    re <- baseline_array(x$get_data(), along_dim = time_margin, baseline_indexpoints = time_index, unit_dims = unit_dims, method = method)
    re <- ECoGTensor$new(data = re, dim = dim(x), varnames = x$varnames, swap_file = file.path(filebase, 0), temporary = TRUE, hybrid = hybrid, dimnames = dnames)
  }

}
