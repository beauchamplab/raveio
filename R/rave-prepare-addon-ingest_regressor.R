# DIPSAUS DEBUG START
# repository <- raveio::prepare_subject_power("demo/DemoSubject")
# channels <- c(14, 100)
# apply_preprocess = FALSE
# quiet = FALSE
# sample_rates = c(30000, 100)


#' @name ingest_regressor
#' @title Ingest signals according to 'RAVE' repository epoch
#' @param repository 'RAVE' repository, for example, generated from
#' \code{\link{prepare_subject_power}}. The repository must contain epoch
#' information.
#' @param source data source, see \code{source_type}
#' @param source_type type of the data source, choices are
#' \describe{
#' \item{\code{'channel'}}{\code{source} should be interpreted as subject
#' channels (integer or series of integers)}
#' \item{\code{'file'}}{\code{source} should be interpreted as file path; the
#' file format must be either 'Matlab' or 'HDF5', with data names to be the
#' blocks in the "epoch" file. The sample rate of underlying signals
#' must coincide with the sample rate presented in the repository}
#' \item{\code{'r_object'}}{\code{source} should be a list of R objects, where
#' the names are blocks in the "epoch" file and the data are signal traces;
#' The sample rate of underlying signals
#' must coincide with the sample rate presented in the repository}
#' }
#' @param filter \code{NULL} or function; only used if \code{source_type} is
#' \code{'channel'}. For function,
#' \code{filter} must have two exact arguments, with the first argument taking
#' the signal data matrix (time point by channel) and second argument taking
#' sample rates of length 2. The first sample rate is the original sampling
#' frequency of the data; the second sample rate is sampling frequency
#' derived from the repository.
#' @param ... passed to internal functions.
#' @returns A matrix of time by trial
#'
NULL

ingest_regressor_internal <- function(repository, signals) {
  # signals <- list(block1 = ..., block2 = ..., ...)

  time_points <- repository$time_points
  epoch_table <- repository$epoch$table
  stitch_events <- repository$stitch_events
  if( inherits(repository, "rave_prepare_power") ) {
    trial_numbers <- repository$power$dimnames$Trial
    sample_rate <- repository$subject$power_sample_rate
  } else {
    trial_numbers <- epoch_table$Trial
    sample_rate <- repository$subject$power_sample_rate
  }

  if( length(stitch_events) == 2 ) {
    stitch_events_pre <- repository$epoch$get_event_colname(event = stitch_events[[1]])
    stitch_events_post <- repository$epoch$get_event_colname(event = stitch_events[[2]])
  } else {
    stitch_events_pre <- "Time"
    stitch_events_post <- "Time"
  }

  n_timepoints <- length(time_points)
  time_selection <- time_points <= 0
  time_points_pre <- round((time_points[time_points <= 0]) * sample_rate)
  time_points_post <- round((time_points[time_points > 0]) * sample_rate)

  results <- lapply(trial_numbers, function(trial_number) {
    # trial_number <- 1
    row_idx <- which(epoch_table$Trial == trial_number)[[1]]
    row <- epoch_table[row_idx, ]

    # time x ...
    signal <- signals[[row$Block]][drop = FALSE]
    if(is.array(signal) || is.matrix(signal)) {
      dm <- dim(signal)
      dim(signal) <- c(dm[[1]], length(signal) / dm[[1]])
    } else {
      dm <- NULL
    }

    time_index_pre <- round(row[[stitch_events_pre]] * sample_rate)
    time_index_post <- round(row[[stitch_events_post]] * sample_rate)
    if( time_index_pre == 0 ) { time_index_pre <- 1 }
    if( time_index_post == 0 ) { time_index_post <- 1 }

    if(length(dm)) {
      re <- array(0.0, c(n_timepoints, ncol(signal)))
      re[time_selection, ] <- signal[time_index_pre + time_points_pre, ]
      re[!time_selection, ] <- signal[time_index_post + time_points_post, ]
      dim(re) <- c(n_timepoints, dm[-1])
    } else {
      re <- rep(0.0, n_timepoints)
      re[time_selection] <- signal[time_index_pre + time_points_pre]
      re[!time_selection] <- signal[time_index_post + time_points_post]
    }
    re
  })

  return(simplify2array(results, higher = TRUE))
}

ingest_regressor_channels <- function(
    repository, channels, filter = NULL,
    apply_preprocess = FALSE, ..., quiet = FALSE) {

  if(!inherits(repository, "rave_repository") || !length(repository$epoch)) {
    stop("ingest_regressor_channels: `repository` must be a RAVE repository with epoch information.")
  }
  if(!inherits(repository, "rave_prepare_power")) {
    stop("ingest_regressor_channels: currently only power `repository`is implemented.")
  }

  has_reference <- length(repository$reference_name) > 0
  if( has_reference ) {
    reference_name <- repository$reference_name
  } else {
    reference_name <- "noref"
  }

  repository_sample_rate <- repository$subject$power_sample_rate

  new_repo <- prepare_subject_with_blocks(
    subject = repository$subject,
    electrodes = channels,
    reference_name = reference_name,
    blocks = unique(repository$epoch$table$Block),
    raw = !apply_preprocess,
    time_frequency = FALSE,
    quiet = quiet
  )

  if(!is.function(filter)) {
    filter <- function(data, sample_rates) {
      original_sample_rate <- sample_rates[[1]]
      target_sample_rate <- sample_rates[[2]]
      # check if we can use decimate
      q <- original_sample_rate / target_sample_rate
      if(abs(q - round(q)) < 1e-6) {
        q <- round(q)
        ret <- gsignal::decimate(x = data, q = q, ftype = "fir")
      } else {
        ret <- gsignal::resample(x = data, p = target_sample_rate, q = original_sample_rate)
      }
      return(ret)
    }
  }

  filtered_signals <- structure(
    names = names(new_repo$block_data),
    lapply(new_repo$block_data, function(block_data) {
      # block_data <- new_repo$block_data[[1]]
      voltage_data_container <- block_data$voltage
      original_sample_rate <- voltage_data_container$sample_rate
      voltage_data <- subset(voltage_data_container$data, Electrode ~ Electrode %in% new_repo$electrode_list, drop = FALSE)
      filtered_signal <- filter(voltage_data, c(original_sample_rate, repository_sample_rate))
      expected_length <- nrow(voltage_data) / original_sample_rate * repository_sample_rate
      actual_length <- length(filtered_signal)
      if(is.array(filtered_signal) || is.matrix(filtered_signal)) {
        actual_length <- nrow(filtered_signal)
      }
      length_difference <- abs(expected_length - actual_length)

      if( abs(length_difference) > 2 ) {
        stop("ingest_regressor_channels: the raw signals, after aggregation and filter, should have the sample rate `", repository_sample_rate, " Hz`. Please check if the filter properly down-sample the signal. (Unexpected number of time-points: expected: ", ceiling(expected_length), "; actual: ", actual_length)
      }
      filtered_signal
    })
  )

  results <- ingest_regressor_internal(repository = repository, signals = filtered_signals)

  return(results)
}


#' @rdname ingest_regressor
#' @export
ingest_regressor <- function(repository, source, source_type = c("channel", "file", "r_object"), filter = NULL, ...) {

  source_type <- match.arg(source_type)

  if( source_type == "channel" ) {
    channels <- dipsaus::parse_svec(source)
    if(!length(channels)) {
      stop(sprintf("Cannot ingest regressors from channel `%s`", deparse1(source)))
    }
    if(!is.function(filter)) {
      filter <- NULL
    }

    res <- ingest_regressor_channels(repository = repository, channels = channels, filter = filter, ...)
  } else {
    # TODO: make some checks???
    if( source_type == "file" ) {
      source <- read_mat(normalizePath(source, mustWork = TRUE))
    } else {
      force(source)
    }
    res <- ingest_regressor_internal(repository = repository, signals = source)
  }
  res
}
