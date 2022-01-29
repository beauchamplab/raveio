#' @title Collapse power array with given analysis cubes
#' @param x a \code{\link[filearray]{FileArray-class}} array, must have 4 modes
#' in the following sequence \code{Frequency}, \code{Time}, \code{Trial},
#' and \code{Electrode}
#' @param analysis_index_cubes a list of analysis indices for each mode
#' @return a list of collapsed (mean) results
#' \describe{
#' \item{\code{freq_trial_elec}}{collapsed over time-points}
#' \item{\code{freq_time_elec}}{collapsed over trials}
#' \item{\code{time_trial_elec}}{collapsed over frequencies}
#' \item{\code{freq_time}}{collapsed over trials and electrodes}
#' \item{\code{freq_elec}}{collapsed over trials and time-points}
#' \item{\code{freq_trial}}{collapsed over time-points and electrodes}
#' \item{\code{time_trial}}{collapsed over frequencies and electrodes}
#' \item{\code{time_elec}}{collapsed over frequencies and trials}
#' \item{\code{trial_elec}}{collapsed over frequencies and time-points}
#' \item{\code{freq}}{power per frequency, averaged over other modes}
#' \item{\code{time}}{power per time-point, averaged over other modes}
#' \item{\code{trial}}{power per trial, averaged over other modes}
#' }
#' @examples
#'
#'
#' # Generate a 4-mode tensor array
#' x <- filearray::filearray_create(
#'   tempfile(), dimension = c(16, 100, 20, 5),
#'   partition_size = 1
#' )
#' x[] <- rnorm(160000)
#' dnames <- list(
#'   Frequency = 1:16,
#'   Time = seq(0, 1, length.out = 100),
#'   Trial = 1:20,
#'   Electrode = 1:5
#' )
#' dimnames(x) <- dnames
#'
#' # Collapse array
#' results <- collapse_power(x, list(
#'   overall = list(),
#'   A = list(Trial = 1:5, Frequency = 1:6),
#'   B = list(Trial = 6:10, Time = 1:50)
#' ))
#'
#' # Plot power over frequency and time
#' groupB_result <- results$B
#' image(t(groupB_result$freq_time),
#'       x = dnames$Time[groupB_result$cube_index$Time],
#'       y = dnames$Frequency[groupB_result$cube_index$Frequency],
#'       xlab = "Time (s)",
#'       ylab = "Frequency (Hz)",
#'       xlim = range(dnames$Time))
#'
#'
#' @export
collapse_power <- function(x, analysis_index_cubes){

  dm <- dim(x)
  ndims <- length(dm)
  pdim <- dm[-ndims]
  nelec <- dm[[ndims]]
  group_names <- names(analysis_index_cubes)

  analysis_index_cubes <- lapply(analysis_index_cubes, function(cube){
    cube$Frequency %?<-% seq_len(dm[[1]])
    cube$Time %?<-% seq_len(dm[[2]])
    cube$Trial %?<-% seq_len(dm[[3]])
    cube$Electrode %?<-% seq_len(dm[[4]])
    cube
  })

  elec_idx <- sort(unique(unlist(lapply(analysis_index_cubes, "[[", "Electrode"))))
  elec_sel <- rep(FALSE, nelec)
  elec_sel[elec_idx] <- TRUE

  # freq_time_trial <- array(0, pdim)

  fun <- function(e){
    v <- x[, , , e, drop = FALSE]
    dim(v) <- pdim
    # freq_time_trial <<- freq_time_trial + v
    re <- list()
    for(ii in seq_along(analysis_index_cubes)){
      cube <- analysis_index_cubes[[ii]]

      if(e %in% cube$Electrode){
        cube_data <- v[cube$Frequency, cube$Time, cube$Trial, drop = FALSE]
        freq_time_elec <- dipsaus::collapse(cube_data, keep = c(1, 2), average = TRUE)
        time_trial_elec <- dipsaus::collapse(cube_data, keep = c(2, 3), average = TRUE)
        freq_trial_elec <- dipsaus::collapse(cube_data, keep = c(1, 3), average = TRUE)

        re[[sprintf("freq_time_elec_%s", ii)]] <- freq_time_elec
        re[[sprintf("time_trial_elec_%s", ii)]] <- time_trial_elec
        re[[sprintf("freq_trial_elec_%s", ii)]] <- freq_trial_elec
        re[[sprintf("freq_elec_%s", ii)]] <- rowMeans(freq_time_elec)
        re[[sprintf("time_elec_%s", ii)]] <- colMeans(freq_time_elec)
        re[[sprintf("trial_elec_%s", ii)]] <- colMeans(freq_trial_elec)
      }

    }
    re
  }

  if(inherits(future::plan(), "multicore")){
    initial_collapse <- dipsaus::lapply_async2(
      x = which(elec_sel), FUN = fun, plan = FALSE
    )
  } else {
    initial_collapse <- lapply(which(elec_sel), fun)
  }


  # initial_names <- rownames(initial_collapse)
  #
  # initial_collapse <- structure(lapply(seq_along(initial_names), function(ii){
  #   simplify2array(initial_collapse[ii,], higher = TRUE)
  # }), names = initial_names)

  structure(
    lapply(seq_along(analysis_index_cubes), function(ii){
      cube <- analysis_index_cubes[[ii]]
      re <- dipsaus::fastmap2()
      re$name = group_names[[ii]]
      re$cube_index = cube

      for(nm in c("freq_time_elec", "time_trial_elec", "freq_trial_elec",
                  "freq_elec", "time_elec", "trial_elec")) {
        re[[nm]] = simplify2array(
          dipsaus::drop_nulls(
            lapply(initial_collapse, '[[',
                   sprintf("%s_%s", nm, ii))
          ),
          higher = TRUE
        )
      }

      # re$freq_time_trial <- freq_time_trial[cube$Frequency, cube$Time, cube$Trial, drop = FALSE]
      re$freq_time <- dipsaus::collapse(re$freq_time_elec, keep = c(1, 2), average = TRUE)
      re$freq_trial <- dipsaus::collapse(re$freq_trial_elec, keep = c(1, 2), average = TRUE)
      re$freq <- rowMeans(re$freq_time)
      re$time_trial <- dipsaus::collapse(re$time_trial_elec, keep = c(1, 2), average = TRUE)
      re$time <- rowMeans(re$time_elec)
      re$trial <- rowMeans(re$trial_elec)
      re$elec <- colMeans(re$freq_elec)
      class(re) <- c("power_collapse_list", class(re))
      re
    }),
    names = group_names
  )
}

#' @export
print.power_collapse_list <- function(x, ...){
  name <- x$name
  if(!length(name)){ name <- "(unnamed)" }
  cat("Collapse list of <", name, ">\n", sep = "")
  NextMethod("print")
}

# analysis_index_cubes <- list(
#   list(),
#   list(
#     Trial = 1:200,
#     Frequency = 1:16,
#     Electrode = 1
#   ),
#   list(
#     Trial = 1:10,
#     Frequency = 1:4,
#     Time = 1:20
#   )
# )
# re <- collapse_power(x, analysis_index_cubes)
