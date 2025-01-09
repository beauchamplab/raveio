#' Compose a "phantom" channel from existing electrodes
#' @description
#' In some cases, for example, deep-brain stimulation ('DBS'),
#' it is often needed to analyze averaged electrode channels from segmented
#' 'DBS' leads, or create bipolar contrast between electrode channels, or
#' to generate non-equally weighted channel averages for 'Laplacian' reference.
#' \code{compose_channel} allows users to generate a phantom channel that does
#' not physically exist, but is treated as a normal electrode channel in 'RAVE'.
#'
#' @param subject 'RAVE' subject
#' @param number new channel number, must be positive integer, cannot be
#' existing electrode channel numbers
#' @param from a vector of electrode channels that is used to compose this
#' new channel, must be non-empty; see \code{weights} if these channels are
#' not equally weighted.
#' @param weights numerical weights used on each \code{from} channels; the
#' length of \code{weights} must equals to the length of \code{from};
#' default is equally weighted for each channel (mean of
#' \code{from} channels).
#' @param normalize whether to normalize the weights such that the composed
#' channel has the same variance as \code{from} channels; default is false
#' @param force whether to overwrite existing composed channel if it exists;
#' default is false. By specifying \code{force=TRUE}, users are risking breaking
#' the data integrity since any analysis based on the composed channel is
#' no longer reproducible. Also users cannot overwrite original channels
#' under any circumstances.
#' @param label the label for the composed channel; will be stored at
#' \code{'electrodes.csv'}
#' @param signal_type signal type of the composed channel; default is
#' \code{'auto'} (same as the first \code{from} channel); other choices
#' see \code{\link{SIGNAL_TYPES}}
#' @returns Nothing
#'
#' @examples
#' library(raveio)
#'
#'
#' # Make sure demo subject exists in this example, just want to make
#' # sure the example does not error out
#' if(
#'   interactive() && "demo" %in% get_projects() &&
#'   "DemoSubject" %in% as_rave_project('demo')$subjects() &&
#'   local({
#'     subject <- as_rave_subject("demo/DemoSubject")
#'     !100 %in% subject$electrodes
#'   })
#' ) {
#'
#'   # the actual example code:
#'   # new channel 100 = 2 x channel 14 - (channe 15 + 16)
#'   compose_channel(
#'     subject = "demo/DemoSubject",
#'     number = 100,
#'     from = c(14, 15, 16),
#'     weights = c(2, -1, -1),
#'     normalize = FALSE
#'   )
#'
#' }
#'
#'
#'
#'
#' @export
compose_channel <- function(
    subject, number, from, weights = rep(1 / length(from), length(from)),
    normalize = FALSE, force = FALSE,
    label = sprintf("Composed-%s", number),
    signal_type = c("auto", "LFP", "Spike", "EKG", "Audio",
                    "Photodiode", "Unknown")) {
  # DIPSAUS DEBUG START
  # subject <- "demo/DemoSubject"
  # number = 100
  # from = c(14,15,16)
  # weights = c(1,1,1)
  # normalize <- TRUE
  # signal_type <- "auto"
  # label = sprintf("Composed-%s", dipsaus::deparse_svec(from))

  signal_type <- match.arg(signal_type)

  if(length(label) != 1 || is.na(label)) {
    label <- "Composed"
  }

  # must import the ephys data first
  subject <- restore_subject_instance(subject, strict = TRUE)
  preprocess_settings <- subject$preprocess_settings
  pdata <- preprocess_settings$data
  number <- as.integer(number)
  if( length(number) != 1 || is.na(number) || number <= 0 || is.infinite(number)) {
    stop("Invalid electrode channel number: ", number)
  }

  from <- as.integer(from)
  weights <- as.numeric(weights)
  if(length(from) != length(weights)) {
    stop("`from` must have the same length as `weights`")
  }
  if(length(from) == 0) {
    stop("`compose_channel`: `from` must be non-empty.")
  }
  if(any(is.na(from)) || !all(from %in% subject$electrodes)) {
    stop("Invalid `from` channels: ", paste(from, collapse = ", "))
  }
  if(any(is.na(weights)) || sum(weights^2) < 1e-5) {
    stop("Invalid `weights`: must have at least one non-zero weight.")
  }
  if(normalize) {
    # true_weights = weights / normalize_factor
    normalize_factor <- sqrt(sum(weights^2))
  } else {
    normalize_factor <- 1
  }
  weights2 <- weights / normalize_factor

  # make sure the from channels have the same sample rate
  sel <- subject$electrodes %in% from
  sample_rate <- unique(subject$raw_sample_rates[sel])
  if(length(sample_rate) > 1) {
    stop("Cannot combine channels with different sampling frequencies: [", paste(sample_rate, collapse = ", "), "]")
  }

  if(signal_type == "auto") {
    signal_type <- subject$electrode_types[sel][[1]]
  }

  # check if this channel is composed
  if(number %in% from) {
    stop("Cannot compose channel ", number, " from channels that contain this number: ", dipsaus::deparse_svec(from))
  }
  lapply(from, function(f) {
    if(isTRUE(pdata[[f]]$composed) &&
       number %in% pdata[[f]]$composed_params$from) {
      stop("Cannot compose channel ", number, " recursively. This channel has been used to compose another channel ", f, ". (Basically when you compose channel B from channel A, you cannot use B to compose channel A. This will cause recursive composition and is disallowed.)")
    }
  })
  if( number %in% subject$electrodes ) {
    if(!isTRUE(subject$preprocess_settings$data[[number]]$composed)) {
      stop("Subject already has existing electrode channel: ", number, ". This channel seems not to be a composed channel, please change `number` to any positive integer other than ", dipsaus::deparse_svec(subject$electrodes))
    }
    if(!force) {
      stop("Subject already has existing composed channel: ", number,
           ". It's not recommended to overwrite existing composed channels as this might affect the data integrity (analyses based on these channels will be no-longer valid. If you know what you are doing (for example, you are at pre-processing stage), you can overwrite the channels by `force=TRUE` in the function call.")
    }
  }

  # Remove this channel temporarily
  if("channels" %in% names(pdata)) {
    pdata$channels <- pdata$electrodes[!pdata$electrodes %in% number]
  }
  pdata$electrodes <- pdata$electrodes[!pdata$electrodes %in% number]
  pdata$`@remove`(as.character(number))
  preprocess_settings$save()

  has_notch <- all(subject$notch_filtered[sel])
  has_wavelet <- has_notch && all(subject$has_wavelet[sel])
  blocks <- subject$blocks

  # Create the new electrode channel
  progress <- dipsaus::progress2(
    title = sprintf("Compose a new channel [%s]", number),
    max = 1 + length(blocks),
    shiny_auto_close = TRUE
  )

  # Create raw ->
  # "/Users/dipterix/rave_data/data_dir/demo/DemoSubject/rave/preprocess/voltage/electrode_13.h5"
  fpath <- file.path(subject$preprocess_path, "voltage", sprintf("electrode_%d.h5", number))
  fpath2 <- file.path(subject$data_path, "voltage", sprintf("%d.h5", number))

  fpower <- file.path(subject$data_path, "power", sprintf("%d.h5", number))
  fphase <- file.path(subject$data_path, "phase", sprintf("%d.h5", number))

  lapply(blocks, function(block) {
    progress$inc(detail = sprintf("Block %s", block))

    dname <- sprintf("/raw/%s", block)
    s <- 0
    for(ii in seq_along(from)) {
      s <- s + raveio::load_h5(
        file = file.path(subject$preprocess_path, "voltage",
                         sprintf("electrode_%d.h5", from[[ii]])),
        name = dname,
        ram = TRUE
      ) * weights2[[ii]]
    }

    save_h5(x = s, file = fpath, name = dname, chunk = 1024, replace = TRUE, ctype = "numeric")

    # re-apply Notch filters
    if( has_notch ) {
      notch_params <- preprocess_settings$notch_params
      if(length(notch_params$frequencies)) {
        s <- ravetools::notch_filter(
          s = s, sample_rate = sample_rate,
          lb = notch_params$frequencies - notch_params$half_bandwidths,
          ub = notch_params$frequencies + notch_params$half_bandwidths
        )
      }
      save_h5(x = s, file = fpath, name = sprintf("/notch/%s", block),
              chunk = 1024, replace = TRUE, ctype = "numeric")

      save_h5(x = s, file = fpath2, name = sprintf("/raw/voltage/%s", block),
              chunk = 1024, replace = TRUE, ctype = "numeric")
    }
    if( has_wavelet ) {

      s <- 0
      for(ii in seq_along(from)) {
        power <- load_h5(
          file = file.path(subject$data_path, "power",
                           sprintf("%d.h5", from[[ii]])),
          name = sprintf("/raw/power/%s", block),
          ram = TRUE
        )
        phase <- load_h5(
          file = file.path(subject$data_path, "phase",
                           sprintf("%d.h5", from[[ii]])),
          name = sprintf("/raw/phase/%s", block),
          ram = TRUE
        )
        s <- s + (sqrt(power) * exp(1i * phase)) * weights2[[ii]]
      }

      power <- Mod(s)^2
      phase <- Arg(s)

      save_h5(x = power, file = fpower, name = sprintf("/raw/power/%s", block),
              replace = TRUE, ctype = "numeric")

      save_h5(x = phase, file = fphase, name = sprintf("/raw/phase/%s", block),
              replace = TRUE, ctype = "numeric")


    }
    return()
  })

  progress$inc("Finalizing...")

  if( has_notch ) {
    save_h5(x = "noref", file = fpath2, name = "reference",
            replace = TRUE, ctype = "character")
  }

  if( has_wavelet ) {
    save_h5(x = "noref", file = fpower, name = "reference",
            replace = TRUE, ctype = "character")
    save_h5(x = "noref", file = fphase, name = "reference",
            replace = TRUE, ctype = "character")
  }

  # reference, cached_reference
  lapply(subject$reference_names, function(ref_name) {
    tryCatch({
      tbl <- subject$meta_data(meta_type = "references", meta_name = ref_name)
      if(number %in% tbl$Electrode) {
        sel <- tbl$Electrode == number
        tbl$Reference[sel] <- "noref"
        tbl$Type[sel] <- "noref"
      } else {
        tbl <- tbl[, c("Electrode", "Group", "Reference", "Type")]
        tbl <- rbind(tbl, data.frame(
          Electrode = number,
          Group = "Composed",
          Reference = "noref",
          Type = "No Reference"
        ))
      }
      safe_write_csv(x = tbl, file = file.path(subject$meta_path, sprintf("reference_%s.csv", ref_name)), row.names = FALSE)
    }, error = function(e) {
      catgl("Cannot update reference table {ref_name} due to the following warning: ", e$message, level = "WARNING")
    })
  })
  cref <- file.path(subject$cache_path, "cached_reference.csv")
  if(file.exists(cref)) {
    tryCatch({
      tbl <- safe_read_csv(cref)
      if(number %in% tbl$Electrode) {
        sel <- tbl$Electrode == number
        tbl$Reference[sel] <- "noref"
      } else {
        tbl <- tbl[, c("Electrode", "Reference")]
        tbl <- rbind(tbl, data.frame(
          Electrode = number,
          Reference = "noref"
        ))
      }
      safe_write_csv(x = tbl, file = cref, row.names = FALSE)
    }, error = function(e) {
      catgl("Cannot update cached_reference.csv table due to the following warning: ", e$message, level = "WARNING")
    })
  }

  # electrodes.csv
  elec_path <- file.path(subject$meta_path, "electrodes.csv")
  if(file.exists(elec_path)) {
    electrode_table <- safe_read_csv(elec_path)

    nms <- names(electrode_table)
    if(all(c("x", "electrode") %in% tolower(nms))) {
      idx1 <- which(tolower(nms) == "x")[[1]]
      idx2 <- which(tolower(nms) == "electrode")[[1]]
      if(idx1 < idx2) {
        nms <- nms[-idx1]
      }
      electrode_table <- electrode_table[, nms]
    }

    if(!number %in% electrode_table$Electrode) {
      row <- electrode_table[which(electrode_table$Electrode %in% from)[[1]],, drop = FALSE]
      append <- TRUE
    } else {
      append <- which(electrode_table$Electrode == number)[[1]]
      row <- electrode_table[append,, drop = FALSE]
    }
    for(nm in names(row)) {
      if(is.numeric(row[[nm]])) {
        switch(
          nm,
          "Radius" = {},
          "Electrode" = {
            row[[nm]] <- number
          },
          "VertexNumber" = {
            row[[nm]] <- -1
          },
          {
            row[[nm]] <- 0
          }
        )
      } else {
        switch(
          nm,
          "Label" = {
            row[[nm]] <- label
          },
          "LabelPrefix" = {
            row[[nm]] <- "Composed"
          },
          "LocationType" = {
            row[[nm]] <- "iEEG"
          },
          "Hemisphere" = {},
          "SurfaceElectrode" = {
            row[[nm]] <- FALSE
          },
          {
            if(startsWith(nm, "FSLabel")) {
              row[[nm]] <- "Unknown"
            } else {
              row[[nm]] <- ""
            }
          }
        )
      }
    }

    if(isTRUE(append)) {
      electrode_table <- rbind(electrode_table, row)
    } else {
      electrode_table[append, ] <- row
    }
  } else {
    elecs <- subject$electrodes
    stype <- subject$electrode_types
    labels <- rep("NoLabel", length(elecs))
    if(!number %in% elecs) {
      elecs <- c(elecs, elecs)
      stype <- c(stype, signal_type)
      labels <- c(labels, label)
    }
    electrode_table <- data.frame(
      Electrode = sort(unique(c(subject$electrodes, number))),
      Coord_x = 0,
      Coord_y = 0,
      Coord_z = 0,
      Label = labels,
      SignalType = stype
    )
  }
  safe_write_csv(electrode_table, file = elec_path, row.names = FALSE)

  pdata[[number]] <- list(
    sample_rate = sample_rate,
    notch_filtered = has_notch,
    has_wavelet = has_wavelet,
    data_imported = TRUE,
    electrode_locked = TRUE,
    electrode_type = signal_type,
    composed = TRUE,
    composed_params = list(
      from = from,
      weights = weights2,
      original_weights = weights,
      normalize_factor = normalize_factor
    )
  )
  if("channels" %in% names(pdata)) {
    pdata$channels <- sort(unique(c(pdata$channels, number)))
  }
  pdata$electrodes <- sort(unique(c(pdata$electrodes, number)))
  preprocess_settings$save()

  message("Done.")
  return(invisible())

}
