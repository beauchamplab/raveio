
#' @rdname rave-prepare
#' @export
prepare_subject_with_blocks <- function(subject, electrodes, reference_name, blocks, signal_type = "LFP", time_frequency = signal_type == "LFP", env = parent.frame(), repository_id = NULL, ...){

  # subject <- "automated/PAV004__block005"
  # electrodes <- " 14:20"
  # time_frequency <- TRUE
  # signal_type <- "LFP"

  if(!isTRUE(signal_type %in% SIGNAL_TYPES)) {
    stop("`prepare_subject_with_blocks`: signal type must be a string of length 1, and must be from the following list: ", paste(SIGNAL_TYPES, collapse = ", "))
  }

  re <- dipsaus::fastmap2()
  re$signal_type <- signal_type
  subject <- as_rave_subject(subject)

  # ----- project -----
  re$project <- subject$project

  # ----- subject -----
  re$subject <- subject

  if(missing(electrodes)){
    electrodes <- subject$get_default(
      "electrodes", default_if_missing = subject$electrodes)
    message("No electrodes specified, trying to get electrodes: ", dipsaus::deparse_svec(electrodes))
  }
  electrodes <- sort(dipsaus::parse_svec(electrodes))
  potential_elecs <- subject$electrodes[subject$electrode_types == signal_type]
  electrodes <- electrodes[electrodes %in% potential_elecs]

  if(!length(electrodes)) {
    stop("No valid electrode is selected.")
  }
  re$electrode_list <- electrodes

  # check blocks
  if(missing(blocks)) {
    blocks <- subject$get_default(
      "blocks", default_if_missing = subject$blocks)
    message("No blocks specified, loading default blocks: ", paste(blocks, collapse = ", "))
  }
  blocks <- blocks[blocks %in% subject$blocks]
  if(!length(blocks)) {
    stop("No valid block is selected")
  }

  # check reference_name
  if(missing(reference_name)) {
    reference_name <- subject$get_default(
      "reference_name", default_if_missing = "No reference")
    message("No reference table specified, loading default reference: ", paste(reference_name, collapse = ", "))
  }
  if(length(reference_name)) {
    reference_name <- reference_name[reference_name %in% subject$reference_names]
  }
  if(!length(reference_name)) {
    catgl("The signal will be as-is with no reference.", level = "DEFAULT")
    re$reference_name <- "noref"
    re$reference_table <- data.frame(
      Electrode = subject$electrodes,
      Group = "Default",
      Reference = "noref",
      Type = "No Reference"
    )
  } else {
    print(reference_name)
    reference_name <- reference_name[[1]]
    re$reference_name <- reference_name
    re$reference_table <- subject$get_reference(reference_name)
  }

  ref_table <- re$reference_table[re$reference_table$Electrode %in% electrodes, ]

  # check if wavelet is required
  if( time_frequency ) {

    if(!any(subject$has_wavelet)) {
      stop("Please run Wavelet module first.")
    }
    e <- subject$electrodes[subject$electrodes %in% electrodes & subject$has_wavelet][[1]]
    elec <- new_electrode(subject = subject, number = e)
    wavelet_ntimepoints <- structure(lapply(blocks, function(block) {
      dat <- load_h5(elec$power_file, sprintf("raw/power/%s", block), ram = FALSE)
      dim(dat)[[2]]
    }), names = blocks)

  } else {
    if(!any(subject$preprocess_settings$data_imported)) {
      stop("Please import data first.")
    }
    wavelet_ntimepoints <- list()
  }

  e <- subject$electrodes[subject$electrodes %in% electrodes &
                            subject$preprocess_settings$data_imported][[1]]
  elec <- new_electrode(subject = subject, number = e)
  voltage_ntimepoints <- structure(lapply(blocks, function(block) {
    dat <- load_h5(elec$voltage_file, sprintf("raw/voltage/%s", block), ram = FALSE)
    length(dat)
  }), names = blocks)

  # ----- electrode_table -----
  electrode_table <- subject$get_electrode_table(
    reference_name = ".fake",
    subset = FALSE,
    simplify = FALSE)
  re$electrode_table <- merge(electrode_table, re$reference_table, by = "Electrode", suffixes = c("_x", ""), all.x = TRUE)
  re$electrode_table$Reference[is.na(re$electrode_table$Reference)] <- "noref"
  re$electrode_table$isLoaded <- re$electrode_table$Electrode %in% electrodes

  # create reference instances
  ref_names <- unique(re$reference_table$Reference)
  ref_names <- ref_names[!ref_names %in% c("noref", "")]
  if(length(ref_names)) {
    refs <- structure(lapply(ref_names, function(ref_name) {
      new_reference(subject = subject, number = ref_name)
    }), names = ref_names)
  } else {
    refs <- list()
  }
  re$reference_instances <- refs

  # load electrode data
  electrode_instances <- structure(lapply(electrodes, function(e) {
    re <- new_electrode(subject = subject, number = e)
    if(is.data.frame(ref_table) && nrow(ref_table)) {
      sel <- ref_table$Electrode == e
      if(length(sel) && any(sel)) {
        ref_name <- ref_table$Reference[sel][[1]]
        ref <- refs[[ref_name]]
        if(!is.null(ref)) {
          re$set_reference(reference = ref)
        }
      }
    }
    re
  }), names = sprintf("e_%d", electrodes))
  re$electrode_instances <- electrode_instances

  # obtain wavelet parameters
  wavelet_params <- subject$preprocess_settings$wavelet_params

  # get sample rates
  if(length(wavelet_params$downsample_to)) {
    wavelet_srate <- wavelet_params$downsample_to
  } else {
    wavelet_srate <- 100
  }

  voltage_srate <- subject$raw_sample_rates[subject$electrodes %in% electrodes]
  voltage_srate <- voltage_srate[[1]]


  # Prepare the cache paths
  cache_root_path <- file.path(cache_root(), subject$project_name,
                               subject$subject_code, "_whole_blocks")

  # prepare placeholder data
  block_data <- structure(lapply(blocks, function(block) {

    block_data <- dipsaus::fastmap2()

    voltage_ntps <- voltage_ntimepoints[[block]]
    voltage_filebase <- file.path(cache_root_path, block, "voltage")
    voltage_dnames <- list(
      Time = seq(0, by = 1 / voltage_srate, length.out = voltage_ntps),
      Electrode = subject$electrodes
    )
    voltage_signature <- dipsaus::digest(list(
      subject_id = subject$subject_id,
      reference_table = re$reference_table,
      all_electrodes = subject$electrodes,
      block_number = block,
      block_length = voltage_ntps,
      voltage_dnames = voltage_dnames,
      voltage_srate = voltage_srate
    ))

    voltage_array <- filearray::filearray_load_or_create(
      filebase = voltage_filebase,
      dimension = unname(vapply(voltage_dnames, length, 0L)),
      type = "float",
      symlink_ok = FALSE,
      mode = "readwrite",
      partition_size = 1L,
      signature = voltage_signature,
      on_missing = function(arr) {
        dimnames(arr) <- voltage_dnames
        arr
      }
    )

    block_data$voltage <- list(
      n_timepoints = voltage_ntps,
      filebase = voltage_filebase,
      dnames = voltage_dnames,
      signature = voltage_signature,
      sample_rate = voltage_srate,
      data = voltage_array
    )

    if(time_frequency) {
      wavelet_ntps <- wavelet_ntimepoints[[block]]
      wavelet_filebase <- file.path(cache_root_path, block, "wavelet")
      wavelet_dnames <- list(
        Frequency = wavelet_params$frequencies,
        Time = seq(0, by = 1 / wavelet_srate, length.out = wavelet_ntps),
        Electrode = subject$electrodes
      )
      wavelet_signature <- dipsaus::digest(list(
        subject_id = subject$subject_id,
        reference_table = re$reference_table,
        all_electrodes = subject$electrodes,
        block_number = block,
        wavelet_params = wavelet_params,
        block_length = wavelet_ntps,
        wavelet_dnames = wavelet_dnames,
        wavelet_srate = wavelet_srate
      ))

      wavelet_array <- filearray::filearray_load_or_create(
        filebase = wavelet_filebase,
        dimension = unname(vapply(wavelet_dnames, length, 0L)),
        type = "complex",
        symlink_ok = FALSE,
        mode = "readwrite",
        partition_size = 1L,
        signature = wavelet_signature,
        on_missing = function(arr) {
          dimnames(arr) <- wavelet_dnames
          arr
        }
      )

      block_data$wavelet <- list(
        n_timepoints = wavelet_ntps,
        filebase = wavelet_filebase,
        dnames = wavelet_dnames,
        signature = wavelet_signature,
        sample_rate = wavelet_srate,
        data = wavelet_array
      )
    }

    block_data

  }), names = blocks)



  subject_electrodes <- subject$electrodes

  # load data: calculate missing electrodes from the first block
  voltage_cached <- block_data[[1]]$voltage$data$get_header("cached_electrodes", default = NULL)
  voltage_more <- electrodes[!electrodes %in% voltage_cached]
  if(length(voltage_more)) {
    voltage_more <- lapply_async(
      electrode_instances[sprintf("e_%d", voltage_more)],
      function(inst) {
        s <- inst$load_blocks(blocks = blocks, type = "voltage", simplify = FALSE)
        for(block in blocks) {
          block_data[[block]]$voltage$data[, subject_electrodes == inst$number] <- s[[block]]
        }
        inst$number
      },
      callback = function(inst) {
        sprintf("Loading voltage|Electrode %d", inst$number)
      }
    )
    voltage_more <- unlist(voltage_more)
    voltage_cached <- unique(c(voltage_cached, voltage_more))
    for(block in blocks) {
      block_data[[block]]$voltage$data$set_header("cached_electrodes", voltage_cached, save = TRUE)
    }
  }

  if(time_frequency) {
    wavelet_cached <- block_data[[1]]$wavelet$data$get_header("cached_electrodes", default = NULL)
    wavelet_more <- electrodes[!electrodes %in% wavelet_cached]
    if(length(wavelet_more)) {
      wavelet_more <- lapply_async(
        electrode_instances[sprintf("e_%d", wavelet_more)],
        function(inst) {
          s <- inst$load_blocks(blocks = blocks, type = "wavelet-coefficient", simplify = FALSE)
          for(block in blocks) {
            block_data[[block]]$wavelet$data[, , subject_electrodes == inst$number] <- t(s[[block]])
          }
          inst$number
        },
        callback = function(inst) {
          sprintf("Loading time-frequency|Electrode %d", inst$number)
        }
      )
      wavelet_more <- unlist(wavelet_more)
      wavelet_cached <- unique(c(wavelet_cached, wavelet_more))
      for(block in blocks) {
        block_data[[block]]$wavelet$data$set_header("cached_electrodes", wavelet_cached, save = TRUE)
      }
    }
  }

  re$block_data <- block_data
  re$blocks <- blocks
  re$has_wavelet <- time_frequency

  digest_key <- list(
    subject_id = re$subject$subject_id,
    reference_table = re$reference_table,
    electrodes = re$electrode_list,
    signal_type = re$signal_type,
    blocks = re$blocks,
    time_frequency = time_frequency
  )
  digest_string <- dipsaus::digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  class(re) <- c("prepare_subject_with_blocks", "rave_prepare_subject", "rave_repository", "fastmap2", "list")
  re

}
