# raw data files to rave

# Guess the name of raw traces from a named list
guess_raw_trace <- function(dat, electrodes = NULL, is_vector = TRUE){
  nms <- names(dat)
  for(nm in nms){
    x <- dat[[nm]]
    if(!is.numeric(x) || mode(x) != "numeric"){ next }

    if(is_vector){
      # should be vector
      dm <- dim(x)
      if(is.null(dm)){
        return(nm)
      } else if (length(dm) %in% c(1,2)){
        if(min(dm) == 1){
          return(nm)
        }
      }
    } else {
      if(!is.matrix(x)){ next }
      dm <- dim(x)
      d1 <- min(dm)

      # d2 is the time points, d1 should be electrodes
      if(d1 < max(electrodes, 1)){ next }
      return(nm)
    }
  }
  return(NULL)
}

#' Validate raw files in 'rave' directory
#' @name rave-raw-validation
#' @description Validate subjects and returns whether the subject can be
#' imported into 'rave'
#' @param subject_code subject code, direct folder under 'rave' raw data path
#' @param blocks block character, direct folder under subject folder. For raw
#' files following 'BIDS' convention, see details
#' @param electrodes electrodes to verify
#' @param format integer or character. For characters, run
#' \code{names(IMPORT_FORMATS)}
#' @param data_type currently only support continuous type of signals
#' @param ... other parameters used if validating \code{'BIDS'} format; see
#' details.
#'
#' @return logical true or false whether the directory is valid. Attributes
#' containing error reasons or snapshot of the data. The attributes might be:
#' \item{\code{snapshot}}{description of data found if passing the
#' validation}
#' \item{\code{valid_run_names}}{For 'BIDS' format, valid
#' \code{session+task+run} name if passing the validation}
#' \item{\code{reason}}{named list where the names are the
#' reason why validation fails and values are corresponding sessions
#' or electrodes or both.}
#' @details
#' Six types of raw file structures are supported. They can be basically
#' classified into two categories: 'rave' native raw structure and
#' 'BIDS-iEEG' structure.
#'
#' In 'rave' native structure, subject folders
#' are stored within the root directory, which can be obtained via
#' \code{raveio_getopt('raw_data_dir')}. Subject directory is the subject code.
#' Inside of subject folder are block files. In 'rave', term 'block'
#' is the combination of session, task, and run. Within each block, there
#' should be 'iEEG' data files.
#'
#' In 'BIDS-iEEG' format, the root directory can be obtained via
#' \code{raveio_getopt('bids_data_dir')}. 'BIDS' root folder contains
#' project folders. This is unlike 'rave' native raw data format.
#' Subject folders are stored within the project directories.
#' The subject folders start with \code{'sub-'}. Within subject
#' folder, there are session folders with prefix \code{'ses-'}. Session
#' folders are optional. 'iEEG' data is stored in \code{'ieeg'} folder under
#' the session/subject folder. \code{'ieeg'} folder should contain at least
#' \describe{
#' \item{electrodes.tsv}{
#'   \code{sub-<label>*_electrodes.tsv}
#' }
#' \item{'iEEG' description}{
#'   \code{sub-<label>*_task-<label>_run-<index>_ieeg.json}
#' }
#' \item{'iEEG' data file}{
#'   \code{sub-<label>*_task-<label>_run-<index>_ieeg.<ext>}, in current
#'   'rave', only extensions \code{'.vhdr+.eeg/.dat'} ('BrainVision') or 'EDF'
#'   (or plus) are supported.
#' }
#' }
#' When format is 'BIDS', \code{project_name} must be specified.
#'
#' The following formats are supported:
#' \describe{
#' \item{\code{'.mat/.h5 file per electrode per block'}}{
#'   'rave' native raw format, each block folder contains multiple
#'   'Matlab' or 'HDF5' files. Each file corresponds to a channel/electrode.
#'   File names should follow \code{'xxx001.mat'} or \code{'xxx001.h5'}. The
#'   numbers before the extension are channel numbers.
#' }
#' \item{\code{'Single .mat/.h5 file per block'}}{
#'   'rave' native raw format, each block folder contains \strong{only} one
#'   'Matlab' or 'HDF5' file. The file name can be arbitrary, but extension
#'   must be either \code{'.mat'} or \code{'.h5'}. Within the file there should
#'   be a matrix containing all the data. The short dimension of the matrix
#'   will be channels, and larger side of the dimension corresponds to the
#'   time points.
#' }
#' \item{\code{'Single EDF(+) file per block'}}{
#'   'rave' native raw format, each block folder contains \strong{only} one
#'   \code{'.edf'} file.
#' }
#' \item{\code{'Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block'}}{
#'   'rave' native raw format, each block folder contains \strong{only} two
#'   files. The first file is header \code{'.vhdr'} file. It contains
#'   all meta information. The second is either \code{'.eeg'} or \code{'.dat'}
#'   file containing the body, i.e. signal entries.
#' }
#' \item{\code{'BIDS & EDF(+)'}}{
#'   'BIDS' format. The data file should have \code{'.edf'} extension
#' }
#' \item{\code{'BIDS & BrainVision (.vhdr+.eeg, .vhdr+.dat)'}}{
#'   'BIDS' format. The data file should have \code{'.vhdr'+'.eeg/.dat'}
#'   extensions
#' }
#' }
#'
NULL

#' @rdname rave-raw-validation
#' @export
validate_raw_file <- function(subject_code, blocks, electrodes, format, data_type = c('continuous'), ...){
  data_type <- match.arg(data_type)
  fname <- list(
    'continuous' = 'validate_raw_file_lfp'
  )[[data_type]]

  do.call(fname, list(
    subject_code = subject_code, blocks = blocks, electrodes = electrodes,
    format = format, data_type = data_type, ...
  ))
}

validation_failure <- local({
  reasons <- dipsaus::fastmap2()
  function(..., .reasons = NULL, .add = FALSE, .reset = FALSE){
    if(.reset || !.add){
      .subset2(reasons, 'reset')()
    }
    for(nm in names(.reasons)){
      reasons[[nm]] <- unique(c(reasons[[nm]], .reasons[[nm]]))
    }
    .reasons <- list(...)
    for(nm in names(.reasons)){
      reasons[[nm]] <- unique(c(reasons[[nm]], .reasons[[nm]]))
    }
    re <- as.list(reasons)
    re <- re[vapply(re, function(x){ length(x) > 0 }, FALSE)]
    if(!length(re)){ re <- NULL }
    structure(FALSE, reason = re, class = 'validate_failure')
  }
})

#' @rdname rave-raw-validation
#' @export
IMPORT_FORMATS <- list(
  '.mat/.h5 file per electrode per block' = 'native_matlab',
  'Single .mat/.h5 file per block' = 'native_matlab2',
  'Single EDF(+) file per block' = 'native_edf',
  'Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block' = 'native_brainvis',
  'BIDS & EDF(+)' = 'bids_edf',
  'BIDS & BrainVision (.vhdr+.eeg, .vhdr+.dat)' = 'bids_brainvis'
)

validate_raw_file_lfp <- function(subject_code, blocks, electrodes, format, check_content = TRUE, ...){
  m <- IMPORT_FORMATS[[format]]
  if(is.null(m)){
    return(structure(FALSE, reason = list(
      'Unknown format' = character(0)
    ), class = 'validate_failure'))
  } else {
    UseMethod('validate_raw_file_lfp', structure(subject_code, class = m))
  }
}

# ------------------------- Validators - LFP ------------------------------

validate_raw_file_lfp.native_matlab <- function(
  subject_code, blocks, electrodes, check_content = TRUE, ...){

  raw_root <- raveio_getopt('raw_data_dir')
  block_paths <- file.path(raw_root, subject_code, blocks)
  if(!all(dir.exists(block_paths))){
    return(validation_failure(
      'One or more block folder is missing.' = blocks[!dir.exists(block_paths)]
    ))
  }

  if(missing(electrodes)){
    electrodes <- NULL
  }
  snapshot <- NULL

  finfo <- dipsaus::fastmap2()
  validation_failure(.reset = TRUE)
  for(b in blocks){
    bpath <- file.path(raw_root, subject_code, b)
    files <- list.files(bpath, pattern = '[0-9]+\\.(mat|h5)$', ignore.case = TRUE)
    if(!length(files)){
      validation_failure('Cannot find any mat/h5 file' = sprintf('Block %s', b), .add = TRUE)
      next
    }
    finfo[[b]] <- list(
      path = bpath,
      files = files
    )
    if(length(electrodes)){
      # also check electrodes
      number <- stringr::str_match(files, '([0-9]+)\\.(mat|h5)$')[,2]
      number <- as.integer(number)
      number <- number[!is.na(number)]
      if(!all(electrodes %in% number)){
        melc <- electrodes[!electrodes %in% number]
        melc <- dipsaus::deparse_svec(melc)
        validation_failure('Electrode file missing' = sprintf('%s (electrode %s)', b, melc), .add = TRUE)
      }
    }
  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  # if check_content, open all files to check length
  if( check_content && length(electrodes) ){
    progress <- dipsaus::progress2('Check electrode files within block', shiny_auto_close = TRUE, max = length(blocks) + 1)
    for(b in blocks){
      progress$inc(b)
      info <- finfo[[b]]
      files <- info$files
      number <- stringr::str_match(files, '([0-9]+)\\.(mat|h5)$')[,2]
      sel <- number %in% as.character(electrodes)
      elec_bak <- as.integer(number[sel])
      files <- files[sel]
      abspaths <- file.path(info$path, files)
      dipsaus::make_forked_clusters(workers = max(raveio_getopt('max_worker'), 1))
      dlen <- lapply(abspaths, function(path){
        tryCatch({
          dl <- NA
          dat <- read_mat(path)
          nm <- guess_raw_trace(dat = dat, electrodes = electrodes, is_vector = TRUE)
          if(length(nm)){
            dl <- length(dat[[nm[[1]]]])
          }
          dl
        }, error = function(e){
          NA
        })
      })
      dlen <- unlist(dlen)
      if(any(is.na(dlen)) || length(unique(dlen)) > 1){
        mis_d <- elec_bak[is.na(dlen)]
        if(length(mis_d)){
          validation_failure(
            'Electrode data lengths are not consistent or missing' =
              sprintf('Files for electrode %s are broken in block %s', dipsaus::deparse_svec(mis_d), b),
            .add = TRUE
          )
        }
        tbl <- sort(table(dlen, useNA = 'no'), decreasing = TRUE)
        if(length(tbl) > 1){
          lths <- as.integer(names(tbl))
          ssel <- dlen %in% lths[-1]
          if(any(ssel)){
            validation_failure(
              'Electrode data point lengths are inconsistent' =
                sprintf('electrode(s) %s in block %s', dipsaus::deparse_svec(elec_bak[ssel]), b),
              .add = TRUE
            )
          }
        }
      } else if(is.null(snapshot)){
        # provide an snapshot
        path <- abspaths[[1]]
        dat <- read_mat(path)
        nms <- names(dat)
        for(nm in nms){
          x <- dat[[nm]]
          if(!is.numeric(x)){ next }
          dm <- dim(x)
          if(length(dm) > 2){ next }
          if(length(dm) == 2 && all(dm > 1)){ next }
          len <- length(x)
          if(len < 100){ next }
          if(is.null(snapshot)){
            snapshot <- sprintf('A single vector of length <strong>%d</strong> (data points)', len)
          }
          rm(x)
        }
        NA
        rm(dat)
      }
    }
  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  return(structure(TRUE, info = finfo, snapshot = snapshot, class = 'validate_success'))
}

validate_raw_file_lfp.native_matlab2 <- function(
  subject_code, blocks, electrodes, check_content = TRUE, ...){
  raw_root <- raveio_getopt('raw_data_dir')
  block_paths <- file.path(raw_root, subject_code, blocks)
  if(!all(dir.exists(block_paths))){
    return(validation_failure(
      'One or more block folder is missing.' = blocks[!dir.exists(block_paths)]
    ))
  }

  if(missing(electrodes)){ electrodes <- NULL }
  snapshot <- NULL
  finfo <- dipsaus::fastmap2()
  validation_failure(.reset = TRUE)

  for(b in blocks){
    bpath <- file.path(raw_root, subject_code, b)
    files <- list.files(bpath, pattern = '\\.(mat|h5)$', ignore.case = TRUE)
    if(!length(files)){
      validation_failure('Cannot find any mat/h5 file' = paste('Block', b), .add = TRUE)
      next
    } else if(length(files) > 1){
      validation_failure('Block contains too many mat/h5 files. Unable to decide which one is the data file. Please either reduce files to one, or try different importing format.' = paste('Block', b), .add = TRUE)
      next
    }
    finfo[[b]] <- list( path = bpath, files = files[[1]])
  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  if( check_content && length(electrodes) ){
    progress <- dipsaus::progress2('Check electrode files within block', shiny_auto_close = TRUE, max = length(blocks) + 1)
    # Need to check content to see whether data is valid
    for(b in blocks){
      progress$inc(b)
      info <- finfo[[b]]
      files <- info$files[[1]]
      abspath <- file.path(info$path, files)
      tryCatch({
        dat <- read_mat(abspath)
        nm <- guess_raw_trace(dat = dat, electrodes = electrodes, is_vector = FALSE)

        if(length(nm) > 1){
          validation_failure(
            .add = TRUE,
            'Block file contains more than one dataset.' = paste('Block', b)
          )
        } else if(length(nm) == 0){
          validation_failure(
            .add = TRUE,
            'Block file contains no dataset.' = paste('Block', b)
          )
        } else{
          nm <- nm[[1]]
          dim <- dim(dat[[nm]])
          # Assume min dim is electrodes as time is usually large
          max_elec <- min(dim)
          mis_e <- electrodes[!electrodes %in% seq_len(max_elec)]

          if(length(mis_e)){
            validation_failure(
              .add = TRUE,
              'Electrode(s) missing' = sprintf('Found matrix (size: %dx%d) in block %s. Electrode %s are missing (available electrodes: 1-%d)',
                                               dim[1], dim[2], b, dipsaus::deparse_svec(mis_e), max_elec)
            )
          } else {
            if(is.null(snapshot)){
              snapshot <- sprintf(
                'Variable name is %s, a matrix: <strong>%d</strong> available electrodes, <strong>%d</strong> time points.',
                sQuote(nm), max_elec, max(dim)
              )
            }
          }

        }

      }, error = function(e){
        validation_failure(
          .add = TRUE,
          'Block file is broken' = paste('Block', b)
        )
      })
    }

  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  return(structure(TRUE, info = finfo, snapshot = snapshot, class = 'validate_success'))
}

validate_raw_file_lfp.native_edf <- function(
  subject_code, blocks, electrodes, check_content = TRUE, ...){

  raw_root <- raveio_getopt('raw_data_dir')
  block_paths <- file.path(raw_root, subject_code, blocks)
  if(!all(dir.exists(block_paths))){
    return(validation_failure('One or more block folder is missing.' = blocks[!dir.exists(block_paths)]))
  }

  if(missing(electrodes)){
    electrodes <- NULL
  }
  snapshot <- NULL
  finfo <- dipsaus::fastmap2()
  validation_failure(.reset = TRUE)

  for(b in blocks){
    bpath <- file.path(raw_root, subject_code, b)
    files <- list.files(bpath, pattern = '\\.(edf)$', ignore.case = TRUE)
    if(!length(files)){
      validation_failure(
        .add = TRUE,
        'Cannot find any EDF file' = paste('Block', b)
      )
      next
    } else if(length(files) > 1){
      validation_failure(
        .add = TRUE,
        'Found more than one EDF in the following block. Please reduce number of EDF files to 1 per block' = paste('Block', b)
      )
      next
    }
    finfo[[b]] <- list( path = bpath, files = files[[1]])
  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  if( check_content && length(electrodes) ){

    srates <- lapply(blocks, function(b){
      info <- finfo[[b]]
      edf_path <- file.path(info$path, info$files)
      header <- tryCatch({read_edf_header(edf_path)}, error = dipsaus::do_nothing)
      finfo[[b]]$header <- header
      if(!length(header)){
        validation_failure(
          .add = TRUE,
          'Failed to open EDF file' =
            sprintf('Block %s, file %s', b, sQuote(info$files))
        )
        return(NA)
      }

      has_elec <- electrodes %in% which(!header$isAnnot2)
      if(!all(has_elec)){
        which_elec <- dipsaus::deparse_svec(which(!has_elec))
        validation_failure(
          .add = TRUE,
          'Electrode(s) not found in EDF file' =
            sprintf('Block %s, electrode(s) %s', b, which_elec)
        )
        return(NA)
      }
      srate <- unique(header$sampleRate2[!is.na(header$sampleRate2)])
      if(length(srate) >= 1){
        return(srate[[1]])
      }else{
        NA
      }
    })

    srates <- unlist(srates)

    if(any(is.na(srates)) || length(unique(srates)) > 1){
      validation_failure(
        .add = TRUE,
        'Found different sample rates across sessions' =
          sprintf('%s, sample rate - %.8g', blocks, srates)
      )
    } else {
      info <- finfo[[blocks[[1]]]]
      header <- info$header
      units <- header$unit2
      units <- units[!is.na(units)]
      units <- unique(units)
      snapshot <- sprintf('EDF format recorded sample rate is <strong>%.8g</strong>, and %d physical units found: <strong>%s</strong>',
                         srates[[1]], length(units), paste(units, collapse = ', '))
    }

  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  return(structure(TRUE, info = finfo, snapshot = snapshot, class = 'validate_success'))
}


validate_raw_file_lfp.native_brainvis <- function(
  subject_code, blocks, electrodes, check_content = TRUE, ...){

  raw_root <- raveio_getopt('raw_data_dir')
  block_paths <- file.path(raw_root, subject_code, blocks)
  if(!all(dir.exists(block_paths))){
    return(validation_failure('One or more block folder is missing.' = blocks[!dir.exists(block_paths)]))
  }

  if(missing(electrodes)){
    electrodes <- NULL
  }
  snapshot <- NULL
  finfo <- dipsaus::fastmap2()
  validation_failure(.reset = TRUE)

  for(b in blocks){
    bpath <- file.path(raw_root, subject_code, b)
    files <- list.files(bpath, pattern = '\\.(vhdr)$', ignore.case = TRUE)
    if(!length(files)){
      validation_failure(
        .add = TRUE,
        'Cannot find any EDF file' = paste('Block', b)
      )
      next
    } else if(length(files) > 1){
      validation_failure(
        .add = TRUE,
        'Found more than one .vhdr file in the following block. Please reduce to 1 file per block' = paste('Block', b)
      )
      next
    }
    finfo[[b]] <- list( path = bpath, files = files[[1]])
  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  if( check_content && length(electrodes) ){

    srates <- lapply(blocks, function(b){
      info <- finfo[[b]]
      path <- file.path(info$path, info$files)
      header <- tryCatch({read_eeg_header(path)}, error = dipsaus::do_nothing)
      finfo[[b]]$header <- header
      if(!length(header)){
        validation_failure(
          .add = TRUE,
          'Failed to open header file' =
            sprintf('Block %s, file %s', b, sQuote(info$files))
        )
        return(NA)
      }

      has_elec <- electrodes %in% seq_len(header$channel_counts)
      if(!all(has_elec)){
        which_elec <- dipsaus::deparse_svec(which(!has_elec))
        validation_failure(
          .add = TRUE,
          'Electrode(s) not found in the header file' =
            sprintf('Block %s, electrode(s) %s', b, which_elec)
        )
        return(NA)
      }

      header$sample_rate
    })

    # get validation failure messages
    failed <- validation_failure(.add = TRUE)
    if(length(attr(failed, 'reason'))){ return(failed) }

    srates <- unlist(srates)

    if(any(is.na(srates)) || length(unique(srates)) > 1){
      validation_failure(
        .add = TRUE,
        'Found different sample rates across sessions' =
          sprintf('%s, sample rate - %.8g', blocks, srates)
      )
    } else {


      data_files <- lapply(blocks, function(b){
        info <- finfo[[b]]
        df <- info$header$common$DataFile
        if(!length(df)){ df <- NA }
        if(!isTRUE(file.exists(file.path(info$path, df)))){
          return(sprintf('Block %s, file %s', b, sQuote(df)))
        }
        NULL
      })
      data_files <- unlist(data_files)

      if(length(data_files)){
        validation_failure(
          .add = TRUE,
          'Data file indicated by header file(s) not found' =
            data_files
        )
      } else {
        info <- finfo[[blocks[[1]]]]
        header <- info$header
        units <- header$channels$unit
        units <- units[!is.na(units)]
        units <- unique(units)

        snapshot <- sprintf('BrainVision format recorded sample rate is <strong>%.8g</strong>, and %d physical units found: <strong>%s</strong>',
                           srates[[1]], length(units), paste(units, collapse = ', '))
      }

    }

  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  return(structure(TRUE, info = finfo, snapshot = snapshot, class = 'validate_success'))
}


validate_raw_file_lfp.bids <- function(
  subject_code, blocks, electrodes, check_content = TRUE,
  supported_data_format = c('.edf', '.vhdr', '.set', '.nwb', '.mef'), ...){

  # project_name = 'ieeg_visual'
  # subject_code = '01'
  project_name <- list(...)[['project_name']]
  if(length(project_name) != 1){
    project_name <- '.'
    warning('project_name is missing, use BIDS root directory as project name')
  }

  raw_root <- raveio_getopt('bids_data_dir')
  if(is.na(raw_root) || !dir.exists(raw_root)){
    raw_root <- raveio_getopt('raw_data_dir')
  }


  # analyze BIDS format
  bids_header <- tryCatch({
    load_bids_ieeg_header(raw_root, project_name, subject_code)
  }, error = function(e){
    e$message
  })

  if(!inherits(bids_header, 'fastmap2')){
    return(validation_failure('Cannot parse BIDS structure for subject' = sprintf('Reason: %s', bids_header)))
  }

  missing_blocks <- blocks[!blocks %in% sprintf('ses-%s', bids_header$session_names)]
  if(length(missing_blocks)){
    return(validation_failure('One or more block folder is missing or contain no ieeg folder.' = missing_blocks))
  }

  if(missing(electrodes)){
    electrodes <- NULL
  }
  snapshot <- NULL
  finfo <- dipsaus::fastmap2()
  validation_failure(.reset = TRUE)

  for(b in blocks){
    b <- stringr::str_remove(b, '^ses-')
    sess_header <- bids_header$sessions[[b]]

    # check tasks
    block_names <- sess_header$block_names

    if(length(block_names) == 0){
      validation_failure(
        .add = TRUE,
        'No task/run found in block' = b
      )
      next
    }

    if(!is.data.frame(sess_header$electrodes)){
      validation_failure(
        .add = TRUE,
        'Cannot find *_electrodes.tsv in session' = b
      )
      next
    }

    nelec <- nrow(sess_header$electrodes)
    mis_e <- electrodes[!electrodes %in% seq_len(nelec)]
    if(length(mis_e)){
      validation_failure(
        .add = TRUE,
        'Missing electrodes' = sprintf('%s, electrode(s) %s', b, dipsaus::deparse_svec(mis_e))
      )
      next
    }
  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  # Check whether electrodes exist in all runs and get run names
  run_names <- dipsaus::fastmap2()

  lapply(blocks, function(b){
    b <- stringr::str_remove(b, '^ses-')
    info <- bids_header$sessions[[b]]

    enames <- info$space_names
    if(length(enames)){
      elec_tbl <- info$spaces[[enames[[1]]]]$table
      elec_names <- elec_tbl$name[electrodes]
    } else {
      elec_names <- NULL
    }


    for(tn in info$block_names){
      task <- info$tasks[[tn]]
      srate <- c(task$common$SamplingFrequency, NA)[[1]]
      run_names$srates <- c(run_names$srates, srate)
      if(!length(task$header_file)){
        run_names$miss_header <- c(run_names$miss_header, tn)
        next
      } else {
        # check extension
        if(!any(stringr::str_extract(task$header_file, '\\.[^.]+$') %in% supported_data_format)){
          run_names$miss_header <- c(run_names$miss_header, tn)
          next
        }
      }
      if(length(elec_names) && any(!elec_names %in% task$channels$name)){
        run_names$miss_channel <- c(run_names$miss_channel, tn)
        next
      }
      run_names$valid <- c(run_names$valid, tn)
      # block
      finfo[[tn]] <- list(
        path = info$ieeg_folder,
        files = task$header_file
      )
    }
    NULL
  })

  nvalid <- length(run_names$valid)
  nmismatch <- length(run_names$miss_channel)
  nmisheader <- length(run_names$miss_header)
  if(!nvalid){
    if(nmismatch){
      validation_failure(
        .add = TRUE,
        'Electrode names in *_electrodes.tsv mismatch with *_channels.tsv in the following runs' =
          run_names$miss_channel
      )
    }
    if(nmisheader){
      validation_failure(
        .add = TRUE,
        .reasons = structure(
          list(run_names$miss_header),
          names = sprintf(
            'No data file found (supported extensions: %s) in the following runs',
            paste(supported_data_format, collapse = ', ')
          )
        )
      )
    }
  }

  # get validation failure messages
  failed <- validation_failure(.add = TRUE)
  if(length(attr(failed, 'reason'))){ return(failed) }

  srates <- unique(run_names$srates)

  snapshot <- sprintf(paste(
    'Total %d combinations of session+task+run found, in which %d have mismatch electrode names,',
    '%d have missing supported data files.',
    '%d unique sample rate(s) found: %s'
  ), nmisheader + nvalid + nmismatch, nmismatch, nmisheader, length(srates), paste(srates, 'Hz', collapse = ', '))

  valid_run_names <- run_names$valid

  return(structure(TRUE, info = finfo, snapshot = snapshot, valid_run_names = valid_run_names, class = 'validate_success'))
}


validate_raw_file_lfp.bids_edf <- function(
  subject_code, blocks, electrodes, check_content = TRUE, ...
) {
  validate_raw_file_lfp.bids(subject_code, blocks, electrodes, check_content,
                             ..., supported_data_format = '.edf')
}

validate_raw_file_lfp.bids_brainvis <- function(
  subject_code, blocks, electrodes, check_content = TRUE, ...
) {
  validate_raw_file_lfp.bids(subject_code, blocks, electrodes, check_content,
                             ..., supported_data_format = '.vhdr')
}


# # get validation failure messages
# failed = validation_failure(.add = TRUE)
# if(length(attr(failed, 'reason'))){ return(failed) }





