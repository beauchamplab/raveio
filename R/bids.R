# 'BIDS' file structure

analyze_bids_fname <- function(data_file){
  data_fname <- stringr::str_extract(data_file, '[^/\\\\]+$')

  parts <- stringr::str_split(data_fname, '_')[[1]]
  param <- list()

  fname <- parts[[length(parts)]]
  parts <- parts[-length(parts)]

  for(p in parts){
    p <- stringr::str_trim(stringr::str_split_fixed(p, '-', 2))
    param[[p[[1]]]] <- p[[2]]
    param$.names <- c(param$.names, p[[1]])
  }
  param$.filename <- fname
  param
}

bids_json <- function(data_file, bids_root){
  bids_root <- normalizePath(bids_root, mustWork = TRUE)
  data_file <- normalizePath(data_file, mustWork = TRUE)

  if(!dir.exists(bids_root)){ stop('bids_root must be a directory') }
  if(dir.exists(data_file)){ stop('data_file must be a file') }

  if(!stringr::str_detect(data_file, stringr::fixed(bids_root, ignore_case = TRUE))){
    stop('bids_json: data_file must be sub-directory of bids_root')
  }

  # parse file name
  param <- analyze_bids_fname(data_file)
  check_params <- function(fname, level = 0){
    p <- analyze_bids_fname(fname)
    for(nm in p$.names){
      if(!isTRUE(param[[nm]] == p[[nm]])){
        return(FALSE)
      }
    }
    return(TRUE)
  }

  # analyze parent directories
  current_dir <- dirname(data_file)
  max_levels <- length(param$.names)

  # find all related files in current directory
  related <- list.files(current_dir, include.dirs = FALSE, all.files = FALSE)
  related <- related[vapply(related, check_params, FALSE)]

  conf <- NULL
  while(!current_dir %in% c('/', '', bids_root) && max_levels >= 0){
    # look for json or tsv files
    jsons <- list.files(current_dir, pattern = '\\.json$', ignore.case = TRUE,
                        include.dirs = FALSE, all.files = FALSE)
    jsons <- jsons[vapply(jsons, check_params, FALSE)]
    conf <- c(file.path(current_dir, jsons), conf)
    current_dir <- dirname(current_dir)
    max_levels <- max_levels-1
  }

  settings <- dipsaus::fastmap2()
  for(f in conf){
    dat <- jsonlite::read_json(f)
    dipsaus::list_to_fastmap2(dat, settings)
  }

  list(
    related_files = related,
    settings = settings,
    parent = dirname(data_file),
    more = param
  )
}

#' Read in description files from 'BIDS-iEEG' format
#' @description Analyze file structures and import all \code{json} and
#' \code{tsv} files. File specification can be found at
#' \url{https://bids-specification.readthedocs.io/en/stable/}, chapter
#' "Modality specific files", section "Intracranial Electroencephalography"
#' (\doi{10.1038/s41597-019-0105-7}). Please note that this function has
#' very limited support on BIDS format.
#'
#' @param bids_root 'BIDS' root directory
#' @param project_name project folder name
#' @param subject_code subject code, do not include \code{"sub-"} prefix
#' @param folder folder name corresponding to 'iEEG' data. It's possible to
#' analyze other folders. However, by default, the function is designed for
#' \code{'ieeg'} folder.
#'
#' @returns A list containing the information below:
#' \item{subject_code}{character, removed leading \code{"sub-"}}
#' \item{project_name}{character, project name}
#' \item{has_session}{whether session/block names are indicated
#' by the file structure}
#' \item{session_names}{session/block names indicated by file
#' structure. If missing, then session name will be "default"}
#' \item{paths}{a list containing path information}
#' \item{stimuli_path}{stimuli path, not used for now}
#' \item{sessions}{
#' A named list containing meta information for each session/block. The
#' names of the list is task name, and the items corresponding to the
#' task contains events and channel information. Miscellaneous files
#' are stored in "others" variable.
#' }
#'
#' @examples
#'
#' # Download https://github.com/bids-standard/bids-examples/
#' # extract to directory ~/rave_data/bids_dir/
#'
#' bids_root <- '~/rave_data/bids_dir/'
#' project_name <- 'ieeg_visual'
#'
#' if(dir.exists(bids_root) &&
#'    dir.exists(file.path(bids_root, project_name, 'sub-01'))){
#'
#'   header <- load_bids_ieeg_header(bids_root, project_name, '01')
#'
#'   print(header)
#'
#'   # sessions
#'   names(header$sessions)
#'
#'   # electrodes
#'   head(header$sessions$`01`$spaces$unknown_space$table)
#'
#'   # visual task channel settings
#'   head(header$sessions$`01`$tasks$`01-visual-01`$channels)
#'
#'   # event table
#'   head(header$sessions$`01`$tasks$`01-visual-01`$channels)
#' }
#'
#' @export
load_bids_ieeg_header <- function(bids_root, project_name, subject_code, folder = 'ieeg'){

  # sub-<label>/
  #   [ses-<label>]/
  #     eeg/
  #       [sub-<label>[_ses-<label>][_(task|space|acq)-<label>][_run-<index>]_electrodes.tsv


  # bids_root <- "/Users/beauchamplab/Dropbox/projects/bids-examples"; folder = 'ieeg'
  # project_name <- 'ieeg_visual'
  # subject_code <- "sub-01"
  bids_root <- normalizePath(bids_root, mustWork = TRUE)

  paths <- dipsaus::fastmap2()
  header <- dipsaus::fastmap2()
  header$paths <- paths

  if(stringr::str_starts(subject_code, '^sub-')){
    subject_code <- stringr::str_remove(subject_code, '^sub-')
  }
  prefix <- 'sub-'


  project_path <- file.path(bids_root, project_name)
  subject_path <- file.path(bids_root, project_name, paste0(prefix, subject_code))

  if(!dir.exists(subject_path)){
    stop('Cannot find path to BIDS subject folder - [BIDS root] > ', project_name, ' > ', prefix, subject_code)
  }

  # check if session exists
  sess_name <- list.dirs(subject_path, recursive = FALSE, full.names = FALSE)
  sel <- stringr::str_starts(sess_name, '^ses-')
  if(any(sel)){
    has_session <- TRUE
    # session exists
    sess_name <- sess_name[sel]
    sess_folders <- file.path(subject_path, sess_name)
    sess_name <- stringr::str_remove(sess_name, '^ses-')
  } else {
    sess_name <- 'default'
    has_session <- FALSE
    sess_folders <- subject_path
  }

  ieeg_folders <- file.path(sess_folders, folder)
  has_ieeg <- dir.exists(ieeg_folders)
  ieeg_folders <- ieeg_folders[has_ieeg]
  sess_name <- sess_name[has_ieeg]
  sess_folders <- sess_folders[has_ieeg]

  stimuli_path <- file.path(project_path, 'stimuli')

  sess_folders <- structure(sess_folders, names = sess_name)
  ieeg_folders <- structure(ieeg_folders, names = sess_name)

  paths$bids_root <- bids_root
  header$project_name <- project_name
  header$subject_code <- subject_code

  paths$project_path <- project_path
  paths$subject_path <- subject_path
  paths$session_paths <- sess_folders
  header$session_names <- sess_name
  header$has_session <- has_session

  paths$ieeg_paths <- ieeg_folders
  header$stimuli_path <- stimuli_path

  # determine task and run names
  sessions <- dipsaus::fastmap2()
  header$sessions <- sessions
  lapply(sess_name, function(sn){
    sheader <- dipsaus::fastmap2()

    sessions[[sn]] <- sheader

    fd <- ieeg_folders[[sn]]
    fs <- list.files(fd)

    sheader$ieeg_folder <- fd
    sheader$all_files <- fs

    if(!length(fs)){ return() }
    #        sub-01       _ses-01             _task-visual                 _run-01            _ieeg         .json
    ptn <- '^sub-([^_-]*)(_ses-([^_-]*)){0,1}(_(task|space)-([^_-]*)){0,1}(_run-([^_-]*)){0,1}_([a-zA-Z]+)\\.([^.]+)$'
    regexp <- stringr::regex(ptn, ignore_case = TRUE)
    match_result <- stringr::str_match(fs, regexp)
    match_result <- match_result[!is.na(match_result[,1]), c(1,2,4,6,7,9,10,11)]
    colnames(match_result) <- c('filename', 'subject', 'session', 'class', 'name', 'run', 'type', 'ext')
    match_result <- as.data.frame(match_result)

    sheader$match_result <- match_result

    # get tasks
    is_task <- match_result$class == 'task'
    is_task[is.na(is_task)] <- FALSE
    task_name <- match_result[is_task,'name']
    runs <- match_result[is_task,'run']

    block_names <- paste(sn, task_name, runs, sep = '-')

    sheader$block_names <- unique(block_names)
    sheader$task_names <- unique(task_name)
    sheader$runs <- unique(runs)

    sfnames <- stringr::str_to_lower(paste0(match_result$type, '.', match_result$ext))
    is_electsv <- ((sfnames == 'electrodes.tsv') & !is_task)

    # load electrodes
    spaces <- dipsaus::fastmap2()
    sheader$spaces <- spaces
    if(sum(is_electsv)){
      f <- match_result$filename[is_electsv][[1]]
      for(electrode_file in f){
        fname <- file.path(fd, electrode_file)
        e <- bids_json(fname, bids_root)
        e$more$space %?<-% 'unknown_space'
        space <- e$more$space
        elec <- utils::read.table(fname, sep = '\t', header = TRUE)
        e$table <- elec
        sheader$space_names <- unique(c(sheader$space_names, space))
        spaces[[space]] <- e
      }
      sheader$electrodes <- elec
    }

    # load task files
    tasks <- dipsaus::fastmap2()
    sheader$tasks <- tasks
    match_result <- match_result[is_task, ]

    for(bname in unique(block_names)){
      task <- dipsaus::fastmap2()
      tasks[[bname]] <- task
      sel <- block_names == bname
      sub <- match_result[sel, ]
      task$run <- unique(sub$run)[[1]]
      task$name <- unique(sub$name)[[1]]
      task$files <- sub$filename
      # match_result$type is ieeg, channels or events
      # read events
      types <- paste(sub$type, sub$ext, sep = '.')
      types <- stringr::str_to_lower(types)

      # ieeg.json
      is_desc <- types == 'ieeg.json'
      f <- sub$filename[is_desc][[1]]
      common <- jsonlite::read_json(file.path(fd, f))
      common <- dipsaus::list_to_fastmap2(common)
      task$common <- common

      is_events <- types == 'events.tsv'
      if(isTRUE(any(is_events))){
        f <- sub$filename[is_events][[1]]
        events <- utils::read.table(file = file.path(fd, f), sep = '\t', header = TRUE)
        task$events <- events
      }
      is_chan_tbl <- types == 'channels.tsv'
      if(isTRUE(any(is_chan_tbl))){
        f <- sub$filename[is_chan_tbl][[1]]
        channels <- utils::read.table(file = file.path(fd, f), sep = '\t', header = TRUE)
        task$channels <- channels
      }

      # look for header/data file
      sub <- sub[!is_chan_tbl & !is_events & !is_desc, ]

      ext <- stringr::str_to_lower(sub$ext)
      task$header_file <- sub$filename[ext %in% c('vhdr', 'edf', 'nwb', 'mef', 'set')]


    }

    NULL

  })
#
#   # For each session blocks, read in .json files
#   sessions <- dipsaus::fastmap2()
#   header$sessions <- sessions
#   lapply(seq_along(sess_name), function(ii){
#     sn <- sess_name[[ii]]
#     sheader <- dipsaus::fastmap2()
#     sessions[[sn]] <- sheader
#     sheader[['tasks']] <- dipsaus::fastmap2()
#     sheader[['others']] <- dipsaus::fastmap2()
#
#     spath <- ieeg_folders[[ii]]
#     if(has_session){
#       tmp <- sprintf('ses-%s_', sn)
#     } else {
#       tmp <- ''
#     }
#
#
#     # look for all json files
#     task_json <-
#       list.files(spath, ignore.case = TRUE, recursive = FALSE,
#                  pattern = sprintf('^sub-%s_%stask-(.*)_%s\\.json$', subject_code, tmp, folder))
#
#     for(task_file in task_json){
#       task_data <- dipsaus::fastmap2()
#       task_header <- jsonlite::read_json(file.path(spath, task_file))
#       task_header <- dipsaus::list_to_fastmap2(task_header)
#       sheader[['tasks']][[task_header$TaskName]] <- list(
#         header = task_header,
#         tables = task_data
#       )
#
#       pattern <- stringr::str_replace(task_file, sprintf('%s\\.json$', folder), '(.*)\\.tsv')
#       tsv_files <- list.files(spath, ignore.case = TRUE, recursive = FALSE, pattern = pattern)
#       for(tsv_file in tsv_files){
#         content_name <- stringr::str_match(tsv_file, pattern)[,2]
#         content <- utils::read.table(file = file.path(spath, tsv_file), sep = '\t', header = TRUE)
#         task_data[[content_name]] <- content
#       }
#     }
#
#     # other json, space or acq
#     other_json <- list.files(spath, pattern = '\\.json$', ignore.case = TRUE, recursive = FALSE)
#     other_json <- other_json[!other_json %in% task_json]
#
#     for(other_file in other_json){
#       name <- stringr::str_match(stringr::str_to_lower(other_file), '([^_]+)\\.json$')[,2]
#       content <- jsonlite::read_json(file.path(spath, other_file))
#       content <- dipsaus::list_to_fastmap2(content)
#       sheader[['others']][[name]] <- content
#     }
#
#     # other tsv files
#     task_tsv <-
#       list.files(spath, ignore.case = TRUE, recursive = FALSE,
#                  pattern = sprintf('^sub-%s_%stask-(.*)\\.tsv$', subject_code, tmp))
#     other_tsv <- list.files(spath, pattern = '\\.tsv', ignore.case = TRUE, recursive = FALSE)
#     other_tsv <- other_tsv[!other_tsv %in% task_tsv]
#
#     for(tsv_file in other_tsv){
#       name <- stringr::str_match(stringr::str_to_lower(tsv_file), '([^_]+)\\.tsv$')[,2]
#       content <- utils::read.table(file = file.path(spath, tsv_file), sep = '\t', header = TRUE)
#       sheader[['others']][[name]] <- content
#     }
#
#   })


  header
}
