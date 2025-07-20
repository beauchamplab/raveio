#' @title Archive and share a subject
#' @param subject 'RAVE' subject to archive
#' @param path path to a zip file to store; if missing or empty,
#' then the path will be automatically created
#' @param includes data to include in the archive; default includes all (
#' original raw signals, processed signals, imaging files, stored pipelines,
#' notes, and user-generated exports)
#' @param config a list of configurations, including changing subject code,
#' project name, or to exclude cache data; see examples
#' @param work_path temporary working path where files are copied; default is
#' temporary path. Set this variable explicitly when temporary path is
#' on external drives (for example, users have limited storage on local
#' drives and cannot hold the entire subject)
#' @param zip_flags \code{\link[utils]{zip}} flags
#'
#' @examples
#'
#' # This example requires you to install demo subject
#'
#' \dontrun{
#'
#'
#' # Basic usage
#' path <- archive_subject('demo/DemoSubject')
#'
#' # clean up
#' unlink(path)
#'
#' # Advanced usage: include all the original signals
#' # and processed data, no cache data, re-name to
#' # demo/DemoSubjectLite
#' path <- archive_subject(
#'   'demo/DemoSubject',
#'   includes = c("orignal_signals", "processed_data"),
#'   config = list(
#'     rename = list(
#'       project_name = "demo",
#'       subject_code = "DemoSubjectLite"
#'     ),
#'     orignal_signals = list(
#'       # include all raw signals
#'       include_all = TRUE
#'     ),
#'     processed_data = list(
#'       include_cache = FALSE
#'     )
#'   )
#' )
#'
#' # Clean up temporary zip file
#' unlink(path)
#'
#' }
#'
#'
#' @export
archive_subject <- function(
    subject, path,
    includes = c("orignal_signals", "processed_data", "rave_imaging",
                 "pipelines", "notes", "user_generated"),
    config = list(),
    work_path = NULL,
    zip_flags = NULL
) {

  # DIPSAUS DEBUG START
  # subject <- "devel/PAV014"
  # includes = c("orignal_signals", "processed_data", "rave_imaging", "pipelines", "notes")
  # config = list()
  subject <- restore_subject_instance(subject, strict = FALSE)
  includes <- includes[includes %in% c("orignal_signals", "processed_data", "rave_imaging", "pipelines", "notes", "user_generated")]

  # parse user configuration
  project_name <- subject$project_name
  subject_code <- subject$subject_code
  rename <- FALSE
  config <- as.list(config)
  if(is.list(config$rename)) {
    new_project_name <- config$rename$project_name
    new_subject_code <- config$rename$subject_code

    if( is.character(new_project_name) && length(new_project_name) == 1 && !is.na(new_project_name) &&
        nzchar(new_project_name) && grepl("^[a-zA-Z][a-zA-Z0-9_-]{0,}$", new_project_name)) {
      project_name <- new_project_name
      rename <- TRUE
    }

    if( is.character(new_subject_code) && length(new_subject_code) == 1 && !is.na(new_subject_code) &&
        nzchar(new_subject_code) && grepl("^[a-zA-Z][a-zA-Z0-9_-]{0,}$", new_subject_code)) {
      subject_code <- new_subject_code
      rename <- TRUE
    }
  }

  if(rename) {
    config$rename <- list(
      project_name = project_name,
      subject_code = subject_code
    )
  } else {
    config$rename <- FALSE
  }

  # check if all raw folders should be included
  include_all_raw <- FALSE
  if(is.list(config$orignal_signals)) {
    include_all_raw <- isTRUE(config$orignal_signals$include_all)
  }

  # check if cache is allowed
  include_cache <- TRUE
  if( is.list(config$processed_data) ) {
    include_cache <- !isFALSE(config$processed_data$include_cache)
  }


  if(length(work_path) != 1 || !is.character(work_path) || is.na(work_path) ||
     !dir.exists(work_path)) {
    root_dir <- file.path(tempdir(check = TRUE), "archive", subject$project_name, subject$subject_code, "archive")
  } else {
    root_dir <- file.path(work_path, "archive")
  }

  if(file.exists(root_dir)) {
    unlink(root_dir, recursive = TRUE, force = TRUE)
  }
  root_dir <- dir_create2(root_dir)
  current_wd <- getwd()
  on.exit({
    unlink(root_dir, recursive = TRUE, force = TRUE)
    setwd(current_wd)
  }, add = TRUE, after = FALSE)
  meta_info <- list(
    version = 2,
    includes = c("meta_data", includes),
    original_project_name = subject$project_name,
    original_subject_code = subject$subject_code,
    user_config = config,
    paths = list()
  )

  copy_file <- function(from, to, ...) {
    if(file.exists(from)) {
      file.copy(from, dir_create2(to), overwrite = TRUE, recursive = TRUE,
                copy.mode = FALSE, copy.date = TRUE)
      return(TRUE)
    }
    return(FALSE)
  }

  copy_file(
    from = file.path(subject$rave_path, "meta"),
    to = dir_create2(file.path(root_dir, "meta_data"))
  )
  meta_info$paths$meta_data <- list(
    type = "data_dir",
    level = "subject",
    # subject/rave/...
    src = 'meta_data',
    dst = '/rave'
  )

  if("orignal_signals" %in% includes) {

    # find original blocks
    if(include_all_raw) {
      blocks <- subject$preprocess_settings$all_blocks
      blocks <- blocks[!(
        startsWith(blocks, "rave-imaging") |
          startsWith(blocks, "CT") |
          startsWith(blocks, "MRI") |
          startsWith(blocks, "DICOM") |
          endsWith(blocks, "_CT") |
          endsWith(blocks, "_MRI")
      )]
    } else {
      blocks <- subject$preprocess_settings$blocks
    }
    blocks <- as.character(blocks)

    path_orignal_signals <- dir_create2(file.path(root_dir, "orignal_signals"))
    if(length(blocks)) {
      for(block in blocks) {
        copy_file(
          from = file.path(subject$preprocess_settings$raw_path, block),
          to = path_orignal_signals
        )
      }
    }

    meta_info$paths$orignal_signals <- list(
      type = "raw_data_dir",
      level = "subject",
      src = "orignal_signals",
      dst = "/"
    )

  }

  if("processed_data" %in% includes) {

    path_processed_data <- dir_create2(file.path(root_dir, "processed_data"))


    copy_file(
      from = file.path(subject$rave_path, "preprocess"),
      to = path_processed_data
    )
    copy_file(
      from = file.path(subject$rave_path, "log.yaml"),
      to = path_processed_data
    )
    copy_file(
      from = file.path(subject$rave_path, "meta"),
      to = path_processed_data
    )
    if( include_cache ) {
      copy_file(
        from = file.path(subject$rave_path, "data"),
        to = path_processed_data
      )
    } else {
      fs <- list.files(
        file.path(subject$rave_path, "data"),
        all.files = FALSE,
        recursive = FALSE,
        include.dirs = TRUE,
        no.. = FALSE,
        full.names = FALSE
      )
      fs <- fs[!startsWith(fs, "cache")]
      for(f in fs) {
        copy_file(
          from = file.path(subject$rave_path, "data", f),
          to = file.path(path_processed_data, "data")
        )
      }
      copy_file(
        from = file.path(subject$rave_path, "data", "cache", "cached_reference.csv"),
        to = file.path(path_processed_data, "data", "cache")
      )
    }

    meta_info$paths$processed_data <- list(
      type = "data_dir",
      level = "subject",
      # subject/rave/...
      src = 'processed_data',
      dst = '/rave'
    )

  }

  if("pipelines" %in% includes) {
    path_pipelines <- dir_create2(file.path(root_dir, "pipelines"))

    pipeline_folders <-
      list.files(
        file.path(subject$rave_path, "pipeline"),
        all.files = FALSE,
        full.names = FALSE,
        recursive = FALSE,
        include.dirs = TRUE,
        no.. = TRUE
      )
    for(f in pipeline_folders) {
      copy_file(
        from = file.path(subject$rave_path, "pipeline", f),
        to = path_pipelines
      )
    }

    meta_info$paths$pipelines <- list(
      type = "data_dir",
      level = "subject",
      src = "pipelines",
      dst = "/rave/pipeline"
    )

  }

  if("rave_imaging" %in% includes) {
    # get rave-imaging folder
    path_imaging <- dir_create2(file.path(root_dir, "rave_imaging"))
    subject_imaging_path <- file.path(subject$preprocess_settings$raw_path, "rave-imaging")

    meta_info$paths$rave_imaging <- list(
      type = "raw_data_dir",
      level = "subject",
      # subject/rave/...
      src = 'rave_imaging',
      dst = '/rave-imaging'
    )

    # copy coregistration
    copy_file(
      from = file.path(subject_imaging_path, "coregistration"),
      to = path_imaging
    )

    copy_file(
      from = file.path(subject_imaging_path, "derivative"),
      to = path_imaging
    )

    copy_file(
      from = file.path(subject_imaging_path, "inputs"),
      to = path_imaging
    )

    copy_file(
      from = file.path(subject_imaging_path, "log"),
      to = path_imaging
    )

    copy_file(
      from = file.path(subject_imaging_path, "scripts"),
      to = path_imaging
    )

    copy_file(
      from = file.path(subject_imaging_path, "custom-data"),
      to = path_imaging
    )

    has_ants <- copy_file(
      from = file.path(subject_imaging_path, "ants"),
      to = path_imaging
    )

    has_fs <- copy_file(
      from = file.path(subject_imaging_path, "fs"),
      to = path_imaging
    )

    if(!(has_fs || has_ants)) {
      fs_path <- subject$freesurfer_path
      if(length(fs_path) == 1 && !is.na(fs_path) && nzchar(fs_path) && file.exists(fs_path)) {
        fs <- list.files(
          fs_path,
          all.files = FALSE,
          full.names = TRUE,
          recursive = FALSE,
          include.dirs = TRUE,
          no.. = FALSE
        )
        for(f in fs) {
          copy_file(
            from = f,
            to = file.path(path_imaging, "fs")
          )
        }
      }
    }



  }

  if("notes" %in% includes) {
    path_notes <- dir_create2(file.path(root_dir, "notes"))


    copy_file(
      from = dirname(subject$note_path),
      to = path_notes
    )

    meta_info$paths$notes <- list(
      type = "data_dir",
      level = "subject",
      # subject/rave/...
      src = 'notes',
      dst = '/'
    )
  }

  if("user_generated" %in% includes) {
    path_user_generated <- dir_create2(file.path(root_dir, "user_generated"))
    # user_generated

    copy_file(
      from = file.path(subject$rave_path, "exports"),
      to = path_user_generated
    )

    copy_file(
      from = file.path(subject$rave_path, "figures"),
      to = path_user_generated
    )

    meta_info$paths$notes <- list(
      type = "data_dir",
      level = "subject",
      # subject/rave/...
      src = 'user_generated',
      dst = '/rave'
    )
  }

  # generate meta
  save_yaml(meta_info, file.path(root_dir, "rave-archive.yaml"))

  # zip
  setwd(dirname(root_dir))
  zipfile_name <- sprintf("./%s.zip", rand_string(10))

  if(length(zip_flags)) {
    utils::zip(zipfile = zipfile_name, files = "./archive", flags = zip_flags)
  } else {
    utils::zip(zipfile = zipfile_name, files = "./archive")
  }

  zipfile_name <- normalizePath(zipfile_name)

  setwd(current_wd)

  if(!missing(path) && !is.na(path) && length(path) == 1) {
    if(file.exists(path)) {
      backup_file(path, remove = TRUE)
    }
    file_move(zipfile_name, path)
  } else {
    path <- zipfile_name
  }

  unlink(root_dir, recursive = TRUE, force = TRUE)

  message("The subject has been created at:\n  ", path)
  return(invisible(normalizePath(path)))
}


#' @title Install a subject from the internet, a zip file or a directory
#' @param path path to subject archive, can be a path to directory, a zip file,
#' or an internet address (must starts with \code{'http'}, or \code{'ftp'})
#' @param overwrite whether to overwrite existing subject, see argument
#' \code{ask} and \code{backup}
#' @param ask when \code{overwrite} is false, whether to ask the user if subject
#' exists; default is true when running in interactive session; users will be
#' prompt with choices; if \code{ask=FALSE} and \code{overwrite=FALSE}, then
#' the process will end with a warning if the subject exists.
#' @param backup whether to back-up the subject when overwriting the data;
#' default is true, which will rename the old subject folders instead of
#' removing; set to true to remove existing subject.
#' @param use_cache whether to use cached extraction directory; default is
#' true. Set it to \code{FALSE} if you want a clean installation.
#' @param dry_run whether to dry-run the process instead of actually installing;
#' this rehearsal can help you see the progress and prevent you from losing data
#' @param force_project,force_subject force set the project or subject;
#' will raise a warning as this might mess up some pipelines
#' @param ... passed to \code{\link[utils]{download.file}}
#' @examples
#'
#' # Please run 2nd example of function archive_subject
#'
#' \dontrun{
#'
#' install_subject(path)
#'
#' }
#'
#' @export
install_subject <- function(
    path = ".", ask = interactive(),
    overwrite = FALSE, backup = TRUE, use_cache = TRUE,
    dry_run = FALSE, force_project = NA, force_subject = NA,
    ...) {

  if(path %in% names(template_subjects)) {
    item <- template_subjects[[path]]
    if(isTRUE(item$version == 1)){
      # use rave::download_sample_data (RAVE 1.0)
      rave <- asNamespace("rave")
      rave$download_sample_data(subject = path, replace_if_exists = TRUE)
      return(invisible())
    }
    # version >= 2
    path <- item$url
    overwrite <- TRUE
    backup <- FALSE
  }

  if(startsWith(path, "http") || startsWith(path, "ftp")) {
    current_timeout <- getOption("timeout", 60)
    options("timeout" = 60*60)
    on.exit({
      options("timeout" = current_timeout)
    })

    zipfile <- file.path(tempdir(check = TRUE), sprintf("%s.zip", dipsaus::digest(path)))
    if(file.exists(zipfile)) {
      if(!use_cache) {
        unlink(zipfile, force = TRUE)
      }
    }
    if(!file.exists(zipfile)) {
      suppressWarnings({
        utils::download.file(path, destfile = zipfile, cacheOK = use_cache, ...)
      })
    }
    path <- zipfile

  }

  if(!dir.exists(path) && file.exists(path)) {
    # this is a zip file
    extract_path <- file.path(
      tempdir(check = TRUE),
      paste0(gsub("\\.zip", "", filenames(path), ignore.case = TRUE), "_UNZIP")
    )
    if(dir.exists(extract_path)) {
      if(!use_cache) {
        unlink(extract_path, recursive = TRUE, force = TRUE)
      }
    }
    if(!dir.exists(extract_path)) {
      utils::unzip(path, overwrite = TRUE, exdir = extract_path)
      on.exit({
        unlink(extract_path, recursive = TRUE, force = TRUE)
      })
    }
    path <- extract_path
  }

  if(dir.exists(file.path(path, "archive"))) {
    path <- file.path(path, "archive")
  }

  # check if this is RAVE 2.0
  if(!file.exists(file.path(path, "rave-archive.yaml"))) {
    stop("This is not a valid RAVE 2.0 subject.")
  }

  meta <- load_yaml(file.path(path, "rave-archive.yaml"))

  if(is.list(meta$user_config$rename)) {
    project_name <- c(meta$user_config$rename$project_name, meta$original_project_name)[[1]]
    subject_code <- c(meta$user_config$rename$subject_code, meta$original_subject_code)[[1]]
  } else {
    project_name <- meta$original_project_name
    subject_code <- meta$original_subject_code
  }

  force <- FALSE
  if(!is.na(force_project)) {
    message("Forcing project -> ", force_project)
    project_name <- force_project
    force <- TRUE
  }
  if(!is.na(force_subject)) {
    message("Forcing subject -> ", force_subject)
    subject_code <- force_subject
    force <- TRUE
  }
  if(force) {
    warning("You have forced to set the project name and/or subject code. This might break some pipelines.")
  }

  # check if this subject exists
  subject <- RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)

  if(file.exists(subject$path) || file.exists(subject$preprocess_settings$raw_path)) {
    if(dry_run) {
      message(glue("[Dry-run message]: Subject [{project_name}/{subject_code}] exists. This subject will be { ifelse(backup, 'renamed', 'REMOVED') }."))
    } else {
      if(!overwrite) {
        ans <- 0
        if(ask && interactive()) {
          message(glue("Subject [{project_name}/{subject_code}] exists. Do you want to overwrite? (Choosing YES will { ifelse(backup, 'rename', 'REMOVE') } existing subject)"))
          ans <- utils::menu(choices = c(
            "Yes",
            "No"
          ))
        }
        if( !isTRUE(ans == 1) ) {
          warning(glue("Subject [{project_name}/{subject_code}] exists and will not replace"))
          return(invisible())
        }
      }

      if( backup ) {
        if(file.exists(subject$preprocess_settings$raw_path)) {
          new_path <- backup_file(subject$preprocess_settings$raw_path, remove = FALSE)
        }
        if(file.exists(subject$path)) {
          file_move(subject$path, file.path(dirname(subject$path), filenames(new_path)))
        }
      }
    }
  }

  copy_file <- function(from, to, ...) {
    if(file.exists(from)) {
      if(dry_run) {
        message(glue("[Dry-run message]: Will copy: { from }\n  -> under: { to }"))
        return(TRUE)
      }
      file.copy(from, dir_create2(to), overwrite = TRUE, recursive = TRUE,
                copy.mode = FALSE, copy.date = TRUE)
      return(TRUE)
    }
    return(FALSE)
  }

  # install
  if(dry_run) {
    lp <- function(...) {
      lapply(...)
    }
  } else {
    lp <- function(...) {
      lapply_async(..., callback = function(nm) {
        sprintf("Installing subject|Installing %s...", nm)
      })
    }
  }
  lp(names(meta$paths), function(nm) {
    item <- meta$paths[[nm]]
    root_path <- switch(
      item$type,
      "data_dir" = subject$path,
      subject$preprocess_settings$raw_path
    )
    dst_path <- file.path(root_path, item$dst)
    src_path <- file.path(path, item$src)
    fs <- list.files(
      src_path,
      all.files = FALSE,
      full.names = TRUE,
      recursive = FALSE,
      include.dirs = TRUE,
      no.. = TRUE
    )
    for(f in fs) {
      copy_file(f, to = dst_path)
    }
  })

  if(dry_run) {

    message("You are running under dry-run (rehearsal) mode, subject is not installed. Please set `dry_run=FALSE` to install subject.")
    invisible(NULL)
  } else {
    message("Done.")
    tryCatch({
      subject <- RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
      subject$initialize_paths(include_freesurfer = FALSE)
      subject
      return(invisible(subject))
    }, error = function(e) {
      invisible(NULL)
    })
  }
}



template_subjects <- list(
  "yael_demo_001" = list(
    version = 2,
    url = "https://github.com/beauchamplab/rave/releases/download/v1.0.3/yael_demo_001.zip"
  ),
  "DemoSubject" = list(
    version = 2,
    url = "https://github.com/beauchamplab/rave/releases/download/v1.0.3/DemoSubject.zip"
  ),
  "KC" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_KC.zip"
  ),
  "YAB" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_YAB.zip"
  ),
  "YAD" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_YAD.zip"
  ),
  "YAF" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_YAF.zip"
  ),
  "YAH" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_YAH.zip"
  ),
  "YAI" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_YAI.zip"
  ),
  "YAJ" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_YAJ.zip"
  ),
  "YAK" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_YAK.zip"
  ),
  "DemoGroupData" = list(
    version = 1,
    url = "https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo__group_data.zip"
  )
)

