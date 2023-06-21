
#' @rdname cmd-external
#' @export
cmd_run_recon_all <- function(
    subject, mri_path,
    args = c(
      "-all", "-autorecon1", "-autorecon2", "-autorecon3",
      "-autorecon2-cp", "-autorecon2-wm", "-autorecon2-pial"
    ),
    work_path = NULL,
    overwrite = FALSE, command_path = NULL,
    dry_run = FALSE, verbose = dry_run) {
  # # Debug:
  # subject <- as_rave_subject("devel/YAH", strict = FALSE)
  # mri_path <- "~/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/YAH/rave-imaging/inputs/MRI/MRI_RAW.nii"
  # command_path = NULL
  # overwrite <- FALSE
  # list2env(list(subject = "devel/YAH",
  #               args = "-all", overwrite = FALSE, dry_run = TRUE, verbose = FALSE,
  #               command_path = NULL), globalenv())
  # program <- "recon-all-clinic"

  all_args <- c(
    "-all", "-autorecon1", "-autorecon2", "-autorecon3",
    "-autorecon2-cp", "-autorecon2-wm", "-autorecon2-pial"
  )
  args <- all_args[all_args %in% args]
  if('-all' %in% args) {
    args <- '-all'
  } else if(!length(args)) {
    stop("`cmd_run_recon_all`: recon-all flag is invalid")
  }

  if(missing(mri_path) || length(mri_path) != 1 || is.na(mri_path) || !file.exists(mri_path) ||
     dir.exists(mri_path)) {
    stop("`cmd_run_recon_all`: `mri_path` is not a valid path.")
  }
  mri_path <- normalizePath(mri_path, winslash = "/")

  if(!grepl("\\.nii($|\\.gz$)", mri_path, ignore.case = TRUE)) {
    stop("`cmd_run_recon_all`: `mri_path` is not a valid NifTi file.")
  }

  subject <- restore_subject_instance(subject, strict = FALSE)

  default_fs_path <- cmd_freesurfer_home(error_on_missing = FALSE)
  freesurfer_home <- tryCatch({
    freesurfer <- normalize_commandline_path(
      path = command_path,
      unset = default_fs_path,
      type = "freesurfer"
    )
    if(length(freesurfer) != 1 || is.na(freesurfer) || !isTRUE(dir.exists(freesurfer))) {
      freesurfer <- NULL
    } else if(!identical(default_fs_path, freesurfer)) {
      raveio_setopt("freesurfer_path", freesurfer)
    }
    freesurfer
  }, error = function(e){ NULL })

  has_freesurfer <- !is.null(freesurfer_home)
  if(has_freesurfer) {
    freesurfer_home <- normalizePath(freesurfer_home, winslash = "/")
  }
  cmd_recon <- "recon-all"

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-recon-all-%y%m%d-%H%M%S.log")

  # Always use a temporary working path since the target directory might contain spaces
  if(length(work_path) != 1 || is.na(work_path) || !dir.exists(work_path)) {
    work_path_symlink <- file.path(R_user_dir("raveio", which = "cache"), "FreeSurfer",
                                   subject$subject_code, fsep = "/")
  } else {
    work_path_symlink <- file.path(work_path, subject$subject_code, fsep = "/")
  }

  work_path_actual <- subject$preprocess_settings$raw_path

  template <- c(readLines(system.file('shell-templates/recon-all-t1.sh', package = "raveio")), "")
  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE)

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts", "cmd-fs-recon.sh"),
    mustWork = FALSE, winslash = "/"
  )
  execute <- function(...) {
    initialize_imaging_paths(subject)
    cmd_execute(script = cmd, script_path = script_path, command = "bash", ...)
  }
  re <- list(
    script = cmd,
    script_path = script_path,
    dry_run = dry_run,
    freesurfer_home = freesurfer_home,
    log_file = file.path(log_path, log_file, fsep = "/"),
    src_path = mri_path,
    dest_path = file.path(work_path_actual, "rave-imaging", "fs", fsep = "/"),
    execute = execute,
    command = "bash"
  )
  if( verbose ) {
    message(cmd)
  }
  if(dry_run) {
    return(invisible(re))
  }

  execute()

  return(invisible(re))

  # cmd_run_recon_all(subject = "devel/YCQ", mri_path = "/Volumes/PennRAID/Dropbox (PENN Neurotrauma)/BeauchampServe/rave_data/raw/YCQ/rave-imaging/inputs/MRI/YCQ_MRI.nii", args = "-autorecon1", verbose = TRUE)
}

#' @rdname cmd-external
#' @export
cmd_run_recon_all_clinical <- function(
    subject, mri_path,
    work_path = NULL,
    overwrite = FALSE, command_path = NULL,
    dry_run = FALSE, verbose = dry_run, ...) {

  ncores <- raveio_getopt("max_worker", default = 1L)

  if(missing(mri_path) || length(mri_path) != 1 || is.na(mri_path) || !file.exists(mri_path) ||
     dir.exists(mri_path)) {
    stop("`cmd_run_recon_all_clinical`: `mri_path` is not a valid path.")
  }
  mri_path <- normalizePath(mri_path, winslash = "/")

  if(!grepl("\\.nii($|\\.gz$)", mri_path, ignore.case = TRUE)) {
    stop("`cmd_run_recon_all_clinical`: `mri_path` is not a valid NifTi file.")
  }

  subject <- restore_subject_instance(subject, strict = FALSE)

  default_fs_path <- cmd_freesurfer_home(error_on_missing = FALSE)
  freesurfer_home <- tryCatch({
    freesurfer <- normalize_commandline_path(
      path = command_path,
      unset = default_fs_path,
      type = "freesurfer"
    )
    if(length(freesurfer) != 1 || is.na(freesurfer) || !isTRUE(dir.exists(freesurfer))) {
      freesurfer <- NULL
    } else if(!identical(default_fs_path, freesurfer)) {
      raveio_setopt("freesurfer_path", freesurfer)
    }
    freesurfer
  }, error = function(e){ NULL })

  has_freesurfer <- !is.null(freesurfer_home)
  if(has_freesurfer) {
    freesurfer_home <- normalizePath(freesurfer_home, winslash = "/")
  }
  cmd_recon <- "recon-all-clinical.sh"

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-recon-all-clinical-%y%m%d-%H%M%S.log")

  # Always use a temporary working path since the target directory might contain spaces
  if(length(work_path) != 1 || is.na(work_path) || !dir.exists(work_path)) {
    work_path_symlink <- file.path(R_user_dir("raveio", which = "cache"), "FreeSurfer",
                                   subject$subject_code, fsep = "/")
  } else {
    work_path_symlink <- file.path(work_path, subject$subject_code, fsep = "/")
  }

  work_path_actual <- subject$preprocess_settings$raw_path

  template <- c(readLines(system.file('shell-templates/recon-all-clinical.sh', package = "raveio")), "")
  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE)

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts", "cmd-fs-recon-clinical.sh"),
    mustWork = FALSE, winslash = "/"
  )
  execute <- function(...) {
    initialize_imaging_paths(subject)
    cmd_execute(script = cmd, script_path = script_path, command = "bash", ...)
  }
  re <- list(
    script = cmd,
    script_path = script_path,
    dry_run = dry_run,
    freesurfer_home = freesurfer_home,
    log_file = file.path(log_path, log_file, fsep = "/"),
    src_path = mri_path,
    dest_path = file.path(work_path_actual, "rave-imaging", "fs", fsep = "/"),
    execute = execute,
    command = "bash"
  )
  if( verbose ) {
    message(cmd)
  }
  if(dry_run) {
    return(invisible(re))
  }

  execute()

  return(invisible(re))

  # cmd_run_recon_all_clinical(subject = "devel/TMP", mri_path = "~/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/TMP/IMG/T1.nii.gz", verbose = TRUE)
}
