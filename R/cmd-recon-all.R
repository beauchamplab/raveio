
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

  work_path_actual <- normalize_path(subject$preprocess_settings$raw_path, must_work = FALSE)

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

#' @rdname yael_preprocess
#' @param run_recon_all whether to run \code{'FreeSurfer'} reconstruction;
#' default is true
#' @param dry_run whether to dry-run the script and check if error exists before
#' actually execute the scripts.
#' @export
cmd_run_yael_preprocess <- function(
    subject_code,
    t1w_path = NULL,
    ct_path = NULL,
    t2w_path = NULL,
    fgatir_path = NULL,
    preopct_path = NULL,
    flair_path = NULL,
    t1w_contrast_path = NULL,
    register_reversed = FALSE,
    normalize_template = "mni_icbm152_nlin_asym_09b",
    run_recon_all = TRUE,
    dry_run = FALSE, verbose = TRUE) {
  # DIPSAUS DEBUG START
  # subject_code = "testtest3"
  # t1w_path = "/Users/dipterix/rave_data/raw_dir/AnonSEEG/preprocessing/anat/sub-AnonSEEG_ses-preop_desc-preproc_acq-ax_T1w.nii"
  # ct_path = "/Users/dipterix/rave_data/raw_dir/AnonSEEG/preprocessing/anat/sub-AnonSEEG_ses-postop_desc-preproc_CT.nii"
  # t2w_path = "/Users/dipterix/rave_data/raw_dir/AnonSEEG/preprocessing/anat/sub-AnonSEEG_ses-preop_desc-preproc_acq-iso_T2w.nii"
  # fgatir_path = "/Users/dipterix/rave_data/raw_dir/AnonSEEG/preprocessing/anat/sub-AnonSEEG_ses-preop_desc-preproc_acq-ax_FGATIR.nii"
  # preopct_path = NULL
  # verbose <- TRUE
  # register_reversed = FALSE
  # flair_path=NULL;t1w_contrast_path=NULL
  # run_recon_all <- FALSE

  run_recon_all <- as.integer(isTRUE(as.logical(run_recon_all)))
  register_reversed <- isTRUE(as.logical(register_reversed))

  yael_process <- YAELProcess$new(subject_code = subject_code)

  if(length(t1w_path)) {
    t1w_path <- normalizePath(t1w_path, winslash = "/", mustWork = TRUE)
  } else {
    if(!length(yael_process$get_input_image("T1w"))) {
      stop("`cmd_run_yael_preprocess`: No `T1w` MRI specified. Please specify a valid file")
    }
    t1w_path <- ""
  }
  if(length(ct_path)) { ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE) } else { ct_path <- "" }
  if(length(t2w_path)) { t2w_path <- normalizePath(t2w_path, winslash = "/", mustWork = TRUE) } else { t2w_path <- "" }
  if(length(fgatir_path)) { fgatir_path <- normalizePath(fgatir_path, winslash = "/", mustWork = TRUE) } else { fgatir_path <- "" }
  if(length(preopct_path)) { preopct_path <- normalizePath(preopct_path, winslash = "/", mustWork = TRUE) } else { preopct_path <- "" }
  if(length(flair_path)) { flair_path <- normalizePath(flair_path, winslash = "/", mustWork = TRUE) } else { flair_path <- "" }
  if(length(t1w_contrast_path)) { t1w_contrast_path <- normalizePath(t1w_contrast_path, winslash = "/", mustWork = TRUE) } else { t1w_contrast_path <- "" }

  subject <- RAVESubject$new(project_name = "YAEL", subject_code = subject_code, strict = FALSE)

  if(length(normalize_template)) {
    if("mni_icbm152_nlin_asym_09b" %in% normalize_template) {
      normalize_template <- unique(c("mni_icbm152_nlin_asym_09b", normalize_template))
    }
    lapply(normalize_template, rpyANTs::ensure_template)
    normalize_template_str <- sprintf(
      "c(%s)", paste(sprintf("'%s'", normalize_template), collapse = ",")
    )
  } else {
    normalize_template_str <- "NULL"
  }

  # default_fs_path <- cmd_freesurfer_home(error_on_missing = FALSE)
  # freesurfer_home <- tryCatch({
  #   freesurfer <- normalize_commandline_path(
  #     path = command_path,
  #     unset = default_fs_path,
  #     type = "freesurfer"
  #   )
  #   if(length(freesurfer) != 1 || is.na(freesurfer) || !isTRUE(dir.exists(freesurfer))) {
  #     freesurfer <- NULL
  #   } else if(!identical(default_fs_path, freesurfer)) {
  #     raveio_setopt("freesurfer_path", freesurfer)
  #   }
  #   freesurfer
  # }, error = function(e){ NULL })
  #
  # has_freesurfer <- !is.null(freesurfer_home)
  # if(has_freesurfer) {
  #   freesurfer_home <- normalizePath(freesurfer_home, winslash = "/")
  # }
  # cmd_recon <- "recon-all"

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-yael-preprocess-%y%m%d-%H%M%S.log")

  # Always use a temporary working path since the target directory might contain spaces
  work_path_actual <- normalize_path(subject$preprocess_settings$raw_path, must_work = FALSE)

  project_name <- "YAEL"

  template <- c(readLines(system.file('shell-templates/yael-preprocess.R', package = "raveio")), "")
  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE)

  if( run_recon_all ) {
    script_name <- "cmd-yael-preprocess-full.R"
  } else {
    script_name <- "cmd-yael-preprocess-simple.R"
  }

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts", script_name),
    mustWork = FALSE, winslash = "/"
  )
  execute <- function(...) {
    initialize_imaging_paths(subject)
    dir_create2(log_path)
    log_abspath <- normalizePath(file.path(log_path, log_file),
                                 winslash = "/", mustWork = FALSE)
    cmd_execute(script = cmd, script_path = script_path,
                args = c("--no-save", "--no-restore"),
                command = rscript_path(),
                stdout = log_abspath, stderr = log_abspath, ...)
  }
  re <- list(
    script = cmd,
    script_path = script_path,
    dry_run = dry_run,
    log_file = file.path(log_path, log_file, fsep = "/"),
    src_path = t1w_path,
    dest_path = file.path(work_path_actual, "rave-imaging", fsep = "/"),
    execute = execute,
    command = rscript_path()
  )
  if( verbose ) {
    message(cmd)
  }
  if(dry_run) {
    return(invisible(re))
  }

  execute()

  return(invisible(re))

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
