
#' @rdname cmd-external
#' @export
cmd_run_3dAllineate <- function(
    subject, mri_path, ct_path,
    overwrite = FALSE, command_path = NULL,
    dry_run = FALSE, verbose = dry_run) {
  # # Debug:
  # subject <- as_rave_subject("devel/YCQ", strict = FALSE)
  # mri_path <- "~/rave_data/raw_dir/YCQ/rave-imaging/inputs/MRI/YCQ_MRI.nii"
  # ct_path <- "~/rave_data/raw_dir/YCQ/rave-imaging/inputs/CT/YCQ_CT.nii"
  # command_path = NULL
  # overwrite <- FALSE

  mri_path <- validate_nii(mri_path)
  ct_path <- validate_nii(ct_path)

  subject <- as_rave_subject(subject, strict = FALSE)
  dest_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "coregistration"),
    winslash = "/", mustWork = FALSE
  )

  default_afni_path <- cmd_afni_home(error_on_missing = FALSE)
  afni_path <- tryCatch({
    afni <- normalize_commandline_path(
      path = command_path,
      unset = default_afni_path,
      type = "afni"
    )
    if(length(afni) != 1 || is.na(afni) || !isTRUE(dir.exists(afni))) {
      afni <- NULL
    } else if(!identical(default_afni_path, afni)) {
      raveio_setopt("afni_path", afni)
    }
    afni
  }, error = function(e){ NULL })

  has_afni <- !is.null(afni_path)

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-3dAllineate-%y%m%d-%H%M%S.log")

  template <- c(readLines(system.file('shell-templates/afni-3dallineate-coregistration.sh',
                                      package = "raveio")), "")
  # template <- readLines('inst/shell-templates/afni-3dallineate-coregistration.sh')
  workdir <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "coregistration"),
    mustWork = FALSE, winslash = "/"
  )
  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE, .null = "")

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts", "cmd-afni-3dallineate.sh"),
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
    afni_home = afni_path,
    log_file = file.path(log_path, log_file, fsep = "/"),
    mri_path = mri_path,
    ct_path = ct_path,
    dest_path = dest_path,
    execute = execute
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


