
#' @rdname cmd-external
#' @export
cmd_run_flirt <- function(
    subject, mri_path, ct_path,
    overwrite = FALSE, command_path = NULL,
    dry_run = FALSE, verbose = dry_run) {
  # # Debug:
  # subject <- as_rave_subject("devel/YCQ", strict = FALSE)
  # mri_path <- "/Volumes/PennRAID/Dropbox (PENN Neurotrauma)/BeauchampServe/rave_data/raw/YCQ/rave-imaging/inputs/MRI/YCQ_MRI.nii"
  # ct_path <- "/Volumes/PennRAID/Dropbox (PENN Neurotrauma)/BeauchampServe/rave_data/raw/YCQ/rave-imaging/inputs/CT/YCQ_CT.nii"
  # command_path = NULL
  # overwrite <- FALSE

  mri_path <- validate_nii(mri_path)
  ct_path <- validate_nii(ct_path)

  subject <- as_rave_subject(subject, strict = FALSE)
  dest_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "coregistration"),
    winslash = "/", mustWork = FALSE
  )

  default_fsl_path <- cmd_fsl_home(error_on_missing = FALSE)
  fsl_home <- tryCatch({
    fsl <- normalize_commandline_path(
      path = command_path,
      unset = default_fsl_path,
      type = "fsl"
    )
    if(length(fsl) != 1 || is.na(fsl) || !isTRUE(dir.exists(fsl))) {
      fsl <- NULL
    } else if(!identical(default_fsl_path, fsl)) {
      raveio_setopt("fsl_path", fsl)
    }
    fsl
  }, error = function(e){ NULL })

  has_fsl <- !is.null(fsl_home)

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-flirt-%y%m%d-%H%M%S.log")

  template <- c(readLines(system.file('shell-templates/fsl-flirt-coregistration.sh',
                                      package = "raveio")), "")
  # template <- readLines('inst/shell-templates/fsl-flirt-coregistration.sh')
  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE, .null = "")

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts", "cmd-fsl-flirt.sh"),
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
    fsl_home = fsl_home,
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


