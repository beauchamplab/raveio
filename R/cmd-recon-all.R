
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
  ravecore::cmd_run_freesurfer_recon_all(
    subject = subject,
    mri_path = mri_path,
    args = args,
    work_path = work_path,
    overwrite = overwrite,
    command_path = command_path,
    dry_run = dry_run,
    verbose = verbose
  )
}

#' @rdname cmd-external
#' @export
cmd_run_recon_all_clinical <- function(
    subject, mri_path,
    work_path = NULL,
    overwrite = FALSE, command_path = NULL,
    dry_run = FALSE, verbose = dry_run, ...) {
  ravecore::cmd_run_freesurfer_recon_all_clinical(
    subject = subject,
    mri_path = mri_path,
    work_path = work_path,
    overwrite = overwrite,
    command_path = command_path,
    dry_run = dry_run,
    verbose = verbose,
    ...
  )
}
