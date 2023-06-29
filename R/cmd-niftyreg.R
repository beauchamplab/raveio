#' Register 'CT' to 'MR' images via \code{'NiftyReg'}
#' @description Supports 'Rigid', 'affine', or 'non-linear' transformation
#' @param ct_path,mri_path absolute paths to 'CT' and 'MR' image files
#' @param coreg_path registration path, where to save results; default is
#' the parent folder of \code{ct_path}
#' @param reg_type registration type, choices are \code{'rigid'},
#' \code{'affine'}, or \code{'nonlinear'}
#' @param interp how to interpolate when sampling volumes, choices are
#' \code{'trilinear'}, \code{'cubic'}, or \code{'nearest'}
#' @param dry_run whether to dry-run the script and to print out the command
#' instead of executing the code; default is false
#' @param subject 'RAVE' subject
#' @param verbose whether to verbose command; default is true
#' @param ... other arguments passed to \code{\link[ravetools]{register_volume}}
#' @returns Nothing is returned from the function. However, several files will
#' be generated at the 'CT' path:
#' \describe{
#' \item{\code{'ct_in_t1.nii'}}{aligned 'CT' image; the image is
#' also re-sampled into 'MRI' space}
#' \item{\code{'CT_IJK_to_MR_RAS.txt'}}{transform matrix from volume 'IJK'
#' space in the original 'CT' to the 'RAS' anatomical coordinate in 'MR'
#' scanner}
#' \item{\code{'CT_RAS_to_MR_RAS.txt'}}{transform matrix from scanner 'RAS'
#' space in the original 'CT' to 'RAS' in 'MR' scanner space}
#' }
#' @export
niftyreg_coreg <- function(
    ct_path, mri_path, coreg_path = NULL,
    reg_type = c("rigid", "affine", "nonlinear"),
    interp = c("trilinear", "cubic", "nearest"),
    verbose = TRUE, ...) {

  require_package("ravetools")

  # DIPSAUS DEBUG START
  # work_path <- "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging"
  # ct_path = "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging/derivative/CT_RAW.nii.gz"
  # mri_path = "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging/derivative/MRI_RAW.nii"
  # reg_type <- "rigid"
  # interp <- "cubic"
  # verbose <- TRUE

  ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE)
  mri_path <- normalizePath(mri_path, winslash = "/", mustWork = TRUE)

  reg_type <- match.arg(reg_type)
  interp <- match.arg(interp)


  rniftyreg <- asNamespace("RNiftyReg")
  rnifti <- asNamespace("RNifti")

  results <- ravetools::register_volume(source = ct_path, target = mri_path, method = reg_type, interpolation = interp, verbose = verbose, ...)

  # Get coregistration path
  if(length(coreg_path) != 1) {
    coreg_path <- dirname(ct_path)
  }
  backup_file(coreg_path, remove = TRUE, quiet = TRUE)
  dir_create2(coreg_path)

  # save MRI_reference.nii.gz
  rnifti$writeNifti(image = results$target,
                    file = file.path(coreg_path, "MRI_reference.nii.gz"))

  # save CT_RAW.nii.gz
  rnifti$writeNifti(image = results$source[[1]],
                    file = file.path(coreg_path, "CT_RAW.nii.gz"))

  # save ct_in_t1.nii.gz
  rnifti$writeNifti(image = results$image,
                    file = file.path(coreg_path, "ct_in_t1.nii.gz"))

  # save forward transform
  forward_transform <- rniftyreg$forward(results)
  if( rniftyreg$isAffine(forward_transform, strict = FALSE) ) {

    # forward_transform is a 4x4 matrix RAS CT to RAS scanner
    affine_ct_ras_to_mri_ras <- solve(forward_transform)
    utils::write.table(
      x = affine_ct_ras_to_mri_ras, sep = "\t", row.names = FALSE,
      col.names = FALSE, file = file.path(coreg_path, "CT_RAS_to_MR_RAS.txt"))

    ct_ijk2ras <- rnifti$xform(image = results$source[[1]])
    affine_ct_ijk_to_mri_ras <- affine_ct_ras_to_mri_ras %*% ct_ijk2ras

    utils::write.table(
      x = affine_ct_ijk_to_mri_ras, sep = "\t", row.names = FALSE,
      col.names = FALSE, file = file.path(coreg_path, "CT_IJK_to_MR_RAS.txt"))

  } else if(isTRUE(rniftyreg$isImage(forward_transform))) {

    # non-linear
    rnifti$writeNifti(image = forward_transform,
                      file = file.path(coreg_path, "forward_control_points.nii.gz"))

    rnifti$writeNifti(image = rniftyreg$reverse(results),
                      file = file.path(coreg_path, "reverse_control_points.nii.gz"))

  }

  return(invisible())
}

#' @rdname niftyreg_coreg
#' @export
cmd_run_niftyreg_coreg <- function(
    subject, ct_path, mri_path,
    reg_type = c("rigid", "affine", "nonlinear"),
    interp = c("trilinear", "cubic", "nearest"),
    verbose = TRUE, dry_run = FALSE, ...) {

  require_package("ravetools")

  # DIPSAUS DEBUG START
  # work_path <- "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging"
  # ct_path = "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging/derivative/CT_RAW.nii.gz"
  # mri_path = "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging/derivative/MRI_RAW.nii"
  # reg_type <- "rigid"
  # interp <- "cubic"
  # verbose <- TRUE
  # subject <- "devel/PAV006"

  subject <- restore_subject_instance(subject, strict = FALSE)
  work_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging"),
    winslash = "/", mustWork = FALSE
  )
  ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE)
  mri_path <- normalizePath(mri_path, winslash = "/", mustWork = TRUE)

  reg_type <- match.arg(reg_type)
  interp <- match.arg(interp)
  force(dry_run)

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-rave-nifty-coregistration.R.log")

  template <- readLines(system.file("shell-templates/rave-nifty-coregistration.R", package = "raveio"))

  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE, .null = "")

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts", "cmd-nifty-coregistration.R"),
    mustWork = FALSE, winslash = "/"
  )

  execute <- function(...) {
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
    mri_path = mri_path,
    ct_path = ct_path,
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
