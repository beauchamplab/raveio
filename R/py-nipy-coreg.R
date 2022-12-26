#' Register 'CT' to 'MR' images via \code{'nipy'} script
#' @description Align 'CT' using
#' \code{nipy.algorithms.registration.histogram_registration}.
#' @param ct_path,mri_path absolute paths to 'CT' and 'MR' image files
#' @param clean_source whether to replace negative 'CT' values with zeros;
#' default is true
#' @param inverse_target whether to inverse 'MRI' color intensity; default
#' is true
#' @param precenter_source whether to adjust the 'CT' transform matrix
#' before alignment, such that the origin of 'CT' is at the center of the
#' volume; default is true. This option may avoid the case that 'CT' is
#' too far-away from the 'MR' volume at the beginning of the optimization
#' @param reg_type registration type, choices are \code{'rigid'} or
#' \code{'affine'}
#' @param smooth,interp,optimizer,tol optimization parameters, see
#' \code{'nipy'} documentation for details.
#' @param similarity the cost function of the alignment; choices are
#' \code{'crl1'} ('L1' regularized correlation), \code{'cc'} (correlation
#' coefficient), \code{'cr'} (correlation), \code{'mi'} (mutual information),
#' \code{'nmi'} (normalized mutual information), \code{'slr'} (likelihood
#' ratio). In reality I personally find \code{'crl1'} works best in most
#' cases, though many tutorials suggest \code{'nmi'}.
#' @param dry_run whether to dry-run the script and to print out the command
#' instead of executing the code; default is false
#' @return Nothing is returned from the function. However, several files will
#' be generated at the 'CT' path:
#' \describe{
#' \item{\code{'CT_coreg_resampled.nii'}}{aligned 'CT' image; the image is
#' also re-sampled into 'MRI' space}
#' \item{\code{'CT_IJK_to_MR_RAS.txt'}}{transform matrix from volume 'IJK'
#' space in the original 'CT' to the 'RAS' anatomical coordinate in 'MR'
#' scanner}
#' \item{\code{'CT_RAS_to_MR_RAS.txt'}}{transform matrix from scanner 'RAS'
#' space in the original 'CT' to 'RAS' in 'MR' scanner space}
#' }
#' @export
py_nipy_coreg <- function(
    ct_path, mri_path, clean_source = TRUE, inverse_target = TRUE,
    precenter_source = TRUE, smooth = 0, reg_type = c("rigid", "affine"),
    interp = c("pv", "tri"), similarity = c('crl1', 'cc', 'cr', 'mi', 'nmi', 'slr'),
    optimizer = c('powell', 'steepest', 'cg', 'bfgs', 'simplex'),
    tol = 0.0001, dry_run = FALSE) {
  # ct_path = "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV010/rave-imaging/coregistration/CT_RAW.nii"
  # mri_path = "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV010/rave-imaging/coregistration/MRI_reference.nii"

  ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE)
  mri_path <- normalizePath(mri_path, winslash = "/", mustWork = TRUE)

  reg_type <- match.arg(reg_type)
  interp <- match.arg(interp)
  similarity <- match.arg(similarity)
  optimizer <- match.arg(optimizer)
  force(clean_source)
  force(inverse_target)
  force(precenter_source)
  force(smooth)
  force(dry_run)
  ftol <- tol
  xtol <- tol

  template <- readLines(system.file("python-scripts/nipy-coregistration.py", package = "raveio"))

  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n",
              .open = '{{', .close = '}}', .trim = FALSE, .null = "")

  tmpf <- tempfile(fileext = ".py", pattern = "nipy-coregistration-")
  writeLines(cmd, tmpf)

  rpymat::run_script(tmpf, work_dir = tempdir(), local = FALSE, convert = FALSE)
  return(invisible())
}
