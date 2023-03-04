#' Register 'CT' to 'MR' images via \code{'ANTs'}
#' @param ct_path,mri_path absolute paths to 'CT' and 'MR' image files
#' @param coreg_path registration path, where to save results; default is
#' the parent folder of \code{ct_path}
#' @param reg_type registration type, choices are \code{'DenseRigid'},
#' \code{'Rigid'}, \code{'Affine'}, \code{'SyN'}, \code{'TRSAA'},
#' \code{'SyNCC'}, \code{'SyNOnly'}, or other types; see
#' \code{\link[rpyANTs]{ants_registration}}
#' @param aff_metric cost function to use for linear or 'affine' transform
#' @param syn_metric cost function to use for \code{'SyN'} transform
#' @param dry_run whether to dry-run the script and to print out the command
#' instead of executing the code; default is false
#' @param subject 'RAVE' subject
#' @param verbose whether to verbose command; default is true
#' @param ... other arguments passed to \code{\link[rpyANTs]{ants_registration}}
#' @return Aligned 'CT' will be generated at the \code{coreg_path} path:
#' \describe{
#' \item{\code{'ct_in_t1.nii.gz'}}{aligned 'CT' image; the image is
#' also re-sampled into 'MRI' space}
#' \item{\code{'transform.yaml'}}{transform settings and outputs}
#' \item{\code{'CT_IJK_to_MR_RAS.txt'}}{transform matrix from volume 'IJK'
#' space in the original 'CT' to the 'RAS' anatomical coordinate in 'MR'
#' scanner; 'affine' transforms only}
#' \item{\code{'CT_RAS_to_MR_RAS.txt'}}{transform matrix from scanner 'RAS'
#' space in the original 'CT' to 'RAS' in 'MR' scanner space; 'affine'
#' transforms only}
#' }
#' @export
ants_coreg <- function(
    ct_path, mri_path, coreg_path = NULL,
    reg_type = c("DenseRigid", "Rigid", "SyN", "Affine", "TRSAA", "SyNCC", "SyNOnly"),
    aff_metric = c('mattes', 'meansquares', 'GC'),
    syn_metric = c('mattes', 'meansquares', 'demons', 'GC'),
    verbose = TRUE, ...) {

  require_package("rpyANTs")

  if( length(reg_type) > 1 ) {
    reg_type <- match.arg(reg_type)
  }
  aff_metric <- match.arg(aff_metric)
  syn_metric <- match.arg(syn_metric)

  # DIPSAUS DEBUG START
  # ct_path <- '~/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV010/rave-imaging/coregistration_[backup_20230219_185631]/CT_RAW.nii'
  # mri_path <- '~/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV010/rave-imaging/coregistration_[backup_20230303_164930]/MRI_reference.nii.gz'
  # aff_metric <- "mattes"
  # syn_metric <- "mattes"
  # verbose <- TRUE
  # reg_type <- 'DenseRigid'
  # coreg_path <- '~/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV010/rave-imaging/coregistration/'

  if(length(coreg_path) != 1) {
    coreg_path <- dirname(ct_path)
  }

  # read in files
  ct_img <- rpyANTs::as_ANTsImage(ct_path)
  mri_img <- rpyANTs::as_ANTsImage(mri_path)

  if(file.exists(coreg_path)) {
    backup_file(coreg_path, remove = TRUE, quiet = TRUE)
  }
  dir_create2(coreg_path)

  # save MRI and CT
  ct_img$to_file(file.path(coreg_path, "CT_RAW.nii.gz"))
  mri_img$to_file(file.path(coreg_path, "MRI_reference.nii.gz"))

  # get ct_img IJK to LPS
  ct_ijk_to_lps <- t(
    t(rpyANTs::py_to_r(ct_img$direction)) *
      as.double(rpyANTs::py_to_r(ct_img$spacing))
  )
  ct_ijk_to_lps <- rbind(cbind(ct_ijk_to_lps, as.double(rpyANTs::py_to_r(ct_img$origin))), c(0,0,0,1))

  transform <- rpyANTs::ants_registration(
    fixed = mri_img,
    moving = ct_img,
    type_of_transform = reg_type,
    aff_metric = aff_metric,
    syn_metric = syn_metric,
    verbose = verbose,
    outprefix = file.path(coreg_path, "ANTs_"),
    ...
  )

  # Save aligned files
  transform$warpedmovout$to_file(file.path(coreg_path, "ct_in_t1.nii.gz"))

  # save transforms
  # `fwdtransforms`: Transforms to move from moving to fixed image.
  # `invtransforms`: Transforms to move from fixed to moving image.
  forward  <- filenames(rpyANTs::py_to_r(transform$fwdtransforms))
  inverse <- filenames(rpyANTs::py_to_r(transform$invtransforms))

  results <- list(
    moving = "CT_RAW.nii.gz",
    fixing = "MRI_reference.nii.gz",
    aligned = "ct_in_t1.nii.gz",
    forward = forward,
    inverse = inverse
  )
  results$linear <- FALSE

  if( grepl("(rigid|affine)", reg_type, ignore.case = TRUE) ) {
    for(f in forward) {
      if(endsWith(f, "mat")) {
        lps_to_lps <- rpyANTs::as_ANTsTransform(file.path(coreg_path, f))
        ct_ijk_to_mri_lps <- solve(as.matrix(lps_to_lps)) %*% ct_ijk_to_lps
        ct_ijk_to_mri_ras <- diag(c(-1, -1, 1, 1)) %*% ct_ijk_to_mri_lps
        # save ct_ijk_to_mri_ras? need test
        utils::write.table(ct_ijk_to_mri_ras, file.path(coreg_path, "CT_IJK_to_MR_RAS.txt"),
                    row.names = FALSE, col.names = FALSE)

        ct_ras_to_mri_ras <- diag(c(-1, -1, 1, 1)) %*% solve(as.matrix(lps_to_lps)) %*% diag(c(-1, -1, 1, 1))
        utils::write.table(ct_ras_to_mri_ras, file.path(coreg_path, "CT_RAS_to_MR_RAS.txt"),
                    row.names = FALSE, col.names = FALSE)
        results$linear <- TRUE
      }
    }
  }

  save_yaml(results, file = file.path(coreg_path, "transform.yaml"))

  return(invisible(results))
}

#' @rdname ants_coreg
#' @export
cmd_run_ants_coreg <- function(
    subject, ct_path, mri_path,
    reg_type = c("DenseRigid", "Rigid", "SyN", "Affine", "TRSAA", "SyNCC", "SyNOnly"),
    aff_metric = c('mattes', 'meansquares', 'GC'),
    syn_metric = c('mattes', 'meansquares', 'demons', 'GC'),
    verbose = TRUE, dry_run = FALSE) {

  require_package("rpyANTs")

  # DIPSAUS DEBUG START
  # work_path <- "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging"
  # ct_path = "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging/derivative/CT_RAW.nii.gz"
  # mri_path = "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/rave-imaging/derivative/MRI_RAW.nii"
  # reg_type <- "Rigid"
  # verbose <- TRUE
  # subject <- "devel/PAV006"
  # dry_run <- FALSE
  # aff_metric <- syn_metric <- 'mattes'

  if( length(reg_type) > 1 ) {
    reg_type <- match.arg(reg_type)
  }
  aff_metric <- match.arg(aff_metric)
  syn_metric <- match.arg(syn_metric)

  subject <- restore_subject_instance(subject, strict = FALSE)
  work_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging"),
    winslash = "/", mustWork = FALSE
  )
  ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE)
  mri_path <- normalizePath(mri_path, winslash = "/", mustWork = TRUE)

  force(dry_run)

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-rave-ants-coregistration.R.log")

  template <- readLines(system.file("shell-templates/rave-ants-coregistration.R", package = "raveio"))

  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE, .null = "")

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts", "cmd-ants-coregistration.R"),
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
