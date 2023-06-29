#' Register 'CT' or 'MR' images via \code{'ANTs'}
#' @description
#' \code{ants_coreg} aligns 'CT' to 'MR' images; \code{ants_mri_to_template}
#' aligns native 'MR' images to group templates
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
#' @param preview whether to preview results; default is false
#' @param template_subject template to map 'MR' images
#' @param ... other arguments passed to \code{\link[rpyANTs]{ants_registration}}
#' @returns Aligned 'CT' will be generated at the \code{coreg_path} path:
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
    syn_metric = c('mattes', 'meansquares', 'demons', 'CC'),
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
    syn_metric = c('mattes', 'meansquares', 'demons', 'CC'),
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


#' @rdname ants_coreg
#' @export
ants_mri_to_template <- function(
    subject,
    template_subject = getOption("threeBrain.template_subject", "N27"),
    preview = FALSE, verbose = TRUE, ...) {

  # DIPSAUS DEBUG START
  # subject <- "devel/PAV006"
  # orig_atlas_path <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV006/rave-imaging/fs/mri/aparc+aseg.nii"
  # fsaverage_atlas_path <- "~/rave_data/others/three_brain/fsaverage/mri/aparc+aseg.nii"
  # verbose <- TRUE
  # template_subject <- "N27"

  debug <- function(...) {
    if(verbose) {
      cat(..., "\n")
    }
  }

  brain <- rave_brain(subject = subject)
  if(is.null(brain)) { stop("Cannot morph electrodes. Please run FreeSurfer recon-all first") }

  require_package("rpyANTs")

  subject <- restore_subject_instance(subject, strict = FALSE)

  # make sure template exists
  template_path <- file.path(threeBrain::default_template_directory(), template_subject)
  if(!dir.exists(template_path)) {
    debug("Cannot find [fsaverage]: downloading...")
    threeBrain::download_template_subject("fsaverage")
  }
  template <- threeBrain::threeBrain(path = template_path, subject_code = template_subject)

  fs_path <- brain$base_path
  morph_path <- file.path(subject$preprocess_settings$raw_path, "rave-imaging", "morph-template")

  # create paths
  if(file.exists(morph_path)) {
    backup_file(morph_path, remove = TRUE, quiet = !verbose)
  }
  dir_create2(morph_path)

  # copy files
  path_orig_atlas <- file.path(morph_path, "aparc+aseg-orig.nii.gz")
  path_template_atlas <- file.path(morph_path, "aparc+aseg-template.nii.gz")
  t1_path <- file.path(fs_path, "mri", "T1.mgz")
  t1_path_tmp <- file.path(morph_path, "T1-orig.nii.gz")

  if( !file.exists(path_orig_atlas) ) {
    debug("Copying in native aparc+aseg.mgz into", basename(path_orig_atlas))
    mgh_to_nii(
      file.path(fs_path, "mri", "aparc+aseg.mgz"),
      path_orig_atlas
    )
  }
  if( !file.exists(path_template_atlas) ) {
    debug("Copying in", template_subject, "aparc+aseg.mgz into", basename(path_template_atlas))
    mgh_to_nii(
      file.path(template_path, "mri", "aparc+aseg.mgz"),
      path_template_atlas
    )
  }
  if( !file.exists(t1_path_tmp) ) {
    debug("Copying in", template_subject, "T1.mgz into T1-orig.nii.gz")
    mgh_to_nii( t1_path, t1_path_tmp )
  }

  debug("Loading ANTs")
  ants <- rpyANTs::load_ants()
  if(verbose) {
    print(ants)
  }

  # Step 0: get template in MNI305
  templateLPS_to_mniLPS <- diag(c(-1,-1,1,1)) %*% template$xfm %*% diag(c(-1,-1,1,1))
  tfile <- tempfile(fileext = ".mat")
  ants$write_transform( rpyANTs::as_ANTsTransform( solve(templateLPS_to_mniLPS) ), tfile )
  atlas_template <- rpyANTs::as_ANTsImage(path_template_atlas, strict = TRUE)
  atlas_mni <- rpyANTs::ants$apply_transforms(
    fixed = atlas_template,
    moving = atlas_template,
    interpolator = "nearestNeighbor",
    transformlist = tfile
  )
  rm(tfile)
  atlas_mni$to_file( file.path(morph_path, "aparc+aseg-template-affine_mni305.nii.gz") )


  # Step 1: from tkrLPS to MNI305 LPS
  # compute from LPS orig_atlas to LPS fsaverage_atlas
  # MNI305RAS = TalXFM*Norig*inv(Torig)*[tkrR tkrA tkrS 1]'
  scannerLPS_to_mniLPS <- diag(c(-1,-1,1,1)) %*% brain$xfm %*% diag(c(-1,-1,1,1))

  path_initial_transform <- file.path(morph_path, "initial_transform.mat")
  initial_transform <- rpyANTs::as_ANTsTransform( solve(scannerLPS_to_mniLPS) )
  ants$write_transform( initial_transform, path_initial_transform )

  # debug: sanity check
  # etable <- subject$get_electrode_table()
  # scannerLPS <- data.frame(x = -etable$T1R, y = -etable$T1A, z = etable$T1S)
  # rpyANTs::ants$apply_transforms_to_points(dim = 3L, points = scannerLPS, transformlist = path_initial_transform, whichtoinvert = list(TRUE))
  # etable[,c("MNI305_x", "MNI305_y", "MNI305_z")]

  atlas_orig <- rpyANTs::as_ANTsImage(path_orig_atlas, strict = TRUE)
  t1_orig <- rpyANTs::as_ANTsImage(t1_path_tmp)

  t1_sanity <- rpyANTs::ants$apply_transforms(
    fixed = t1_orig,
    moving = t1_orig,
    transformlist = path_initial_transform
  )
  t1_sanity$to_file(
    file.path(morph_path, "T1-orig-affine_mni305.nii.gz")
  )

  # resample orig into fsaverage (MNI305)
  debug("Transform native aparc+aseg to MNI305 space")
  altas_origin_in_mni <- rpyANTs::ants$apply_transforms(
    fixed = atlas_orig,
    moving = atlas_orig,
    transformlist = path_initial_transform,
    interpolator = "nearestNeighbor",
    verbose = verbose
  )

  debug("Saving aparc+aseg (MNI305) to aparc+aseg-orig-affine_mni305.nii.gz")
  altas_origin_in_mni$to_file(
    file.path(morph_path, "aparc+aseg-orig-affine_mni305.nii.gz")
  )
  # Sanity check:
  # Open and overlay the followings, they should align pretty well
  # aparc+aseg-orig-in-mni305.nii
  # t1-orig-in-mni305.nii
  # aparc+aseg-fsaverage.nii

  # rpyANTs::ants_plot_grid(list(atlas_template, altas_origin_in_mni),
  #                         slices = list(128L, 128L))

  # SyN transform (affine + SDR)
  debug("Aligning aparc+aseg (MNI305) to fsaverage using SyN")
  transform <- rpyANTs::ants_registration(
    fixed = atlas_mni, moving = altas_origin_in_mni,
    type_of_transform = "SyN",
    outprefix = file.path(morph_path, "ants-"),
    verbose = verbose,
    write_composite_transform = TRUE
  )

  debug("Saving morphed output to aparc+aseg-orig-morphed-to-template.nii.gz")
  transform$warpedmovout$to_file(
    file.path(morph_path, "aparc+aseg-orig-morphed-to-template.nii.gz")
  )

  # save configurations
  config <- list(
    origin = subject$subject_code,
    template = template_subject,
    initial_transform = basename(path_initial_transform),
    fwdtransforms = basename(rpyANTs::py_to_r(transform$fwdtransforms)),
    invtransforms = basename(rpyANTs::py_to_r(transform$invtransforms)),
    original = basename(path_orig_atlas),
    reference = basename(path_template_atlas),
    method = "SyNRA"
  )
  save_yaml(config, file = file.path(morph_path, "transform.yaml"))

  # sanity check: put T1 and morph
  t1_sanity <- rpyANTs::ants$apply_transforms(
    fixed = atlas_template,
    moving = t1_orig,
    transformlist = file.path(morph_path, c(
      config$fwdtransforms,
      config$initial_transform
    ))
  )

  t1_sanity$to_file(
    file.path(morph_path, "t1-orig-morphed-to-template.nii.gz")
  )

  morph_results <- tryCatch({
    ants_morph_electrode(subject, preview = preview, ...)
  }, error = function(e) {
    NULL
  })

  if(is.null(morph_results)) {
    invisible(morph_results)
  } else {
    morph_results
  }
}

#' @rdname ants_coreg
#' @export
cmd_run_ants_mri_to_template <- function(
    subject, template_subject = getOption("threeBrain.template_subject", "N27"),
    verbose = TRUE, dry_run = FALSE) {

  require_package("rpyANTs")

  # DIPSAUS DEBUG START
  # subject <- "devel/YAH"
  # template_subject <- "fsaverage"
  # dry_run = FALSE

  subject <- restore_subject_instance(subject, strict = FALSE)
  force(template_subject)
  force(dry_run)

  work_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging"),
    winslash = "/", mustWork = FALSE
  )

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-rave-ants-mri-to-template.R.log")

  template <- readLines(system.file("shell-templates/rave-ants-mri-to-template.R", package = "raveio"))

  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE, .null = "")

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts",
              "cmd-ants-mri-to-template.R"),
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
    subject = subject$subject_id,
    dry_run = dry_run,
    log_file = file.path(log_path, log_file, fsep = "/"),
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


#' @rdname ants_coreg
#' @export
ants_morph_electrode <- function(subject, preview = FALSE, dry_run = FALSE) {

  # DIPSAUS DEBUG START
  # subject <- "devel/PAV006"

  subject <- restore_subject_instance(subject, strict = FALSE)
  brain <- rave_brain(subject, use_141 = FALSE)
  if(is.null(brain)) { stop("Cannot morph electrodes. Please run FreeSurfer recon-all first") }

  electrode_table <- subject$get_electrode_table()
  coords <- electrode_table[, c("Coord_x", "Coord_y", "Coord_z")]

  # ANTs does not allow data frame with less than 2 rows
  overhead <- FALSE
  if(nrow(coords) <= 1 ) {
    overhead <- TRUE
    coords <- rbind(coords, data.frame(
      Coord_x = c(0, 0), Coord_y = c(0, 0), Coord_z = c(0, 0)
    ))
  }

  require_package("rpyANTs")
  # check morph_path
  morph_path <- file.path(subject$preprocess_settings$raw_path, "rave-imaging", "morph-template")
  if( !dir.exists(morph_path) ) {
    stop("Cannot find morph path. Please run the following first:\n  ",
         sprintf("raveio::ants_mri_to_template('%s')", subject$subject_id))
  }

  # get coords into scannerLPS
  coord_lps <- diag(c(-1,-1,1,1)) %*% brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*% rbind(t(as.matrix(coords)), 1)
  coord_lps <- as.data.frame(t(coord_lps[1:3, ]))
  names(coord_lps) <- c("x", "y", "z")

  # apply transform
  config <- load_yaml(file.path(morph_path, "transform.yaml"))
  # transform_initial <- file.path(morph_path, config$initial_transform)
  # transform_fwd <- file.path(morph_path, config$fwdtransforms)
  transform_inv <- file.path(morph_path, config$invtransforms)

  ants <- rpyANTs::load_ants()
  new_pos <- ants$apply_transforms_to_points(
    dim = 3L,
    points = coord_lps,
    transformlist = transform_inv
  )
  new_pos <- rpyANTs::py_to_r(new_pos)
  new_pos[rowSums(coords^2) == 0, ] <- 0
  coord_lps[rowSums(coords^2) == 0, ] <- 0
  distance <- sqrt(rowSums(as.matrix(new_pos - coord_lps)^2))
  new_pos$x <- -new_pos$x
  new_pos$y <- -new_pos$y

  if( overhead ) {
    new_pos <- new_pos[seq_len(nrow(new_pos) - 2), ]
    coord_lps <- coord_lps[seq_len(nrow(coord_lps) - 2), ]
    distance <- distance[seq_len(length(distance) - 2)]
  }

  # combine two
  morph_electrodes <- electrode_table[, c("Electrode", "Label", "Coord_x", "Coord_y", "Coord_z")]
  morph_electrodes$MorphMNI305R <- new_pos$x
  morph_electrodes$MorphMNI305A <- new_pos$y
  morph_electrodes$MorphMNI305S <- new_pos$z
  morph_electrodes$AffineMNI305R <- -coord_lps$x
  morph_electrodes$AffineMNI305A <- -coord_lps$y
  morph_electrodes$AffineMNI305S <- coord_lps$z
  morph_electrodes$MorphDistance <- distance

  if( !dry_run ) {
    electrode_table$MNI305_x <- morph_electrodes$MorphMNI305R
    electrode_table$MNI305_y <- morph_electrodes$MorphMNI305A
    electrode_table$MNI305_z <- morph_electrodes$MorphMNI305S
    electrode_table$MNI305MorphDistance <- distance
    save_meta2(data = electrode_table, meta_type = "electrodes",
               project_name = subject$project_name,
               subject_code = subject$subject_code)
  }

  if( preview ) {
    preview_brain <- threeBrain::threeBrain(
      path = brain$base_path,
      subject_code = subject$subject_code,
      template_subject = config$template
    )
    tbl <- morph_electrodes
    tbl$MNI305_x <- morph_electrodes$MorphMNI305R
    tbl$MNI305_y <- morph_electrodes$MorphMNI305A
    tbl$MNI305_z <- morph_electrodes$MorphMNI305S
    preview_brain$set_electrodes(tbl)
    preview_brain$set_electrode_values(morph_electrodes)
    template_brain <- threeBrain::merge_brain(preview_brain, template_subject = config$template)
    viewer <- template_brain$plot(
      additional_subjects = subject$subject_code,
      controllers = list(
        "Display Data" = "MorphDistance"
      ),
      val_ranges = list(
        "MorphDistance" = c(0, ceiling(min(max(distance, na.rm = TRUE), 10)))
      )
    )
    print(viewer)
  }

  if( dry_run ) {
    return( morph_electrodes )
  } else {
    return( invisible(morph_electrodes) )
  }

}

