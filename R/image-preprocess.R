#' @name yael-image-processing
#' @title 'YAEL' imaging processing
#' @description
#' For preliminary process, aligns 'T1' to template image, performs 'FreeSurfer'
#' reconstruction (if supported), register 'MRI' to 'CT'; for post-process,
#' calculate the electrode coordinates in template coordinate system, and
#' map atlases or 'ROI' to native brain.
#' @param subject_code 'RAVE' subject ID or subject code (default project will
#' be \code{'YAEL'})
#' @param t1_path path to 'T1' image
#' @param ct_path path to 'CT' image
#' @param templates template image to align 'T1'; currently supports
#' \code{'mni_icbm152_nlin_asym_09a'} and \code{'mni_icbm152_nlin_asym_09b'}
#' @param freesurfer whether to run 'FreeSurfer' command, default is
#' automatically determined; will enable on 'Linux' and 'OSX' and when the
#' image directory is empty, and disable on 'Windows' or the 'FreeSurfer' folder
#' has been previously generated.
#' @param verbose whether to verbose the progress; default is true
#' @param launch_rave whether to launch 'RAVE' after process
#' @param wait_recon whether to wait for 'FreeSurfer' reconstruction to finish
#' before launching 'RAVE'
#' @param roi_folder 'ROI' or atlas folder in template
#' @param parallel whether to run registration and 'T1' processing at the same
#' time; default is false. Turn it on to save time, but only available when
#' running in 'RStudio'
#' @returns \code{yael_preprocess_t1} and \code{yael_preprocess} returns 'bash'
#' command if 'FreeSurfer' is not available, or launches 'FreeSurfer' in the
#' background if available.
#' @examples
#'
#' \dontrun{
#'
#' library(raveio)
#' yael_preprocess(
#'   subject_code = "YAB",
#'   t1_path = "~/YAB_T1.nii",
#'   ct_path = "~/YAB_CT.nii",
#'   templates = "mni_icbm152_nlin_asym_09a",
#'   launch_rave = TRUE
#' )
#'
#' # run after localization
#' yael_postprocess(
#'   subject_code = "YAB",
#'   roi_folder = "~/template_roi/harvardox_amygdala_R_bin.nii.gz",
#'   templates = "mni_icbm152_nlin_asym_09a"
#' )
#'
#'
#' }
#'
NULL

#' @rdname yael-image-processing
#' @export
yael_preprocess_t1 <- function(subject_code, t1_path, templates = "mni_icbm152_nlin_asym_09a", freesurfer = NA, verbose = TRUE) {

  # DIPSAUS DEBUG START
  # subject_code = "Liming"
  # t1_path <- "~/rave_data/raw_dir/yael_demo_001/rave-imaging/derivative/MRI_reference.nii.gz"
  # templates = "mni_icbm152_nlin_asym_09a"
  # freesurfer=NA
  # verbose = TRUE
  # ct_path <- "~/rave_data/raw_dir/yael_demo_001/rave-imaging/derivative/CT_RAW.nii.gz"

  additional_commands <- NULL

  if(!grepl("/", subject_code)) {
    subject_code <- sprintf("YAEL/%s", subject_code)
  }
  subject <- restore_subject_instance(subject_code, strict = FALSE)
  work_path <- subject$imaging_path

  # Map T1 to templates
  rpyANTs::t1_preprocess(t1_path = t1_path, templates = templates, work_path = work_path, verbose = verbose)

  # Freesurfer
  fs_path <- file.path(work_path, "fs")
  fs_path_valid <- threeBrain::check_freesurfer_path(fs_path, autoinstall_template = FALSE, return_path = FALSE, check_volume = TRUE, check_surface = FALSE)

  if(isTRUE(freesurfer)) {
    fs_needed <- TRUE
  } else if (isFALSE(freesurfer) || get_os() == "windows") {
    fs_needed <- FALSE
  } else {
    fs_needed <- !fs_path_valid
  }

  if(fs_needed) {

    fs_home <- cmd_freesurfer_home(error_on_missing = FALSE, unset = NA)
    if(is.na(fs_home)) {
      fs_home <- "recon-all"
      dry_run <- TRUE
    } else {
      dry_run <- FALSE
    }

    if( dry_run ) {
      overwrite <- TRUE
    } else {
      # run recon-all -autorecon1
      fs_cmd <- cmd_run_recon_all(
        subject = subject,
        mri_path = t1_path,
        overwrite = TRUE,
        args = "-autorecon1",
        dry_run = FALSE,
        command_path = fs_home,
        verbose = verbose
      )
      overwrite <- FALSE
      if(is.list(fs_cmd) && length(fs_cmd$script_path) == 1 &&
         file.exists(fs_cmd$script_path)) {
        file.copy(fs_cmd$script_path,
                  file.path(dirname(fs_cmd$script_path), "cmd-fs-autorecon1.sh"),
                  overwrite = TRUE)
      }
    }

    fs_cmd <- cmd_run_recon_all(
      subject = subject,
      mri_path = t1_path,
      overwrite = overwrite,
      args = "-all",
      dry_run = TRUE,
      command_path = fs_home,
      verbose = FALSE
    )
    dir_create2(dirname(fs_cmd$script_path))
    dir_create2(dirname(fs_cmd$log_file))

    writeLines(fs_cmd$script, con = fs_cmd$script_path)

    cmd <- sprintf("'bash' '%s'", normalize_path(fs_cmd$script_path))
    if(dry_run || !dipsaus::rs_avail(child_ok = TRUE, shiny_ok = TRUE)) {
      additional_commands <- cmd
    } else {
      check <- dipsaus::rs_exec(
        name = sprintf("recon-all (%s)", subject$subject_code),
        bquote({
          system(.(cmd))
        }),
        quoted = TRUE,
        rs = TRUE,
        as_promise = FALSE,
        wait = FALSE,
        focus_on_console = TRUE,
        nested_ok = TRUE
      )
      catgl("Started recon-all in the background. Please do NOT close RStudio", level = "info")

      return(check)

    }
  }


  if(length(additional_commands)) {
    catgl("Please run the following bash command to finish FreeSurfer recon-all:\n\n  {paste(additional_commands, collapse='\n')}", level = "info")
  }

  invisible(additional_commands)
}

#' @rdname yael-image-processing
#' @export
yael_coregistration <- function(subject_code, ct_path, t1_path = NA, verbose = TRUE) {
  if(!grepl("/", subject_code)) {
    subject_code <- sprintf("YAEL/%s", subject_code)
  }
  subject <- restore_subject_instance(subject_code, strict = FALSE)
  work_path <- subject$imaging_path

  if(is.na(t1_path)) {
    # try to determine the MRI
    t1_path <- c(
      file.path(work_path, "fs", "mri", "T1.mgz"),
      file.path(work_path, "fs", "mri", "T1.nii.gz"),
      file.path(work_path, "fs", "mri", "T1.nii"),
      file.path(work_path, "ants", "mri", "T1.nii.gz"),
      file.path(work_path, "inputs", "MRI", "MRI_RAW.nii.gz")
    )
    t1_path <- t1_path[file.exists(t1_path)]
    if(!length(t1_path)) {
      stop("Cannot find T1 image from this subject. Please explicitly specify the path to T1 image.")
    }
  }
  t1_path <- normalize_path(t1_path[[1]], must_work = TRUE)
  ct_path <- normalize_path(ct_path, must_work = TRUE)

  # back up CT
  ct_dirpath <- dir_create2(file.path(work_path, "inputs", "CT"))
  fname <- tolower(filenames(ct_path))
  if(endsWith(fname, "gz")) {
    fname <- "CT_RAW.nii.gz"
  } else {
    fname <- "CT_RAW.nii"
  }
  ct_path2 <- file.path(ct_dirpath, fname)
  file.copy(ct_path, file.path(ct_dirpath, fname))
  ct_path2 <- normalize_path(ct_path2)

  rpyANTs::halpern_register_ct_mri(
    fixed = ct_path,
    moving = t1_path,
    outprefix = file.path(work_path, "coregistration", "ct_mri_coreg_"),
    fixed_is_ct = TRUE,
    verbose = verbose
  )

  file.copy(
    from = file.path(work_path, "coregistration", "ct_mri_coreg_orig_fixed.nii.gz"),
    to = file.path(work_path, "coregistration", "CT_RAW.nii.gz"),
    overwrite = TRUE
  )
  file.copy(
    from = file.path(work_path, "coregistration", "ct_mri_coreg_orig_moving.nii.gz"),
    to = file.path(work_path, "coregistration", "MRI_mov.nii.gz"),
    overwrite = TRUE
  )
  file.copy(
    from = file.path(work_path, "coregistration", "ct_mri_coreg_Warped.nii.gz"),
    to = file.path(work_path, "coregistration", "t1_in_ct.nii.gz"),
    overwrite = TRUE
  )

  deriv_path <- dir_create2(file.path(work_path, "derivative"))
  file.copy(
    from = file.path(work_path, "coregistration", "CT_RAW.nii.gz"),
    to = file.path(deriv_path, "CT_RAW.nii.gz"),
    overwrite = TRUE
  )
  file.copy(
    from = file.path(work_path, "coregistration", "t1_in_ct.nii.gz"),
    to = file.path(deriv_path, "t1_in_ct.nii.gz"),
    overwrite = TRUE
  )
  file.copy(
    from = file.path(work_path, "coregistration", "ct_mri_coreg_CT_IJK_to_MR_RAS.txt"),
    to = file.path(deriv_path, "transform-ctIJK2mrRAS.txt"),
    overwrite = TRUE
  )
  file.copy(
    from = file.path(work_path, "coregistration", "ct_mri_coreg_CT_RAS_to_MR_RAS.txt"),
    to = file.path(deriv_path, "transform-ctRAS2mrRAS.txt"),
    overwrite = TRUE
  )

  # config <- yaml::read_yaml("~/rave_data/raw_dir/alexis/rave-imaging/derivative/conf-coregistration.yaml")
  config <- list(
    `Heads up` = "Do NOT edit this file",
    profile = "MRI coregister to CT",
    work_path = work_path,
    timestamp = strftime(Sys.time(), "%a %b %d %H:%M:%S %Z %Y", tz = "UTC"),
    command = list(
      comment = "This is R function call using Halpern's pipeline. The MRI is coregistered into CT.",
      execute = "raveio::t1_ct_coregistration"
    ),
    input_image = list(
      type = "nifti",
      path = ct_path2,
      backup = c(
        "./derivative/CT_RAW.nii.gz",
        "./coregistration/CT_RAW.nii.gz"
      ),
      comment = "original CT image file as the fixing image"
    ),
    reference_image = list(
      type = "nifti",
      path = t1_path,
      backup = "./coregistration/MRI_mov.nii.gz",
      comment = "MR image file as the moving image"
    ),
    outputs = list(
      t1_in_ct = list(
        type = "nifti",
        path = "./coregistration/t1_in_ct.nii.gz",
        backup = "./derivative/t1_in_ct.nii.gz",
        comment = "re-sampled MRI; the resolution is the same as fixing CT"
      ),
      CT_IJK_to_MR_RAS = list(
        type = "transform",
        dimension = "4x4",
        path = "./coregistration/ct_mri_coreg_CT_IJK_to_MR_RAS.txt",
        backup = "./coregistration/transform-ctIJK2mrRAS.txt",
        transform_from = list(volume = "input_image", coordinate_system = "IJK"),
        transform_to = list(
          volume = "reference_image",
          space = "scanner",
          coordinate_system = "RAS"
        ),
        comment = "From voxel IJK coordinate to MRI scanner RAS coordinate"
      ),
      CT_RAS_to_MR_RAS = list(
        type = "transform",
        dimension = "4x4",
        path = "./coregistration/ct_mri_coreg_CT_RAS_to_MR_RAS.txt",
        backup = "./coregistration/transform-ctRAS2mrRAS.txt",
        transform_from = list(
          volume = "input_image",
          space = "scanner (CT)",
          coordinate_system = "RAS"
        ),
        transform_to = list(
          volume = "reference_image",
          space = "scanner",
          coordinate_system = "RAS"
        ),
        comment = "From CT scanner RAS coordinate to MRI scanner RAS coordinate"
      )
    )
  )

  save_yaml(config, file.path(deriv_path, "conf-coregistration.yaml"))

}


#' @rdname yael-image-processing
#' @export
yael_preprocess <- function(subject_code, t1_path, ct_path, templates = "mni_icbm152_nlin_asym_09a", freesurfer = NA, verbose = TRUE, parallel = TRUE, launch_rave = FALSE, wait_recon = FALSE) {

  rs_avail <- dipsaus::rs_avail(child_ok = TRUE, shiny_ok = TRUE)

  if(!grepl("/", subject_code)) {
    subject_code <- sprintf("YAEL/%s", subject_code)
  }
  subject <- restore_subject_instance(subject_code, strict = FALSE)

  if( parallel && rs_avail ) {
    check1 <- dipsaus::rs_exec(
      name = sprintf("YAEL coregistration (%s)", subject$subject_code),
      wait = FALSE, quoted = TRUE, rs = TRUE, as_promise = FALSE,
      focus_on_console = TRUE, nested_ok = TRUE,
      bquote({
        raveio <- asNamespace("raveio")
        raveio$yael_coregistration(
          .(subject$subject_id),
          t1_path = .(t1_path),
          ct_path = .(ct_path),
          verbose = .(verbose)
        )
      })
    )
    check2 <- yael_preprocess_t1(
      subject_code = subject$subject_id,
      t1_path = t1_path,
      templates = templates,
      freesurfer = freesurfer,
      verbose = verbose
    )
    check1()
  } else {
    yael_coregistration(
      subject$subject_id,
      t1_path = t1_path,
      ct_path = ct_path,
      verbose = verbose
    )
    check2 <- yael_preprocess_t1(
      subject_code = subject$subject_id,
      t1_path = t1_path,
      templates = templates,
      freesurfer = freesurfer,
      verbose = verbose
    )
  }

  if( wait_recon && is.function(check2) ) {
    check2()
  }

  if(launch_rave) {
    try({
      rave <- asNamespace("rave")
      rave$start_rave2(as_job = TRUE)
    }, silent = TRUE)
  }

  if(is.function(check2)) {
    return(check2)
  } else if ( is.character(check2) ) {
    catgl("Please run the following bash command to finish FreeSurfer recon-all:\n\n  {paste(check2, collapse='\n')}", level = "info")
  }

}

#' @rdname yael-image-processing
#' @export
yael_postprocess <- function(subject_code, roi_folder = NULL, templates = "mni_icbm152_nlin_asym_09a", verbose = TRUE) {
  if(!grepl("/", subject_code)) {
    subject_code <- sprintf("YAEL/%s", subject_code)
  }
  subject <- restore_subject_instance(subject_code, strict = FALSE)
  work_path <- subject$imaging_path

  available_templates <- list.dirs(file.path(work_path, "template_mapping"), full.names = FALSE, recursive = FALSE)
  templates <- templates[templates %in% available_templates]

  # Apply transforms to ROI from template to T1
  if(length(roi_folder) && length(templates)) {
    roi_folder <- roi_folder[file.exists(roi_folder)]
    if(length(roi_folder)) {

      lapply_async(templates, function(template_name) {
        try({
          template_mapping_prefix <- file.path(work_path, "template_mapping", template_name, sprintf("%s_", template_name))
          rpyANTs::halpern_apply_transform_template_mri(
            roi_folder = roi_folder,
            outprefix = template_mapping_prefix,
            verbose = FALSE
          )
          return(template_mapping_prefix)
        })
        return(NULL)
      }, callback = function(template_name) {
        sprintf("Morphing ROI from [%s]", template_name)
      })

    }
  }

  # calculate MNI coordinates (nonlinear)
  template_urls <- asNamespace('rpyANTs')$template_urls
  if(length(available_templates)) {
    templates <- c(templates, available_templates)
    templates <- templates[templates %in% names(template_urls)]
  }
  if(!length(templates)) { return(invisible()) }

  template_name <- templates[[1]]
  template_coord_sys <- template_urls[[template_name]]$coord_sys
  template_mapping_prefix <- file.path(work_path, "template_mapping", template_name, sprintf("%s_", template_name))

  transforms <- normalize_path(
    sprintf(
      "%s%s", template_mapping_prefix,
      c(
        "affine_0GenericAffine.mat",
        "deformable_0GenericAffine.mat",
        "deformable_1InverseWarp.nii.gz"
      )),
    must_work = TRUE
  )

  brain <- rave_brain(subject)
  if(is.null(brain)) { return(invisible()) }
  electrode_table <- brain$electrodes$raw_table
  if(!nrow(electrode_table)) { return(invisible()) }

  t1_ras <- electrode_table[, c("T1R", "T1A", "T1S")]
  if(!length(t1_ras)) { return(invisible()) }

  invalids <- rowSums(t1_ras^2) == 0

  # ANTs uses LPS instead of RAS
  t1_lps <- data.frame(
    x = -t1_ras$T1R,
    y = -t1_ras$T1A,
    z = t1_ras$T1S
  )

  mni_lps <- rpyANTs::ants_apply_transforms_to_points(
    dim = 3,
    points = t1_lps,
    transformlist = transforms,
    whichtoinvert = c(TRUE, TRUE, FALSE),
    verbose = verbose
  )

  mni_lps <- data.matrix(mni_lps)
  mni_lps[invalids, ] <- 0

  if(template_coord_sys == "MNI152") {
    electrode_table$MNI152_x <- -mni_lps[,1]
    electrode_table$MNI152_y <- -mni_lps[,2]
    electrode_table$MNI152_z <- mni_lps[,3]

    mni305 <- solve(raveio::MNI305_to_MNI152) %*% rbind(-mni_lps[,1], -mni_lps[,2], mni_lps[,3], 1)
    electrode_table$MNI305_x <- mni305[1, ]
    electrode_table$MNI305_y <- mni305[2, ]
    electrode_table$MNI305_z <- mni305[3, ]
  } else {
    electrode_table$MNI305_x <- -mni_lps[,1]
    electrode_table$MNI305_y <- -mni_lps[,2]
    electrode_table$MNI305_z <- mni_lps[,3]

    mni152 <- raveio::MNI305_to_MNI152 %*% rbind(-mni_lps[,1], -mni_lps[,2], mni_lps[,3], 1)
    electrode_table$MNI152_x <- mni152[1, ]
    electrode_table$MNI152_y <- mni152[2, ]
    electrode_table$MNI152_z <- mni152[3, ]
  }
  brain$set_electrodes(electrode_table)

  save_meta2(data = electrode_table, meta_type = "electrodes", project_name = subject$project_name, subject_code = subject$subject_code)


  invisible(brain)
}


# raveio::yael_preprocess(
#   subject_code = "Liming",
#   t1_path = "~/rave_data/raw_dir/mayo/rave-imaging/coregistration/T1_defaced.nii.gz",
#   ct_path = "~/rave_data/raw_dir/mayo/rave-imaging/coregistration/CT_defaced.nii.gz",
#   templates = "mni_icbm152_nlin_asym_09a",
#   launch_rave = TRUE
# )
