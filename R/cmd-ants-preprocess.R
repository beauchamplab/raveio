#' @title Process 'T1' weighted 'MRI' using \code{ANTs}
#' @param work_path working directory, all intermediate images will be stored
#' here
#' @param image_path input image path
#' @param resample whether to resample the input image before processing
#' @param verbose whether to verbose the processing details
#' @param template_subject template mapping, default is derived from
#' \code{\link{raveio_getopt}}
#' @returns Nothing. All images are saved to \code{work_path}
#' @export
ants_preprocessing <- function(work_path, image_path, resample = FALSE, verbose = TRUE,
         template_subject = raveio_getopt("threeBrain_template_subject")) {
  # DIPSAUS DEBUG START
  # work_path <- "~/Desktop/junk2"
  # image_path <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/YAH/MRI/MRI_RAW.nii"
  # electrode_file <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/data/demo/YAH/rave/meta/electrodes.csv"
  # resample <- FALSE
  # verbose <- TRUE
  # template_subject = raveio::raveio_getopt("threeBrain_template_subject")

  # debug <- verbose && interactive()
  template_mapping <- "antsRegistrationSyNRepro[a]" #"a: Affine" # s: SYN (SDR)
  # System preparation
  ants <- rpyANTs::load_ants()
  # antspynet <- rpyANTs::load_antspynet()
  # py <- rpyANTs::import("__main__", convert = FALSE)

  # if(length(electrode_file) == 1 && file.exists(electrode_file)) {
  #   electrode_table <- data.matrix(read.csv(electrode_file)[, c("T1R","T1A","T1S")])
  # } else {
  #   electrode_table <- NULL
  # }

  visualize <- function(x, filename, overlay = NULL, overlay_cmap = "jet", overlay_alpha = 0.2, title = "") {
    try({
      x$plot(
        overlay = overlay,
        ncol = 4L,
        nslices = 12L,
        crop = TRUE,
        overlay_cmap = overlay_cmap,
        overlay_alpha = 0.2,
        title = title,
        dpi = 300L,
        filename = file.path(visualization_path, filename)
      )
    })
  }

  # ensure template
  ensure_template <- function(template_code) {
    template_mri_path <- file.path(threeBrain::default_template_directory(), template_code)
    if(!dir.exists(template_mri_path)) {
      threeBrain::download_template_subject(subject_code = template_code)
    }
    dname_prefix <- c("brain.finalsurfs", "brain", "T1")
    fnames <- rbind(
      sprintf("%s.mgz", dname_prefix),
      sprintf("%s.nii.gz", dname_prefix),
      sprintf("%s.nii", dname_prefix)
    )
    template_mri_path <- file.path(template_mri_path, "mri", as.vector(fnames))
    template_mri_path <- template_mri_path[file.exists(template_mri_path)]
    if(!length(template_mri_path)) {
      stop(sprintf("The template that you choose [%s] does not have T1 MRI matching search list", template_code))
    }
    template_mri_path[[1]]
  }
  template_subject_path <- ensure_template(template_subject)
  template_brain_object <- threeBrain::merge_brain(template_subject = template_subject)


  # prepare working directory
  work_path <- raveio::dir_create2(work_path)
  volume_path <- raveio::dir_create2(file.path(work_path, "mri"))
  transform_path <- raveio::dir_create2(file.path(volume_path, "transforms"))
  segmentation_path <- raveio::dir_create2(file.path(volume_path, "segmentation"))
  surface_path <- raveio::dir_create2(file.path(work_path, "surf"))
  subcortical_path <- raveio::dir_create2(file.path(surface_path, "subcortical"))
  visualization_path <- raveio::dir_create2(file.path(work_path, "visualization"))

  # Read in original image
  image_original <- rpyANTs::as_ANTsImage(image_path, strict = TRUE)
  image_original$to_file(file.path(volume_path, "orig.nii.gz"))
  input_image <- image_original


  # Resample
  if( resample ) {
    input_image <- rpyANTs::ants_resample_image(
      image_original,
      resample_params = c(256, 256, 256),
      use_voxels = TRUE,
      interp_type = "linear"
    )
  }
  input_image$to_file(file.path(volume_path, "resampled.nii.gz"))

  # Brain mask
  probability_mask <- rpyANTs::antspynet_brain_extraction(x = input_image, modality = "t1", verbose = verbose)
  brain_mask <- ants$threshold_image(probability_mask, 0.5, 1.0, 1L, 0L)
  brain_mask = ants$morphology(brain_mask, "close", 6L)$iMath_fill_holes()
  brain_mask$to_file(file.path(volume_path, "brain_mask.nii.gz"))

  # Skull-strip
  skull_strip <- input_image$clone()
  skull_strip[brain_mask < 0.5] <- 0L
  skull_strip$to_file(file.path(volume_path, "brain.nii.gz"))

  visualize(
    input_image,
    filename = "1.brain-extraction.png",
    overlay = skull_strip,
    overlay_cmap = "jet",
    overlay_alpha = 0.2,
    title = "RAVE-ANTs Brain extraction"
  )

  # MNI mapping
  template_brain <- rpyANTs::as_ANTsImage(template_subject_path)
  template_brain$to_file(file.path(transform_path, "template.nii.gz"))

  registration = rpyANTs::ants_registration(
    fixed = template_brain,
    moving = skull_strip,
    type_of_transform = template_mapping,
    verbose = verbose
  )

  fwdtransforms <- rpyANTs::py_to_r(registration$fwdtransforms)
  for(ii in seq_along(fwdtransforms)) {
    file.copy(fwdtransforms[[ii]], file.path(transform_path, sprintf("fwdtransforms-%d.mat", ii)), overwrite = TRUE)
  }
  # affine <- solve(rpyANTs::as_ANTsTransform(fwdtransforms)[])
  # xfm_path <- file.path(transform_path, "talairach.xfm")

  invtransforms <- rpyANTs::py_to_r(registration$invtransforms)
  for(ii in seq_along(invtransforms)) {
    file.copy(invtransforms[[ii]], file.path(transform_path, sprintf("invtransforms-%d.mat", ii)), overwrite = TRUE)
  }

  # morphed to MNI
  preprocessed_image = rpyANTs::ants_apply_transforms(
    fixed = template_brain,
    moving = skull_strip,
    verbose = verbose,
    transformlist = registration$fwdtransforms,
    interpolator = "linear"
  )
  preprocessed_image$to_file(file.path(transform_path, "morphed.nii.gz"))

  visualize(
    template_brain,
    filename = "2.native-morphed-to-template.png",
    overlay = preprocessed_image,
    overlay_cmap = "jet",
    overlay_alpha = 0.4,
    title = "RAVE-ANTs native overlaid on MNI brain (Affine)"
  )

  # calculate affine matrix
  native_lps <- data.frame(
    x = c(0,-1,0,0),
    y = c(0,0,-1,0),
    z = c(0,0,0,1)
  )
  template_lps <- rpyANTs::py_to_r(ants$apply_transforms_to_points(
    dim = 3L,
    points = native_lps,
    transformlist = registration$fwdtransforms,
    verbose = verbose, whichtoinvert = list(TRUE)))

  native_ras <- matrix(byrow = FALSE, ncol = 4, data = c(
    0, 0, 0, 1,
    1, 0, 0, 1,
    0, 1, 0, 1,
    0, 0, 1, 1
  ))
  template_ras <- rbind(
    -template_lps$x,
    -template_lps$y,
    template_lps$z,
    1
  )

  # template_xfm %*% template_ras = xfm %*% native_ras

  xfm <- (template_brain_object$template_object$xfm) %*% template_ras %*% solve(native_ras)
  writeLines(
    con = file.path(transform_path, "talairach.xfm"),
    c(
      "MNI Transform File",
      "% avi2talxfm", "",
      "Transform_Type = Linear;",
      "Linear_Transform = ",
      paste(sprintf("%.6f", xfm[1, ]), collapse = " "),
      paste(sprintf("%.6f", xfm[2, ]), collapse = " "),
      paste0(paste(sprintf("%.6f", xfm[3, ]), collapse = " "), ";")
    )
  )

  # Image segmentation - Atropos
  atropos_segmentation <- rpyANTs::antspynet_deep_atropos(
    x = input_image,
    do_preprocessing = TRUE,
    use_spatial_priors = TRUE,
    aseg_only = FALSE,
    verbose = verbose
  )
  atropos_path <- raveio::dir_create2(file.path(segmentation_path, "atropos"))
  atropos_segmentation$segmentation_image$to_file(file.path(atropos_path, "aseg-atropos.nii.gz"))
  prob_images <- atropos_segmentation$probability_images
  prob_images[1]$to_file(file.path(atropos_path, "prob-atropos-1-csf.nii.gz"))
  prob_images[2]$to_file(file.path(atropos_path, "prob-atropos-2-gray-matter.nii.gz"))
  prob_images[3]$to_file(file.path(atropos_path, "prob-atropos-3-white-matter.nii.gz"))
  prob_images[4]$to_file(file.path(atropos_path, "prob-atropos-4-deep-gray-matter.nii.gz"))
  prob_images[5]$to_file(file.path(atropos_path, "prob-atropos-5-brain-stem.nii.gz"))
  prob_images[6]$to_file(file.path(atropos_path, "prob-atropos-6-cerebellum.nii.gz"))

  visualize(
    input_image,
    filename = "3.atropos.png",
    overlay = atropos_segmentation$segmentation_image,
    overlay_cmap = "jet",
    overlay_alpha = 0.6,
    title = "RAVE-ANTs Deep Atropos Segmentation"
  )

  # Image segmentation & parcellation - DKT
  DKTatlas_segmentation <- rpyANTs::antspynet_desikan_killiany_tourville_labeling(
    x = input_image,
    do_preprocessing = TRUE,
    return_probability_images = FALSE,
    do_lobar_parcellation = TRUE,
    verbose = verbose
  )

  dkt_path <- raveio::dir_create2(file.path(segmentation_path, "DKTatlas"))
  DKTatlas_segmentation$segmentation_image$to_file(file.path(dkt_path, "aparc.DKTatlas+aseg.nii.gz"))
  DKTatlas_segmentation$segmentation_image$to_file(file.path(volume_path, "aparc.DKTatlas+aseg.nii.gz"))
  DKTatlas_segmentation$segmentation_image$to_file(file.path(volume_path, "aparc+aseg.nii.gz"))
  DKTatlas_segmentation$lobar_parcellation$to_file(file.path(dkt_path, "DKTatlas-lobar.nii.gz"))


  visualize(
    input_image,
    filename = "4.DKT-atlas.png",
    overlay = DKTatlas_segmentation$segmentation_image,
    overlay_cmap = "viridis",
    overlay_alpha = 0.6,
    title = "RAVE-ANTs Deep DKT Parcellation"
  )

  # Generate surface
  DTK_lobar <- DKTatlas_segmentation$lobar_parcellation

  cortical_mask_left <- DTK_lobar$threshold_image(low_thresh=1L, high_thresh=4L, inval=1L, outval=0L)
  cortical_mask_left[brain_mask < 0.5] <- 0L
  cortical_mask_left[prob_images[1] > 0.5] <- 0L

  cortical_mask_right <- DTK_lobar$threshold_image(low_thresh=7L, high_thresh=10, inval=1L, outval=0L)
  cortical_mask_right[brain_mask < 0.5] <- 0L
  cortical_mask_right[prob_images[1] > 0.5] <- 0L

  cortical_mask_left$to_file(file.path(volume_path, "mask_left.nii.gz"))
  cortical_mask_right$to_file(file.path(volume_path, "mask_right.nii.gz"))

  # pial
  pial_mask <- prob_images[2]$threshold_image(0.5, 1.0, 1L, 0L)
  pial_mask_left = pial_mask$clone()
  pial_mask_left[cortical_mask_left < 0.5] = 0
  # pial_mask_left <- pial_mask_left$morphology("close", 1L)$iMath_fill_holes()

  pial_mask_right = pial_mask$clone()
  pial_mask_right[cortical_mask_right < 0.5] = 0
  # pial_mask_right <- pial_mask_right$morphology("close", 1L)$iMath_fill_holes()

  pial_mask_left$to_file(file.path(volume_path, "mask_pial_left.nii.gz"))
  pial_mask_right$to_file(file.path(volume_path, "mask_pial_right.nii.gz"))

  # white matter
  white_matter <- prob_images[3]$threshold_image(0.5, 1.0, 1L, 0L)

  white_matter_left = white_matter$clone()
  white_matter_left[cortical_mask_left < 0.5] = 0

  white_matter_right = white_matter$clone()
  white_matter_right[cortical_mask_right < 0.5] = 0

  white_matter_left$to_file(file.path(volume_path, "mask_white_left.nii.gz"))
  white_matter_right$to_file(file.path(volume_path, "mask_white_right.nii.gz"))

  # generate surfaces
  IJK2tkrRAS <- threeBrain::get_ijk2ras(input_image, type = "tkr")

  generate_surface <- function(mask, name, smooth_delta = 2.5) {
    surface <- ravetools::mesh_from_volume(
      volume = mask[],
      output_format = "freesurfer",
      IJK2RAS = IJK2tkrRAS,
      threshold = 0.5,
      verbose = verbose,
      remesh = TRUE,
      remesh_voxel_size = 1.5,
      smooth = TRUE,
      smooth_delta = smooth_delta,
      smooth_lambda = 2
    )
    freesurferformats::write.fs.surface(
      filepath = file.path(surface_path, name),
      vertex_coords = surface$vertices,
      faces = surface$faces,
      format = "bin"
    )
  }
  generate_surface(cortical_mask_left, "lh.pial-coarse")
  generate_surface(cortical_mask_right, "rh.pial-coarse")
  generate_surface(white_matter_left, "lh.white-coarse")
  generate_surface(white_matter_right, "rh.white-coarse")

  dkt_labels <- sort(unique(as.vector(
    threeBrain::read.nifti1.data(filepath = file.path(volume_path, "aparc.DKTatlas+aseg.nii.gz"))
  )))
  dkt_labels <- dkt_labels[!dkt_labels %in% 0]

  raveio::lapply_async(dkt_labels, function(lbl) {
    try({
      threeBrain::generate_subcortical_surface(
        atlas = file.path(volume_path, "aparc.DKTatlas+aseg.nii.gz"), index = lbl,
        save_prefix = subcortical_path, smooth_delta = 2.5, remesh = TRUE, smooth = TRUE
      )
    })
    NULL
  }, callback = function(lbl) {
    sprintf("Generating ROI surfaces|DKT: %s", lbl)
  })

  try({
    brain <- threeBrain::threeBrain(
      path = work_path,
      subject_code = "TEMP",
      atlas_types = "aparc.DKTatlas+aseg"
    )
    threeBrain::save_brain(
      brain$plot(),
      directory = file.path(visualization_path, "5.viewer3D-with-MRI"),
    )
  })

  try({
    brain <- threeBrain::threeBrain(
      path = work_path,
      subject_code = "TEMP",
      surface_types = seq(1000, 3000),
      atlas_types = "aparc.DKTatlas+aseg"
    )
    threeBrain::save_brain(
      brain$plot(),
      directory = file.path(visualization_path, "6.viewer3D-with-DKT"),
      title = "Surfaces estimated from DKT-Atlas"
    )
  })


  invisible()

}
