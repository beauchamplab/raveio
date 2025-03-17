#' Load 'FreeSurfer' or 'AFNI/SUMA' brain from 'RAVE'
#' @description Create 3D visualization of the brain and visualize with
#' modern web browsers
#' @param subject character, list, or \code{\link{RAVESubject}} instance; for
#' list or other objects, make sure \code{subject$subject_id} is a valid 'RAVE'
#' subject 'ID'
#' @param surfaces one or more brain surface types from \code{"pial"},
#' \code{"white"}, \code{"smoothwm"}, \code{"pial-outer-smoothed"}, etc.;
#' check \code{\link[threeBrain]{freesurfer_brain2}}
#' @param overlays volumes to overlay; default is \code{'aparc.a2009s+aseg'}
#' @param annotations surface annotation or curvature data to load;
#' default is \code{'label/aparc.a2009s'}, referring to the
#' \code{'*h.aparc.a2009s.annot'} under the label folder.
#' @param usetemplateifmissing whether to use template brain when the subject
#' brain files are missing. If set to true, then a template (usually 'N27')
#' brain will be displayed as an alternative solution, and electrodes will be
#' rendered according to their \code{'MNI305'} coordinates, or
#' \code{'VertexNumber'} if given.
#' @param include_electrodes whether to include electrode in the model; default
#' is true
#' @param ... ignored, reserved for legacy code
#' @returns A \code{'threeBrain'} instance if brain is found or
#' \code{usetemplateifmissing} is set to true; otherwise returns \code{NULL}
#' @examples
#'
#'
#' # Please make sure DemoSubject is correctly installed
#' # The subject is ~1GB from Github
#'
#' if(interactive()){
#'   brain <- rave_brain("demo/DemoSubject")
#'
#'   if( !is.null(brain) ) { brain$plot() }
#'
#' }
#'
#'
#' @export
rave_brain <- function(
    subject, surfaces = 'pial', overlays = "aparc.a2009s+aseg", annotations = "label/aparc.a2009s",
    ..., usetemplateifmissing = FALSE, include_electrodes = TRUE){

  subject <- as_rave_subject(subject, strict = FALSE)

  fs_path <- subject$freesurfer_path

  electrode_table <- NULL
  if(include_electrodes) {
    electrode_table <- subject$meta_data(meta_type = "electrodes")
    if(!is.data.frame(electrode_table) || !nrow(electrode_table)) {
      electrode_table <- NULL
    }
  }


  if(is.na(fs_path) || !isTRUE(dir.exists(fs_path))){
    if( !usetemplateifmissing ){
      return(invisible())
    }
    brain <- threeBrain::merge_brain(
      template_surface_types = surfaces,
      template_atlas_types = overlays,
      template_annotation_types = annotations
    )

    if(is.data.frame(electrode_table)) {
      # try to use MNI305 position
      if(all(paste0("MNI305_", c("x", "y", "z")) %in% names(electrode_table))){
        electrode_table$Coord_x <- electrode_table$MNI305_x
        electrode_table$Coord_y <- electrode_table$MNI305_y
        electrode_table$Coord_z <- electrode_table$MNI305_z
      }

      brain$set_electrodes(electrodes = electrode_table)
    }

  } else {
    # if(recache){
    #   if( clean_before_cache ){
    #     fs <- list.files(file.path(fs_path, 'RAVE'), pattern = '\\.json$',
    #                     all.files = FALSE, recursive = FALSE, full.names = TRUE,
    #                     ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
    #     lapply(fs, unlink)
    #   }
    #   threeBrain::import_from_freesurfer(fs_path, subject_name = subject$subject_code)
    # }

    brain <- threeBrain::threeBrain(
      path = fs_path,
      subject_code = subject$subject_code,
      surface_types = surfaces,
      atlas_types = overlays,
      annotation_types = annotations
    )


    if(is.data.frame(electrode_table)) {
      brain$set_electrodes(electrodes = electrode_table)
    }

    # if( compute_template ){
    #   tf <- tempfile()
    #   new_table <- brain$calculate_template_coordinates(save_to = tf)
    #   if( file.exists(tf) ){
    #     brain$electrodes$raw_table_path <- NULL
    #     unlink(tf)
    #     # need to update meta
    #     save_meta2(
    #       data = new_table,
    #       meta_type = 'electrodes',
    #       project_name = subject$project_name,
    #       subject_code = subject$subject_code
    #     )
    #   }
    # }

  }

  brain$meta$constructor_params <- list(
    project_name = subject$project_name,
    subject_code = subject$subject_code,
    use_141 = FALSE,
    usetemplateifmissing = usetemplateifmissing
  )

  brain
}


transform_point_to_template_surface <- function(subject, scan_ras_mat, hemisphere, use_surface = "pial", template = NA, flip_hemisphere = FALSE, verbose = TRUE, ...) {

  if(length(template) != 1 || is.na(template) || !nzchar(template)) {
    template <- "fsaverage"
    # make sure the template exists
    template_imaging_path <- file.path(threeBrain::default_template_directory(), template)
    if(!dir.exists(template_imaging_path)) {
      threeBrain::download_template_subject(subject_code = template)
    }
  }

  subject <- restore_subject_instance(subject, strict = FALSE)

  if(tolower(use_surface) == "pial.t1") {
    use_surface <- "pial"
  }
  native_brain <- rave_brain(subject, surfaces = c("pial", "pial.T1", "sphere.reg", use_surface))
  template_brain <- threeBrain::merge_brain(template_subject = template)
  template_brain <- template_brain$template_object

  if(verbose) {
    catgl("Using `{use_surface}` surface and template `{template}` to transform points to fsaverage (MNI305) space",
          level = "INFO")
  }

  if(length(scan_ras_mat) == 3) {
    scan_ras_mat <- matrix(scan_ras_mat, ncol = 3)
  }
  stopifnot2(is.matrix(scan_ras_mat) && ncol(scan_ras_mat) == 3, msg = "The matrix `scan_ras_mat` must be nx3 dimension")
  n_contacts <- nrow(scan_ras_mat)

  if(missing(hemisphere)) {
    # auto
    hemisphere <- rep("a", n_contacts)
  } else {
    hemisphere <- substr(tolower(hemisphere), 1L, 1L)
    if(length(hemisphere) == 1) {
      hemisphere <- rep(hemisphere, n_contacts)
    }
  }

  stopifnot2(length(hemisphere) == n_contacts, msg = "Argument `hemisphere` must have length 1 or equaling to the number of contacts")

  valid_positions <- rowSums(is.na(scan_ras_mat)) == 0

  tkr_ras_mat <- cbind(scan_ras_mat, 1) %*% t(native_brain$Torig %*% solve(native_brain$Norig))

  get_surface <- function(brain, surface_type, hemisphere) {
    surf_object <- brain$surfaces[[surface_type]]
    if( !length(surf_object) ) {
      stop("Cannot find surface ", sQuote(surface_type), " from the imaging files. Did you run FreeSurfer reconstruction?")
    }
    if(hemisphere == "l") {
      dset_name <- sprintf("free_vertices_FreeSurfer Left Hemisphere - %s (%s)", surf_object$surface_type, brain$subject_code)
    } else {
      dset_name <- sprintf("free_vertices_FreeSurfer Right Hemisphere - %s (%s)", surf_object$surface_type, brain$subject_code)
    }

    cache_info <- surf_object$group$group_data[[dset_name]]

    surface_path <- cache_info$absolute_path
    surface <- threeBrain::read.fs.surface(surface_path)
    surface$faces <- surface$faces - min(surface$faces) + 1L
    surface
  }

  native_lh_surf <- get_surface(native_brain, use_surface, "l")
  native_rh_surf <- get_surface(native_brain, use_surface, "r")
  native_lh_spreg <- get_surface(native_brain, "sphere.reg", "l")
  native_rh_spreg <- get_surface(native_brain, "sphere.reg", "r")

  template_lh_surf <- get_surface(template_brain, "pial", "l")
  template_rh_surf <- get_surface(template_brain, "pial", "r")
  template_lh_spreg <- get_surface(template_brain, "sphere.reg", "l")
  template_rh_spreg <- get_surface(template_brain, "sphere.reg", "r")

  if(flip_hemisphere) {
    native_brain$add_surface("sphere")
    native_lh_sp <- get_surface(native_brain, "sphere", "l")
    native_rh_sp <- get_surface(native_brain, "sphere", "r")
  } else {
    native_lh_sp <- NULL
    native_rh_sp <- NULL
  }

  template_tkr2mni305 <- template_brain$xfm %*% template_brain$Norig %*% solve(template_brain$Torig)
  native_tkr2mni305 <- native_brain$xfm %*% native_brain$Norig %*% solve(native_brain$Torig)
  mni305_to_152 <- MNI305_to_MNI152

  mapping <- lapply(seq_len(n_contacts), function(ii) {
    is_valid <- valid_positions[[ii]]
    if(!is_valid) {
      return(list(
        Sphere_x = NA_real_,
        Sphere_y = NA_real_,
        Sphere_z = NA_real_,

        MNI152_x = NA_real_,
        MNI152_y = NA_real_,
        MNI152_z = NA_real_,

        MNI305_x = NA_real_,
        MNI305_y = NA_real_,
        MNI305_z = NA_real_,

        DistanceShifted = NA_real_
      ))
    }

    hemi <- hemisphere[[ii]]

    tkr_ras <- tkr_ras_mat[ii, seq_len(3)]

    if( !hemi %in% c("l", "r") ) {
      mni305_vol <- native_tkr2mni305 %*% c(tkr_ras, 1.0)
      if(isTRUE(mni305_vol[[1]] > 0)) {
        hemi <- "r"
      } else {
        hemi <- "l"
      }
    }

    if(flip_hemisphere) {
      if( hemi == "l" ) {
        native_surf <- native_lh_surf
        native_sphere <- native_lh_sp
        native_sphere_flipped <- native_rh_sp

        native_spherereg <- native_rh_spreg
        template_surf <- template_rh_surf
        template_spherereg <- template_rh_spreg
      } else {
        native_surf <- native_rh_surf
        native_sphere <- native_rh_sp
        native_sphere_flipped <- native_lh_sp

        native_spherereg <- native_lh_spreg
        template_surf <- template_lh_surf
        template_spherereg <- template_lh_spreg
      }
    } else {
      if( hemi == "l" ) {
        native_surf <- native_lh_surf
        native_spherereg <- native_lh_spreg
        native_sphere <- native_lh_sp
        native_sphere_flipped <- native_rh_sp
        template_surf <- template_lh_surf
        template_spherereg <- template_lh_spreg
      } else {
        native_surf <- native_rh_surf
        native_spherereg <- native_rh_spreg
        native_sphere <- native_rh_sp
        native_sphere_flipped <- native_lh_sp
        template_surf <- template_rh_surf
        template_spherereg <- template_rh_spreg
      }
    }


    # calculate native distance to surface
    dir <- t(native_surf$vertices[, seq_len(3), drop = FALSE]) - tkr_ras
    native_dist_to_surf <- sqrt(colSums((dir)^2))
    native_node_id <- which.min(native_dist_to_surf)

    distance_shifted <- native_dist_to_surf[[native_node_id]]

    if(flip_hemisphere) {
      sphere_xyz_native <- native_sphere$vertices[native_node_id, seq_len(3)]
      sphere_xyz_native[[1]] <- -sphere_xyz_native[[1]]
      dir <- t(native_sphere_flipped$vertices[, seq_len(3), drop = FALSE]) - sphere_xyz_native
      # now native_node_id is for the opposite cortex
      native_node_id <- which.min(sqrt(colSums((dir)^2)))
    }

    sphere_xyz <- native_spherereg$vertices[native_node_id, seq_len(3)]

    # also calculate template coordinate
    template_sphere_dist <- sqrt(colSums(
      (
        t(template_spherereg$vertices[, seq_len(3), drop = FALSE]) - sphere_xyz
      )^2
    ))
    template_node_id <- which.min(template_sphere_dist)
    template_tkr_ras <- template_surf$vertices[template_node_id, seq_len(3)]

    template_mni305 <- template_tkr2mni305 %*% c(template_tkr_ras, 1)
    template_mni152 <- mni305_to_152 %*% template_mni305

    list(
      Sphere_x = sphere_xyz[[1]],
      Sphere_y = sphere_xyz[[2]],
      Sphere_z = sphere_xyz[[3]],

      MNI152_x = template_mni152[[1]],
      MNI152_y = template_mni152[[2]],
      MNI152_z = template_mni152[[3]],

      MNI305_x = template_mni305[[1]],
      MNI305_y = template_mni305[[2]],
      MNI305_z = template_mni305[[3]],

      DistanceShifted = distance_shifted
    )
  })

  data.table::rbindlist(mapping)

}


transform_point_to_template_volumetric <- function(subject, scan_ras_mat, method = c("auto", "affine", "nonlinear"), flip_hemisphere = FALSE, verbose = TRUE, ...) {

  method <- match.arg(method)

  subject <- restore_subject_instance(subject, strict = FALSE)

  if( verbose ) {
    verbose <- TRUE
  } else {
    verbose <- FALSE
  }

  if(length(scan_ras_mat) == 3) {
    scan_ras_mat <- matrix(scan_ras_mat, ncol = 3)
  }
  stopifnot2(is.matrix(scan_ras_mat) && ncol(scan_ras_mat) == 3, msg = "The matrix `scan_ras_mat` must be nx3 dimension")
  # n_contacts <- nrow(scan_ras_mat)

  valid_positions <- rowSums(is.na(scan_ras_mat)) == 0

  if(!any(valid_positions)) {
    re <- scan_ras_mat
    re[] <- NA_real_
    return(re)
  }

  native_brain <- rave_brain(subject)

  template_name <- "mni_icbm152_nlin_asym_09b"
  if(method %in% c("auto", "nonlinear")) {
    # check if nonlinear exists
    mapping <- tryCatch({
      yael <- YAELProcess$new(subject_code = subject$subject_code)
      mapping <- yael$get_template_mapping(template_name = "mni_icbm152_nlin_asym_09b")
      if(!length(mapping)) {
        mapping <- yael$get_template_mapping(template_name = "mni_icbm152_nlin_asym_09a")
        template_name <- "mni_icbm152_nlin_asym_09a"
      }
      if(!length(mapping)) {
        mapping <- yael$get_template_mapping(template_name = "mni_icbm152_nlin_asym_09c")
        template_name <- "mni_icbm152_nlin_asym_09c"
      }
      mapping
    }, error = function(e) {
      NULL
    })

    if(length(mapping)) {
      method <- "nonlinear"
    } else {
      if(method == "nonlinear") {
        stop("Unable to map points from native to template using non-linear method. Missing non-linear deformation files or Python for RAVE is not configured properly.")
      }
      method <- "affine"
    }
  }

  if( method == "nonlinear") {
    if(verbose) {
      catgl("Using non-linear volumetric mapping to transform points to MNI152 space",
            level = "INFO")
    }

    # map to template using non-linear deformation
    yael <- YAELProcess$new(subject_code = subject$subject_code)


    scan_ras_selected <- scan_ras_mat[valid_positions, , drop = FALSE]
    if(nrow(scan_ras_selected) == 1) {
      # ANTs does not like it when there is one row
      scan_ras_selected <- rbind(scan_ras_selected, scan_ras_selected)
    }
    mni152_ras <- yael$transform_points_to_template(native_ras = scan_ras_selected,
                                                    template_name = template_name,
                                                    verbose = FALSE)
    tmp <- scan_ras_mat
    tmp[valid_positions, ] <- mni152_ras[seq_len(sum(valid_positions)), ]
    tmp[!valid_positions, ] <- NA_real_
    mni152_ras <- tmp
  } else {
    if(verbose) {
      catgl("Using affine matrix (volumetric) for mapping points to MNI152 space",
            level = "INFO")
    }
    mni152_ras <- native_brain$electrodes$apply_transform_points(
      positions = scan_ras_mat, from = "scannerRAS", to = "MNI152")
    mni152_ras[!valid_positions, ] <- NA_real_
  }

  if( flip_hemisphere ) {
    # for those who wish to display the electrodes on the same side
    # rare but this is MNI space, for demo purposes (mostly :)
    mni152_ras[, 1] <- -mni152_ras[, 1]
  }

  mni305_ras <- cbind(mni152_ras, 1) %*% t(solve(MNI305_to_MNI152))

  data.table::data.table(
    MNI152_x = mni152_ras[, 1],
    MNI152_y = mni152_ras[, 2],
    MNI152_z = mni152_ras[, 3],

    MNI305_x = mni305_ras[, 1],
    MNI305_y = mni305_ras[, 2],
    MNI305_z = mni305_ras[, 3]
  )

}

#' Calculate template 'MNI' coordinates for points on native brain
#' @param subject 'RAVE' subject
#' @param positions optional matrix of 3 columns, either in scanner or surface
#' space (specified by \code{space}); default is missing and will use the
#' electrode localization results (\code{electrodes.csv})
#' @param space if \code{positions} is given, which native coordinate system
#' should be used; default is native 'T1' (or \code{'scannerRAS'}); alternative
#' is 'FreeSurfer' surface coordinate (or \code{'tkrRAS'})
#' @param mapping_method whether the mapping is \code{'volumetric'} or
#' \code{'surface'}; default is the former.
#' @param flip_hemisphere whether to flip the hemisphere; default is
#' \code{FALSE}
#' @param verbose whether to verbose the mapping progress; default is true
#' @param project_surface for surface mapping only, which surface to project
#' electrodes onto; default is \code{'pial'} surface, other common choices
#' are \code{'white'} for white-matter, or \code{'smoothwm'} for smoothed
#' white matter
#' @param volumetric_transform for volume mapping only, which type of transform
#' to use; default is \code{'auto'} detecting and use non-linear deformation
#' if exists, and fall back to 'affine' transform; other choices are
#' \code{'affine'} or \code{'nonlinear'}
#' @param ... ignored
#' @returns A table of electrode 'MNI' coordinates.
#' @examples
#'
#' if(interactive()) {
#'
#' transform_point_to_template('demo/DemoSubject', mapping_method = "volumetric")
#'
#' }
#'
#' @export
transform_point_to_template <- function(
    subject, positions, space = c("scannerRAS", "tkrRAS"),
    mapping_method = c("volumetric", "surface"), flip_hemisphere = FALSE,
    verbose = TRUE, project_surface = "pial",
    volumetric_transform = c("auto", "affine", "nonlinear"), ...) {

  volumetric_transform <- match.arg(volumetric_transform)

  subject <- restore_subject_instance(subject, strict = FALSE)
  native_brain <- rave_brain(subject, usetemplateifmissing = FALSE)
  if(is.null(native_brain)) {
    stop("Unable to find the rave-imaging folder for subject ", subject$subject_id)
  }

  if(missing(positions)) {
    positions <- as.matrix(native_brain$electrodes$raw_table[, c("Coord_x", "Coord_y", "Coord_z")])
    invalid_positions <- rowSums(positions^2) == 0
    positions[invalid_positions, ] <- NA_real_
    space <- "tkrRAS"
  } else {
    space <- match.arg(space)
    positions <- as.matrix(positions)
    if(!length(positions)) {
      stop("No valid `positions` found.")
    }
    if(length(positions) == 3) {
      positions <- matrix(positions, ncol = 3L)
    }
  }
  if( space == "tkrRAS" ) {
    positions <- native_brain$electrodes$apply_transform_points(
      positions = positions, from = "tkrRAS", to = "scannerRAS")
  }


  mapping_method <- match.arg(mapping_method)
  if(mapping_method == "volumetric") {
    res <- transform_point_to_template_volumetric(subject = subject, scan_ras_mat = positions, flip_hemisphere = flip_hemisphere, verbose = verbose, method = volumetric_transform, ...)
  } else {
    res <- transform_point_to_template_surface(subject = subject, scan_ras_mat = positions, flip_hemisphere = flip_hemisphere, verbose = verbose, use_surface = project_surface, ...)
  }

  return(res)

}


#' @rdname transform_point_to_template
#' @param interpolator whether the transform lean towards volume mapping
#' (\code{interpolator=0}) or surface mapping (\code{interpolator=1})
#' @param n_segments positive integers with length of two: resolution of the
#' mapping; default segments the thin-film array into 16 by 16 segments
#' @param group_labels \code{NULL} (default) or a character vector indicating
#' the group labels of thin-film electrodes;
#' default assumes that all contacts are from thin-film electrodes.
#' @param template_subject template subject to be mapped to; default is
#' \code{'cvs_avg35_inMNI152'}, which is a \code{'MNI152'} template generated
#' by 'FreeSurfer'; other choices are \code{'fsaverage'} and \code{'bert'}
#' @export
transform_thinfilm_to_mni152 <- function(
    subject, flip_hemisphere = FALSE, interpolator = 0.3,
    n_segments = c(16, 16), group_labels = NULL, project_surface = "pial",
    volumetric_transform = c("auto", "affine", "nonlinear"),
    template_subject = c("cvs_avg35_inMNI152", "fsaverage", "bert", "MNI152")) {

  volumetric_transform <- match.arg(volumetric_transform)
  template_subject <- match.arg(template_subject)
  if(template_subject == "MNI152") {
    template_subject <- "cvs_avg35_inMNI152"
  }

  # DIPSAUS DEBUG START
  # devtools::load_all()
  # subject <- "YAEL/Precision003"
  # list2env(list(flip_hemisphere = FALSE, interpolator = 0.7,
  #               n_segments = c(16, 16), group_labels = NULL, project_surface = "pial",
  #               volumetric_transform = "affine"), envir=.GlobalEnv)
  # template_subject <- "cvs_avg35_inMNI152"

  subject <- restore_subject_instance(subject, strict = FALSE)

  electrode_table <- subject$get_electrode_table()
  if(!is.data.frame(electrode_table) || nrow(electrode_table) == 0) {
    stop("No valid electrode table")
  }

  # brain objects
  native_brain <- rave_brain(subject, usetemplateifmissing = FALSE)
  if(is.null(native_brain)) {
    stop("Unable to find the rave-imaging folder for subject ", subject$subject_id)
  }
  # MNI152 mapping uses cvs_avg35_inMNI152 template
  template_file_path <- file.path(threeBrain::default_template_directory(), template_subject)
  if(!dir.exists(template_file_path)) {
    threeBrain::download_template_subject(template_subject)
  }
  template <- threeBrain::merge_brain(native_brain, template_subject = template_subject)
  template$template_object$add_annotation("label/aparc.a2009s")

  # some variables being reused again and again
  colnames_coordxyz <- sprintf("Coord_%s", c("x", "y", "z"))

  # from tkr-RAS to scanner-RAS (native and template)
  native_tkr2scan <- native_brain$Norig %*% solve(native_brain$Torig)
  template_tkr2scan <- template$template_object$Norig %*% solve(template$template_object$Torig)

  # from native tkr-RAS to MNI152 (only valid for this template)
  native_tkr_to_template_tkr <- template_tkr2scan %*% solve(template$template_object$xfm) %*% native_brain$xfm %*% native_tkr2scan

  # template from MNI305 to template-T1 (template space)
  template_mni305_to_scan <- solve(template$template_object$xfm)

  # load pial surface of the template brain
  template_pial_group <- template$template_object$surfaces$pial$group$group_data
  template_dset_lh_pial <- sprintf("free_vertices_FreeSurfer Left Hemisphere - pial (%s)",
                                   template$template_subject)
  template_dset_rh_pial <- sprintf("free_vertices_FreeSurfer Right Hemisphere - pial (%s)",
                                   template$template_subject)
  template_lh_pial <- freesurferformats::read.fs.surface(template_pial_group[[template_dset_lh_pial]]$absolute_path)
  template_rh_pial <- freesurferformats::read.fs.surface(template_pial_group[[template_dset_rh_pial]]$absolute_path)

  template_lh_pial_scan_ras <- template_tkr2scan %*% t(cbind(template_lh_pial$vertices, 1))
  template_lh_pial_face_index <- t(template_lh_pial$faces)
  template_lh_pial_face_index <- template_lh_pial_face_index - min(template_lh_pial_face_index) + 1L

  template_rh_pial_scan_ras <- template_tkr2scan %*% t(cbind(template_rh_pial$vertices, 1))
  template_rh_pial_face_index <- t(template_rh_pial$faces)
  template_rh_pial_face_index <- template_rh_pial_face_index - min(template_rh_pial_face_index) + 1L

  template_lh_annot <- ieegio::read_surface(template_pial_group[["lh_annotation_label/aparc.a2009s"]]$absolute_path)
  template_rh_annot <- ieegio::read_surface(template_pial_group[["rh_annotation_label/aparc.a2009s"]]$absolute_path)

  # Load sphere.reg of the template brain
  template_spherereg_group <- template$template_object$surfaces$sphere.reg$group$group_data
  template_dset_lh_spherereg <- sprintf("free_vertices_FreeSurfer Left Hemisphere - sphere.reg (%s)", template$template_subject)
  template_dset_rh_spherereg <- sprintf("free_vertices_FreeSurfer Right Hemisphere - sphere.reg (%s)", template$template_subject)
  template_lh_spherereg <- freesurferformats::read.fs.surface(template_spherereg_group[[template_dset_lh_spherereg]]$absolute_path)
  template_rh_spherereg <- freesurferformats::read.fs.surface(template_spherereg_group[[template_dset_rh_spherereg]]$absolute_path)

  # get geometry configurations
  geometry_names_full <- sprintf("%s_%s", electrode_table$Prototype, electrode_table$LabelPrefix)
  geometry_names <- unique(geometry_names_full)
  if(length(group_labels)) {
    geometry_names <- geometry_names[geometry_names %in% group_labels]
  }
  if(!length(geometry_names)) {
    stop("No valid geometry object found. Please make sure the column `Prototype` and `LabelPrefix` are valid in electrodes.csv. Available group labels are: ", paste(sQuote(unique(geometry_names_full)), collapse = ", "))
  }



  # Do this for each electrode geometry
  new_electrode_table <- lapply(geometry_names, function(geometry_name) {

    # geometry_name <- geometry_names[[1]]
    geometry <- native_brain$electrodes$geometries[[geometry_name]]

    # get contact locations in surface coordinates
    electrode_selection <- geometry_names_full == geometry_name
    tkr_ras <- as.matrix(electrode_table[electrode_selection, colnames_coordxyz])

    # get contact positions in model space
    model_positions <- solve(geometry$transform) %*% t(cbind(tkr_ras, 1))

    # get bounding box range at model space (only the first 2 dimensions as z is 0)
    model_bounding_box <- apply(model_positions[c(1, 2), , drop = FALSE], 1, range)

    # segment model_bounding_box into small boxes specified by `n_segments`
    segment_spacing <- (model_bounding_box[2, ] - model_bounding_box[1, ]) / c(n_segments)

    # construct iterator to iterate segments
    segment_design <- expand.grid(
      column = seq_len(n_segments[[1]] + 1),
      row = seq_len(n_segments[[2]] + 1)
    )

    # matrix from segment index to model space
    segment_to_model <- diag(c(segment_spacing, 0, 1))
    segment_to_model[1:2, 4] <- model_bounding_box[1, 1:2] - segment_spacing

    # get corner coordinates (model space) for each segment
    # apply(segment_corners_model_ras,1,range)[, 1:2] should be model_bounding_box[, 1:2]
    # ncol(segment_corners_model_ras) should be prod(n_segments + 1)
    segment_corners_model_ras <- segment_to_model %*% t(cbind(as.matrix(segment_design), 1, 1))

    segment_corners_tkr_ras <- geometry$transform %*% segment_corners_model_ras
    segment_corners_tkr_ras_transposed <- t(segment_corners_tkr_ras[1:3, , drop = FALSE])

    # calculate the corners in MNI152 (surface & volume)
    segment_corners_mni152_surface <- as.data.frame(
      transform_point_to_template(
        subject = subject,
        positions = segment_corners_tkr_ras_transposed,
        space = "tkrRAS",
        mapping_method = "surface",
        flip_hemisphere = flip_hemisphere,
        project_surface = project_surface
      )
    )
    segment_corners_mni152_volume <- as.data.frame(
      transform_point_to_template(
        subject = subject,
        positions = segment_corners_tkr_ras_transposed,
        space = "tkrRAS",
        mapping_method = "volumetric",
        flip_hemisphere = flip_hemisphere,
        volumetric_transform = volumetric_transform
      )
    )

    # determine the hemisphere and the template surfaces to be used
    if(isTRUE(mean(segment_corners_mni152_volume$MNI152_x, na.rm = TRUE) > 0)) {
      electrode_hemisphere <- "right"
      template_surface <- template_rh_pial
      template_scan_ras <- template_rh_pial_scan_ras
      template_face_index <- template_rh_pial_face_index
      template_spherereg <- template_rh_spherereg
      template_annot <- template_rh_annot
    } else {
      electrode_hemisphere <- "left"
      template_surface <- template_lh_pial
      template_scan_ras <- template_lh_pial_scan_ras
      template_face_index <- template_lh_pial_face_index
      template_spherereg <- template_lh_spherereg
      template_annot <- template_lh_annot
    }

    # get the coordinates on the templates
    # volumetric
    sc_template_scan_volu_transposed <- template_mni305_to_scan %*% rbind(t(as.matrix(segment_corners_mni152_volume[, c("MNI305_x", "MNI305_y", "MNI305_z")])), 1)
    sc_template_scan_volu_transposed <- sc_template_scan_volu_transposed[1:3, , drop = FALSE]
    # surface
    kdtree <- ravetools::vcg_kdtree_nearest(
      target = template_spherereg,
      query = as.matrix(segment_corners_mni152_surface[, c("Sphere_x", "Sphere_y", "Sphere_z"), drop = FALSE]),
      k = 1
    )
    sc_template_scan_surf_transposed <- t(template_surface$vertices[kdtree$index, 1:3, drop = FALSE])
    # need to transform them into template scanner space
    sc_template_scan_surf_transposed <- (template_tkr2scan %*% rbind(sc_template_scan_surf_transposed, 1))[1:3, , drop = FALSE]

    # interpolate the contacts - prepare the data in row-major matrix form
    # sc_mni152_surf_transposed <- t(as.matrix(segment_corners_mni152_surface[, c("MNI152_x", "MNI152_y", "MNI152_z")]))
    # sc_mni152_volu_transposed <- t(as.matrix(segment_corners_mni152_volume[, c("MNI152_x", "MNI152_y", "MNI152_z")]))

    # interpolate the contacts - calculate MNI coordinates for each contact
    # mni152_interpolated is a 6xn matrix with first 3 rows surface-registered
    # locations and then volumetric locations
    mni152_interpolated <- apply(model_positions, 2, function(model_pos) {
      # model_pos <- model_positions[,278]
      fct <- (model_pos[1:2] - model_bounding_box[1, 1:2]) / segment_spacing
      if(any(fct < 0) || any(fct > n_segments)) {
        return(c(0, 0, 0))
      }

      segment_id1 <- floor(fct)
      segment_id2 <- ceiling(fct)

      # segment_design[corner1_id, ]
      corner1_id <- sum(segment_id1 * c(1, n_segments[[1]] + 1)) + 1
      corner2_id <- sum(c(segment_id1[[1]], segment_id2[[2]]) * c(1, n_segments[[1]] + 1)) + 1
      corner3_id <- sum(segment_id2 * c(1, n_segments[[1]] + 1)) + 1
      corner4_id <- sum(c(segment_id2[[1]], segment_id1[[2]]) * c(1, n_segments[[1]] + 1)) + 1

      interpolator <- fct - segment_id1

      isomorphic_interpolation <- function(positions) {
        vec1 <- (positions[, corner4_id] - positions[, corner1_id]) * interpolator[[1]]
        vec2 <- (positions[, corner2_id] - positions[, corner1_id]) +
          (positions[, corner3_id] - positions[, corner2_id]) * interpolator[[1]]
        (vec2 - vec1) * interpolator[[2]] + vec1 + positions[, corner1_id]
      }

      # This should be c(0, 0)
      # model_pos - isomorphic_interpolation(positions = model_control_points)

      # contact MNI152, interpolated
      mni_sur <- isomorphic_interpolation(sc_template_scan_surf_transposed)
      mni_vol <- isomorphic_interpolation(sc_template_scan_volu_transposed)
      # sphere_sur <- isomorphic_interpolation(sc_sphere_surf_transposed)

      c(mni_sur, mni_vol) #, sphere_sur[[4]])
    })


    # Thinfilm ECoG model normals is 0,0,1, then to tkr then to MNI152
    template_normals <- native_tkr_to_template_tkr %*% geometry$transform %*% c(0, 0, 1, 0)

    # normalize so the length of vector `template_normals` is 1
    template_normals <- template_normals[1:3] / sqrt(sum(template_normals[1:3])^2)

    # Interpolate and raycast the contacts to surface
    # interpolate: interpolator=0 is pure volumetric, interpolator=1 is pure surface
    mni152_center <- interpolator * mni152_interpolated[1:3, , drop = FALSE] + (1 - interpolator) * mni152_interpolated[4:6, , drop = FALSE]

    # calculate vector from volumetric to surface mapping
    mni152_surf_vol_dir <- mni152_interpolated[4:6, , drop = FALSE] - mni152_interpolated[1:3, , drop = FALSE]

    # normalize mni152_surf_vol_dir
    mni152_surf_vol_len <- sqrt(colSums(mni152_surf_vol_dir^2))
    sel <- mni152_surf_vol_len > 0
    mni152_surf_vol_dir_normalized <- mni152_surf_vol_dir
    mni152_surf_vol_dir_normalized[, sel] <- mni152_surf_vol_dir[, sel] / mni152_surf_vol_len[sel]

    # project direction, make sure the projection is "outwards" from inner brain to outer
    # this is ensured by calculating inner prod with template_normals, since template_normals is
    # always outwards
    is_same_direction <- as.vector(template_normals %*% mni152_surf_vol_dir_normalized > 0)
    mni152_surf_vol_dir_normalized[, !is_same_direction] <- -mni152_surf_vol_dir_normalized[, !is_same_direction]

    # TODO: re-think if using `interpolator` is appropriate here
    # if interpolator=0 (vol mapping), mapping projection is template_normals
    ray_direction <- interpolator * mni152_surf_vol_dir_normalized + 2 * template_normals[1:3]
    ray_length <- sqrt(colSums(ray_direction^2))
    sel <- ray_length > 0
    ray_direction[, sel] <- t(t(ray_direction[, sel]) / ray_length[sel])

    # determine the maximum offset to avoid mapping contacts to sulci
    if(!is.null(template_annot)) {
      # Heuristic of how much offsets if the direction is inner (avoid mapping contacts to the sulci)
      offsets <- abs(template_normals %*% mni152_surf_vol_dir)
      offsets <- sort(offsets)
      offset_length <- length(offsets)
      offset_idx1 <- max(floor(0.2 * offset_length), 1)
      offset_idx2 <- min(ceiling(0.8 * offset_length), offset_length)
      shift_cap <- offsets[[offset_idx1]] * interpolator + offsets[[offset_idx2]] * (1 - interpolator)
    } else {
      # using annotations
      annot_labels <- tolower(template_annot$annotations$label_table$Label)
      sel <- !startsWith(annot_labels, "s_") & !annot_labels %in% c("unknown")
      # select annotation keys that marked "gyrus"
      keys <- template_annot$annotations$label_table$Key[sel]
      gyrus_roi <- template_surface$vertices[template_annot$annotations$data_table[[1]] %in% keys, 1:3, drop = FALSE]
      kdtree <- ravetools::vcg_kdtree_nearest(as.matrix(gyrus_roi), query = t(mni152_center), k = 5)
      distance_est <- stats::quantile(kdtree$distance, 0.75)
      project_distances <- sapply(seq_len(ncol(ray_direction)), function(ii) {
        ray_dir <- ray_direction[, ii]
        orig_pos <- mni152_center[, ii]
        sapply(kdtree$index[ii, ], function(idx) {
          sum((gyrus_roi[idx, ] - orig_pos) * template_normals)
        })
        # dist <- kdtree$distance[ii, ]
        # sel <- dist <= distance_est
        # if(any(sel)) {
        #   idx <- kdtree$index[ii, sel][[which.max(dist[sel])[[1]]]]
        # } else {
        #   idx <- kdtree$index[ii, which.min(dist)[[1]]]
        # }
        # sum((gyrus_roi[idx, ] - mni152_center[, ii]) * ray_direction[, ii])
        # ray_dir <- ray_direction[, ii]
      })

      shift_cap <- -stats::quantile(project_distances, 0.25)
    }


    # raycast: smash contacts to the pial surface



    raycast <- ravetools::vcg_raycaster(
      structure(
        class = "mesh3d",
        list(
          vb = template_scan_ras[1:3, , drop = FALSE],
          it = template_face_index - min(template_face_index) + 1L
        )
      ),
      ray_origin = mni152_center,
      ray_direction = ray_direction,
      both_sides = TRUE
    )

    # ensure the raycaster distance is not too large
    raycast$distance[raycast$distance < -shift_cap] <- -shift_cap

    raycast_offset <- t(raycast$ray_direction) * raycast$distance

    # pop the contacts to the surface by 0.8mm
    # this is because the electrodes underneath the surface if rendered via phong/standard shaders
    mni152_projected <- mni152_center + t(raycast_offset)# + template_normals * 0.8
    mni152_projected[, !raycast$has_intersection] <- mni152_center[, !raycast$has_intersection]

    # also calculate MNI305 position
    mni305_interpolated <- template$template_object$xfm %*% rbind(
      mni152_projected[1:3, , drop = FALSE], 1)


    new_electrode_table <- electrode_table[electrode_selection, , drop = FALSE]
    new_electrode_table$MNI305_x <- mni305_interpolated[1, ]
    new_electrode_table$MNI305_y <- mni305_interpolated[2, ]
    new_electrode_table$MNI305_z <- mni305_interpolated[3, ]
    new_electrode_table$MNI152_x <- mni152_projected[1, ]
    new_electrode_table$MNI152_y <- mni152_projected[2, ]
    new_electrode_table$MNI152_z <- mni152_projected[3, ]
    new_electrode_table$Radius <- geometry$contact_sizes

    new_electrode_table
  })

  new_electrode_table <- do.call("rbind", new_electrode_table)

  new_electrode_table <- new_electrode_table[order(new_electrode_table$Electrode), , drop = FALSE]

  # scene <-
  #   r3js::plot3js(
  #     xlim = range(new_electrode_table$MNI152_x) + c(-1, 1),
  #     ylim = range(new_electrode_table$MNI152_y) + c(-1, 1),
  #     zlim = range(new_electrode_table$MNI152_z) + c(-1, 1),
  #     label_axes = FALSE, draw_box = FALSE, draw_grid = FALSE,
  #     aspect = c(1, 1, 1) # Maintain a constant aspect ratio
  #   ) |>
  #   r3js::light3js(
  #     position = c(0, 0, 1),
  #     type = "directional",
  #     intensity = 1,
  #     col = "white"
  #   ) |>
  #   r3js::light3js(type = "ambient",
  #                  intensity = 0.2,
  #                  col = "white") |>
  #   r3js::shape3js(
  #     vertices = t(template_lh_pial_scan_ras[1:3, , drop = FALSE]),
  #     faces = t(template_lh_pial_face_index),
  #     col = "gray",
  #     # opacity = 0.6,
  #     mat = "phong"
  #   ) |>
  #   r3js::shape3js(
  #     vertices = t(template_rh_pial_scan_ras[1:3, , drop = FALSE]),
  #     faces = t(template_rh_pial_face_index),
  #     col = "gray",
  #     # opacity = 0.6,
  #     mat = "phong"
  #   ) |>
  #   r3js::points3js(
  #     x = new_electrode_table$MNI152_x,
  #     y = new_electrode_table$MNI152_y,
  #     z = new_electrode_table$MNI152_z,
  #     size = new_electrode_table$Radius * 5,
  #     # opacity = rep(0.0, nrow(new_electrode_table)),
  #     # col = dipsaus::col2hexStr(pal[color_index], alpha = 0.3),
  #     col = "red",
  #     geometry = FALSE
  #   ) |>
  #   # r3js::points3js(
  #   #   x = sc_mni152_surf_transposed[1, ],
  #   #   y = sc_mni152_surf_transposed[2, ],
  #   #   z = sc_mni152_surf_transposed[3, ],
  #   #   size = rep(1, ncol(sc_mni152_surf_transposed)),
  #   #   # opacity = rep(0.0, nrow(new_electrode_table)),
  #   #   # col = dipsaus::col2hexStr(pal[color_index], alpha = 0.3),
  #   #   col = "blue",
  #   #   geometry = FALSE
  #   # ) |>
  #   # r3js::arrows3js(
  #   #   from = t(mni152_center),
  #   #   to = as.matrix(new_electrode_table[, sprintf("MNI152_%s", c("x", "y", "z"))]),
  #   #   lwd = 0.01, arrowhead_width = 0, arrowhead_length = 0,
  #   #   col = "gray"
  #   # ) |>
  #   r3js::r3js(
  #     rotation = c(0, 0, 0)
  #   )
  # print(scene)

  return(new_electrode_table)

  # native_brain$set_electrodes(new_electrode_table, priority = "sphere")
  # native_brain$set_electrode_values(new_electrode_table)
  # # brain$plot()
  # template$plot()

}
