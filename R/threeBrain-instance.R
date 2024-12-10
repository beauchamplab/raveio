#' Load 'FreeSurfer' or 'AFNI/SUMA' brain from 'RAVE'
#' @description Create 3D visualization of the brain and visualize with
#' modern web browsers
#' @param subject character, list, or \code{\link{RAVESubject}} instance; for
#' list or other objects, make sure \code{subject$subject_id} is a valid 'RAVE'
#' subject 'ID'
#' @param surfaces one or more brain surface types from \code{"pial"},
#' \code{"white"}, \code{"smoothwm"}, \code{"pial-outer-smoothed"}, etc.;
#' check \code{\link[threeBrain]{freesurfer_brain2}}
#' @param use_141 whether to use 'AFNI/SUMA' standard 141 brain
#' @param recache whether to re-calculate cache; only should be used when
#' the original 'FreeSurfer' or 'AFNI/SUMA' files are changed; such as new
#' files are added
#' @param clean_before_cache whether to clean the original cache before
#' \code{recache}; only set it to be true if original cached files are
#' corrupted
#' @param compute_template whether to compute template mappings; useful when
#' template mapping with multiple subjects are needed
#' @param usetemplateifmissing whether to use template brain when the subject
#' brain files are missing. If set to true, then a template (usually 'N27')
#' brain will be displayed as an alternative solution, and electrodes will be
#' rendered according to their \code{'MNI305'} coordinates, or
#' \code{'VertexNumber'} if given.
#' @param include_electrodes whether to include electrode in the model; default
#' is true
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
rave_brain <- function(subject, surfaces = 'pial', use_141 = TRUE,
                       recache = FALSE, clean_before_cache = FALSE,
                       compute_template = FALSE, usetemplateifmissing = FALSE,
                       include_electrodes = TRUE){

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
    brain <- threeBrain::merge_brain()

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
      path = fs_path, subject_code = subject$subject_code,
      surface_types = surfaces
    )


    if(is.data.frame(electrode_table)) {
      brain$set_electrodes(electrodes = electrode_table)
    }

    if( compute_template ){
      tf <- tempfile()
      new_table <- brain$calculate_template_coordinates(save_to = tf)
      if( file.exists(tf) ){
        brain$electrodes$raw_table_path <- NULL
        unlink(tf)
        # need to update meta
        save_meta2(
          data = new_table,
          meta_type = 'electrodes',
          project_name = subject$project_name,
          subject_code = subject$subject_code
        )
      }
    }

  }

  brain$meta$constructor_params <- list(
    project_name = subject$project_name,
    subject_code = subject$subject_code,
    use_141 = use_141,
    usetemplateifmissing = usetemplateifmissing
  )

  brain
}


transform_point_to_template_surface <- function(subject, scan_ras_mat, hemisphere, use_surface = "pial", template = NA, flip_hemisphere = FALSE, ...) {
  # subject <- "YAEL/Precision003"
  # scan_ras_mat <- c(1,1,1)
  # hemisphere <- "r"
  # template <- NA

  if(length(template) != 1 || is.na(template) || !nzchar(template)) {
    template <- "cvs_avg35_inMNI152"
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
  # subject <- "YAEL/Precision003"
  # scan_ras_mat <- c(1,1,1)
  # method <- "auto"

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
      catgl("Using non-linear mapping to transform points to MNI152 space",
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
                                                    verbose = verbose)
    tmp <- scan_ras_mat
    tmp[valid_positions, ] <- mni152_ras[seq_len(sum(valid_positions)), ]
    tmp[!valid_positions, ] <- NA_real_
    mni152_ras <- tmp
  } else {
    if(verbose) {
      catgl("Using affine matrix for mapping points to MNI152 space",
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
#' @param ... passed to other inner functions. For volume mapping, additional
#' arguments include \code{method} with choices \code{'auto'} (default),
#' \code{'affine'}, and \code{'nonlinear'}. For surface mapping, additional
#' arguments include \code{use_surface} (which surface to project to)
#' with default \code{'pial'}; \code{template} (which template to use for
#' calculating 'MNI' coordinates) with a default template 'FreeSurfer'
#' 'MNI' brain \code{'cvs_avg35_inMNI152'}
#' @examples
#'
#' if(interactive()) {
#'
#' transform_point_to_template('demo/DemoSubject', mapping_method = "volumetric")
#'
#' }
#'
#' @export
transform_point_to_template <- function(subject, positions, space = c("scannerRAS", "tkrRAS"), mapping_method = c("volumetric", "surface"), flip_hemisphere = FALSE, ...) {

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
    res <- transform_point_to_template_volumetric(subject = subject, scan_ras_mat = positions, flip_hemisphere = flip_hemisphere, ...)
  } else {
    res <- transform_point_to_template_surface(subject = subject, scan_ras_mat = positions, flip_hemisphere = flip_hemisphere, ...)
  }

  return(res)

}
