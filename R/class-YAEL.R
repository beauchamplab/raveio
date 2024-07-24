# DIPSAUS DEBUG START
# subject_code <- "testtest2"
# ct_path = '/Users/dipterix/rave_data/raw_dir/testtest/rave-imaging_old/sub-AnonSEEG_ses-postop_desc-preproc_CT.nii'
# mr_path = '/Users/dipterix/rave_data/raw_dir/testtest/rave-imaging_old/sub-AnonSEEG_ses-preop_desc-preproc_acq-ax_T1w.nii'
# pipelines = c("coregistration", "normalization")
# templates = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c")
# template_path = "/Users/dipterix/rave_data/others/three_brain/templates/mni_icbm152_nlin_asym_09b/T1.nii.gz"
# self <- YAELProcess$new(subject_code)
# private <- self$.__enclos_env__$private
# verbose=T
# native_type = 'T1w'
# template_name = c("mni_icbm152_nlin_asym_09a"); template_name2 <- camel_template_name(template_name)
# native_ras = rnorm(90)
NULL

# self$set_input_image(mr_path, "T1w")
# self$set_input_image(ct_path, "CT")

# dipsaus::rs_exec({
#   self <- raveio:::YAELProcess$new(subject_code)
#   print(self)
#   self$register_to_T1w("CT", reverse = TRUE, verbose = TRUE)
# })
# dipsaus::rs_exec({
#   devtools::load_all("/Users/dipterix/Dropbox (Personal)/projects/raveio")
#   subject_code <- "testtest3"
#   self <- raveio:::YAELProcess$new(subject_code)
#   print(self)
#   self$map_to_template(template_name = "mni_icbm152_nlin_asym_09a")
# })

call_rpyants <- function(.name, ...) {
  rpyANTs <- asNamespace("rpyANTs")
  re <- rpyANTs[[.name]]
  if(is.function(re)) {
    re <- re(...)
  }
  return(re)
}

camel_template_name <- function(
    template_name = c(
      "mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"
    )) {
  template_name <- match.arg(template_name)
  template_info <- call_rpyants("template_urls")[[template_name]]
  template_name2 <- template_info$name
  return( template_name2 )
}


#' @title Class definition of \code{'YAEL'} image pipeline
#' @description
#' Rigid-registration across multiple types of images, non-linear normalization
#' from native brain to common templates, and map template atlas or 'ROI' back
#' to native brain. See examples at \code{\link{as_yael_process}}
#' @export
YAELProcess <- R6::R6Class(
  classname = "YAELProcess",
  private = list(
    .subject_code = character(),
    .impl = function() {
      rpyants <- call_rpyants("load_rpyants")
      rpyants$registration$YAELPreprocess(private$.subject_code, self$work_path)
    }
  ),
  public = list(
    #' @description
    #' Constructor to instantiate the class
    #' @param subject_code character code representing the subject
    initialize = function(subject_code) {
      stopifnot2(length(subject_code) == 1 && is.character(subject_code) &&
                   !is.na(subject_code) && nzchar(subject_code),
                 msg = "Please enter a valid subject code (with combinations of letter and digits)")
      private$.subject_code <- subject_code
    },

    #' @description
    #' Set the raw input for different image types
    #' @param path path to the image files in \code{'NIfTI'} format
    #' @param type type of the image
    #' @param overwrite whether to overwrite existing images if the same type
    #' has been imported before; default is false
    #' @param on_error when the file exists and \code{overwrite} is false,
    #' how should this error be reported; choices are \code{'warning'} (default),
    #' \code{'error'} (throw error and abort), or \code{'ignore'}.
    #' @returns whether the image has been set (or replaced)
    set_input_image = function(
      path, type = c("T1w", "T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR"),
      overwrite = FALSE, on_error = c("warning", "error", "ignore") ) {
      path <- normalize_path(path, must_work = TRUE)
      type <- match.arg(type)
      on_error <- match.arg(on_error)
      yael_py <- private$.impl()
      dir_create2(self$work_path)
      tryCatch({
        yael_py$set_image(path = path, type = type, overwrite = isTRUE(overwrite))
        if( type == "T1w" ) {
          path <- self$get_input_image("T1w")
          if(length(path)) {
            mri_dir <- file.path(self$work_path, "inputs", "MRI")
            dir_create2(mri_dir)
            file.copy(path, to = file.path(mri_dir, "MRI_RAW.nii.gz"), overwrite = TRUE)
          }
        }
        TRUE
      }, error = function(e) {
        switch(
          on_error,
          "warning" = { warning(e$message) },
          "error" = { stop(e$message) },
          {}
        )
        FALSE
      })
    },

    #' @description Get image path
    #' @param type type of the image
    #' @returns Absolute path if the image
    get_input_image = function(type = c("T1w", "T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR")) {
      type <- match.arg(type)
      yael_py <- private$.impl()
      call_rpyants("to_r", yael_py$input_image_path(type))
    },

    #' @description Get 'RAVE' subject instance
    #' @param project_name project name; default is \code{'YAEL'}
    #' @param strict passed to \code{\link{as_rave_subject}}
    #' @returns 'RAVE' subject instance
    get_subject = function(project_name = "YAEL", strict = FALSE) {
      RAVESubject$new(project_name = project_name, subject_code = private$.subject_code, strict = strict)
    },

    #' @description Register other images to 'T1' weighted 'MRI'
    #' @param image_type type of the image to register, must be set via
    #' \code{process$set_input_image} first.
    #' @param reverse whether to reverse the registration; default is false,
    #' meaning the fixed (reference) image is the \code{'T1'}. When setting to
    #' true, then the \code{'T1'} 'MRI' will become the moving image
    #' @param verbose whether to print out the process; default is true
    #' @returns Nothing
    register_to_T1w = function(image_type = c("CT", "T2w", "FLAIR", "preopCT", "T1wContrast", "fGATIR"), reverse = FALSE, verbose = TRUE) {
      image_type <- match.arg(image_type)
      reverse <- isTRUE(reverse)
      verbose <- isTRUE(verbose)
      yael_py <- private$.impl()
      yael_py$register_to_T1w(type = image_type, reverse = reverse, verbose = verbose)
      if( image_type == "CT" ) {
        # write down extras for YAEL localization module
        mapping <- self$get_native_mapping(
          image_type = "CT", relative = FALSE)
        file.copy(
          from = mapping$CT_path,
          to = file.path(mapping$work_path, "coregistration", "CT_RAW.nii.gz"),
          overwrite = TRUE
        )
        file.copy(
          from = mapping$T1w_path,
          to = file.path(mapping$work_path, "coregistration", "MRI_reference.nii.gz"),
          overwrite = TRUE
        )
        file.copy(
          from = mapping$mappings$vox2ras,
          to = file.path(mapping$work_path, "coregistration", "CT_IJK_to_MR_RAS.txt"),
          overwrite = TRUE
        )
      }
      invisible()
    },

    #' @description Get the mapping configurations used by \code{register_to_T1w}
    #' @param image_type type of the image registered to 'T1' weighted 'MRI'
    #' @param relative whether to use relative path (to the \code{work_path} field)
    #' @returns A list of moving and fixing images, with rigid transformations
    #' from different formats.
    get_native_mapping = function(image_type = c("CT", "T2w", "FLAIR", "preopCT", "T1wContrast", "fGATIR"), relative = FALSE) {
      image_type <- match.arg(image_type)
      yael_py <- private$.impl()
      return( call_rpyants("to_r", yael_py$get_native_mapping(image_type, relative = isTRUE(relative))) )
    },

    #' @description Normalize native brain to \code{'MNI152'} template
    #' @param template_name which template to use, choices are \code{'mni_icbm152_nlin_asym_09a'},
    #' \code{'mni_icbm152_nlin_asym_09b'}, \code{'mni_icbm152_nlin_asym_09c'}.
    #' @param native_type which type of image should be used to map to template;
    #' default is \code{'T1w'}
    #' @param verbose whether to print out the process; default is true
    #' @returns See method \code{get_template_mapping}
    map_to_template = function(
                 template_name = c(
                   "mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"
                 ),
                 native_type = c("T1w", "T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR"),
                 verbose = TRUE){
      native_type <- match.arg(native_type)
      template_name <- match.arg(template_name)
      # camel version for BIDS
      template_info <- call_rpyants("template_urls")[[template_name]]
      template_name2 <- template_info$name
      template_folder <- call_rpyants("ensure_template", template_name)
      template_path <- normalize_path(file.path(template_folder, "T1.nii.gz"))
      template_mask <- normalize_path(file.path(template_folder, "T1_brainmask.nii.gz"))
      if(!file.exists(template_mask)) { template_mask <- NULL }
      verbose <- isTRUE(verbose)
      yael_py <- private$.impl()
      yael_py$map_to_template(
        template_path = template_path,
        template_name = template_name2,
        native_type = native_type,
        template_mask_path = template_mask,
        native_mask_path = NULL,
        verbose = verbose
      )
      return (invisible(
        call_rpyants("to_r", yael_py$get_template_mapping(
          template_name = template_name2,
          native_type = native_type
        ))
      ))
    },

    #' @description Get configurations used for normalization
    #' @param template_name which template is used
    #' @param native_type which native image is mapped to template
    #' @param relative whether the paths should be relative or absolute; default
    #' is false (absolute paths)
    #' @returns A list of input, output images, with forward and inverse
    #' transform files (usually two \code{'Affine'} with one displacement field)
    get_template_mapping = function(
      template_name = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"),
      native_type = c("T1w", "T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR"), relative = FALSE
    ) {
      template_name <- match.arg(template_name)
      template_name2 <- camel_template_name(template_name)
      native_type <- match.arg(native_type)
      yael_py <- private$.impl()
      return(call_rpyants("to_r", yael_py$get_template_mapping(
        template_name = template_name2,
        native_type = native_type,
        relative = isTRUE(relative)
      )))
    },

    #' @description Apply transform from images (usually an atlas or 'ROI')
    #' on template to native space
    #' @param template_roi_path path to the template image file which will be
    #' transformed into individuals' image
    #' @param template_name templates to use
    #' @param native_type which type of native image to use for calculating
    #' the coordinates (default \code{'T1w'})
    #' @param interpolator how to interpolate the \code{'voxels'}; default is
    #' \code{"auto"}: \code{'linear'} for probabilistic map and \code{'nearestNeighbor'}
    #' otherwise.
    #' @param verbose whether the print out the progress
    #' @returns transformed image in 'ANTs' format
    transform_image_from_template = function(
      template_roi_path,
      template_name = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"),
      native_type = c("T1w", "T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR"),
      interpolator = c("auto", "nearestNeighbor", "linear", "gaussian", "bSpline", "cosineWindowedSinc", "welchWindowedSinc", "hammingWindowedSinc", "lanczosWindowedSinc", "genericLabel"), verbose = TRUE
    ) {
      stopifnot(file.exists(template_roi_path))
      template_roi_path <- normalize_path(template_roi_path)
      native_type <- match.arg(native_type)
      interpolator <- match.arg(interpolator)
      template_name <- match.arg(template_name)
      template_name2 <- camel_template_name(template_name)
      yael_py <- private$.impl()
      if( interpolator == "auto" ) {
        yael_py$transform_image_from_template(
          path = template_roi_path,
          template_name = template_name2,
          native_type = native_type,
          verbose = isTRUE(verbose)
        )
      } else {
        yael_py$transform_image_from_template(
          path = template_roi_path,
          template_name = template_name2,
          native_type = native_type,
          interpolator = interpolator,
          verbose = isTRUE(verbose)
        )
      }

    },

    #' @description Apply transform to images (usually an atlas or 'ROI')
    #' from native space to template
    #' @param native_roi_path path to the native image file that will be
    #' transformed into template
    #' @param template_name templates to use
    #' @param native_type which type of native image to use for calculating
    #' the coordinates (default \code{'T1w'})
    #' @param interpolator how to interpolate the \code{'voxels'}; default is
    #' \code{"auto"}: \code{'linear'} for probabilistic map and \code{'nearestNeighbor'}
    #' otherwise.
    #' @param verbose whether the print out the progress
    #' @returns transformed image in 'ANTs' format
    transform_image_to_template = function(
      native_roi_path,
      template_name = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"),
      native_type = c("T1w", "T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR"),
      interpolator = c("auto", "nearestNeighbor", "linear", "gaussian", "bSpline", "cosineWindowedSinc", "welchWindowedSinc", "hammingWindowedSinc", "lanczosWindowedSinc", "genericLabel"), verbose = TRUE
    ) {
      stopifnot(file.exists(native_roi_path))
      native_roi_path <- normalize_path(native_roi_path)
      native_type <- match.arg(native_type)
      interpolator <- match.arg(interpolator)
      template_name <- match.arg(template_name)
      template_folder <- call_rpyants("ensure_template", template_name)
      template_path <- normalize_path(file.path(template_folder, "T1.nii.gz"))
      template_name2 <- camel_template_name(template_name)
      yael_py <- private$.impl()

      # native_roi_path <- "/Volumes/BeauchampServe/rave_data/raw/PAV042/rave-imaging/fs/mri/wmparc.mgz"
      # template_name <- "mni_icbm152_nlin_asym_09a"
      # native_type <- "T1w"
      # interpolator <- "auto"
      # self <- raveio::as_yael_process(subject = "PAV042"); yael_py <- self$.__enclos_env__$private$.impl()
      # template_name2 <- raveio:::camel_template_name(template_name)
      # verbose <- T
      # template_path <- call_rpyants("ensure_template", name = template_name)

      if( interpolator == "auto" ) {
        yael_py$transform_image_to_template(
          path = native_roi_path,
          template_name = template_name2,
          template_path = template_path,
          native_type = native_type,
          verbose = isTRUE(verbose)
        )
      } else {
        yael_py$transform_image_to_template(
          path = native_roi_path,
          template_name = template_name2,
          template_path = template_path,
          native_type = native_type,
          interpolator = interpolator,
          verbose = isTRUE(verbose)
        )
      }
    },

    #' @description Generate atlas maps from template and morph to native brain
    #' @param template_name which template to use
    #' @param atlas_folder path to the atlas folder (that contains the atlas
    #' files)
    #' @param surfaces whether to generate surfaces (triangle mesh); default is
    #' \code{NA} (generate if not existed). Other choices are \code{TRUE}
    #' for always generating and overwriting surface files, or \code{FALSE}
    #' to disable this function. The generated surfaces will stay in native
    #' \code{'T1'} space.
    #' @param verbose whether the print out the progress
    #' @returns Nothing
    generate_atlas_from_template = function(
      template_name = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"),
      atlas_folder = NULL, surfaces = NA,
      verbose = TRUE
    ) {
      template_name <- match.arg(template_name)
      template_name2 <- camel_template_name(template_name)

      if( length(atlas_folder) != 1 || is.na(atlas_folder) || !nzchar(atlas_folder) ) {
        atlas_folder <- file.path(call_rpyants("ensure_template", name = template_name), "atlases")
      }
      if(!dir.exists(atlas_folder)) {
        stop("No atlases for template [", template_name, "]. Please place your atlas files under \n\t", atlas_folder)
      }

      yael_py <- private$.impl()
      yael_py$generate_atlas_from_template(
        template_name = template_name2,
        template_atlas_folder = atlas_folder,
        verbose = isTRUE(verbose)
      )
      volume_files <- list.files(
        file.path(self$work_path, "atlases"),
        pattern = "\\.(nii|nii\\.gz)$",
        recursive = TRUE,
        include.dirs = FALSE,
        all.files = FALSE,
        full.names = TRUE
      )
      if(isFALSE(surfaces)) { return(invisible()) }
      lapply_async(volume_files, function(path) {
        dname <- dirname(path)
        fname <- gsub("\\.(nii|nii\\.gz)$", '', basename(path), ignore.case = TRUE)
        fname <- sprintf("%s.gii", fname)
        dst_path <- file.path(dname, fname)
        if(isTRUE(surfaces) || !file.exists(dst_path)) {
          mesh <- threeBrain::volume_to_surf(path, save_to = dst_path)
        }
        NULL
      }, callback = function(path) {
        fname <- gsub("\\.(nii|nii\\.gz)$", '', basename(path), ignore.case = TRUE)
        sprintf("Generating surfaces|%s", fname)
      })
      invisible()
    },

    #' @description Transform points from native images to template
    #' @param native_ras matrix or data frame with 3 columns indicating points
    #' sitting on native images in right-anterior-superior (\code{'RAS'})
    #' coordinate system.
    #' @param template_name template to use for mapping
    #' @param native_type native image type where the points sit on
    #' @param verbose whether the print out the progress
    #' @returns A matrix of 3 columns, each row is a transformed points (
    #' invalid rows will be filled with \code{NA})
    transform_points_to_template = function(
      native_ras, template_name = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"),
      native_type = c("T1w", "T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR"), verbose = TRUE
    ) {
      template_name <- match.arg(template_name)
      template_name2 <- camel_template_name(template_name)
      native_type <- match.arg(native_type)

      if(is.vector(native_ras)) {
        native_ras <- matrix(native_ras, ncol = 3, byrow = TRUE)
      } else if(!is.matrix(native_ras)) {
        native_ras <- as.matrix(native_ras)
      }
      dimnames(native_ras) <- NULL
      storage.mode(native_ras) <- "double"
      invalid_rows <- is.na(rowMeans(native_ras, na.rm = FALSE))
      any_invalid <- any(invalid_rows)

      if(any_invalid) {
        native_ras[invalid_rows,] <- 0.0
      }

      yael_py <- private$.impl()
      res <- yael_py$transform_points_to_template(
        points = native_ras, template_name = template_name2,
        native_type = native_type, verbose = isTRUE(verbose)
      )
      res <- call_rpyants("to_r", res)
      if(any_invalid) {
        res[invalid_rows, ] <- NA_real_
      }
      res
    },

    #' @description Transform points from template images to native
    #' @param template_ras matrix or data frame with 3 columns indicating points
    #' sitting on template images in right-anterior-superior (\code{'RAS'})
    #' coordinate system.
    #' @param template_name template to use for mapping
    #' @param native_type native image type where the points sit on
    #' @param verbose whether the print out the progress
    #' @returns A matrix of 3 columns, each row is a transformed points (
    #' invalid rows will be filled with \code{NA})
    transform_points_from_template = function(
      template_ras, template_name = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"),
      native_type = c("T1w", "T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR"), verbose = TRUE
    ) {
      template_name <- match.arg(template_name)
      template_name2 <- camel_template_name(template_name)
      native_type <- match.arg(native_type)

      if(is.vector(template_ras)) {
        template_ras <- matrix(template_ras, ncol = 3, byrow = TRUE)
      } else if(!is.matrix(template_ras)) {
        template_ras <- as.matrix(template_ras)
      }
      dimnames(template_ras) <- NULL
      storage.mode(template_ras) <- "double"
      invalid_rows <- is.na(rowMeans(template_ras, na.rm = FALSE))
      any_invalid <- any(invalid_rows)

      if(any_invalid) {
        template_ras[invalid_rows,] <- 0.0
      }

      yael_py <- private$.impl()
      res <- yael_py$transform_points_from_template(
        points = template_ras, template_name = template_name2,
        native_type = native_type, verbose = isTRUE(verbose)
      )
      res <- call_rpyants("to_r", res)
      if(any_invalid) {
        res[invalid_rows, ] <- NA_real_
      }
      res
    },

    #' @description
    #' Create a reconstruction folder (as an alternative option) that
    #' is generated from template brain to facilitate the '3D' viewer.
    #' Please make sure method \code{map_to_template} is called before using
    #' this method (or the program will fail)
    #' @param template_name template to use for mapping
    #' @param add_surfaces whether to create surfaces that is morphed from
    #' template to local; default is \code{TRUE}. Please enable this option
    #' only if the cortical surfaces are not critical (for example,
    #' you are studying the deep brain structures). Always use
    #' \code{'FreeSurfer'} if cortical information is used.
    construct_ants_folder_from_template = function(
      template_name = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c"),
      add_surfaces = TRUE
    ) {
      ants_dir <- file.path(self$work_path, "ants")
      mri_dir <- file.path(ants_dir, "mri")
      surf_dir <- file.path(ants_dir, "surf")

      mr_path <- self$get_input_image("T1w")
      if(length(mr_path) != 1 || is.na(mr_path) || !file.exists(mr_path)) {
        stop("T1w image is not set. Please set the input image via\n\tself$set_input_image('/path/to/T1-weighted/MRI.nii.gz', type = 'T1w')")
      }

      dir_create2(mri_dir)

      # T1.mgz (freesurfer style so users don't need to have to re-localize)
      threeBrain::conform_volume(mr_path, save_to = file.path(mri_dir, "T1.mgz"))

      if(!length(template_name)) {
        return(invisible())
      }
      template_name <- match.arg(template_name)
      template_path <- call_rpyants("ensure_template", name = template_name)

      # brainmask
      brainmask_dst <- file.path(mri_dir, "brainmask.nii.gz")
      template_mask <- file.path(template_path, "T1_brainmask.nii.gz")
      if(file.exists(template_mask)) {
        mask <- self$transform_image_from_template(
          template_roi_path = template_mask,
          template_name = template_name,
          native_type = "T1w",
          interpolator = "nearestNeighbor",
          verbose = FALSE
        )
        mask$to_file(normalize_path(brainmask_dst, must_work = FALSE))
      } else {
        mask <- NULL
      }

      # intensity normalization & skull strip
      nu <- rpyANTs::correct_intensity(image = mr_path, mask = mask)
      nu$to_file(normalize_path(file.path(mri_dir, "nu.nii.gz"), must_work = FALSE))

      # skull-strip
      if(length(mask)) {
        skullstrip <- nu * mask
        # normalize to 0-255
        arr <- skullstrip[]
        arr[arr < 0] <- 0
        skullstrip <- skullstrip$astype("uint8")
        skullstrip[] <- floor(arr / max(arr) * 255)
        skullstrip$to_file(normalize_path(file.path(mri_dir, "brain.finalsurfs.nii.gz"),
                                          must_work = FALSE))
      } else {
        nu$to_file(normalize_path(file.path(mri_dir, "brain.finalsurfs.nii.gz"), must_work = FALSE))
      }

      # write down transforms
      suppressWarnings({

        tryCatch({
          mapping <- self$get_template_mapping(template_name = template_name,
                                               native_type = "T1w",
                                               relative = FALSE)
          n2t <- mapping$native_to_template$transformlist
          affine_path <- n2t[[length(n2t)]]
          if(length(affine_path) && file.exists(affine_path)) {
            # extract the affine transform (native to MNI152)
            transform <- rpyANTs::as_ANTsTransform(affine_path)[]
            scan_ras_to_mni305 <- solve(MNI305_to_MNI152) %*% transform
            transform_dir <- dir_create2(file.path(mri_dir, "transforms"))

            writeLines(
              con = file.path(transform_dir, "talairach.xfm"),
              sep = "\n",
              c(
                "MNI Transform File",
                "% avi2talxfm", "",
                "Transform_Type = Linear;",
                "Linear_Transform = ",
                paste(sprintf("%.6f", scan_ras_to_mni305[1, ]), collapse = " "),
                paste(sprintf("%.6f", scan_ras_to_mni305[2, ]), collapse = " "),
                sprintf("%s;", paste(sprintf("%.6f", scan_ras_to_mni305[3, ]), collapse = " "))
              )
            )
          }
        }, error = function(e) {
        })

      })

      native_brain <- threeBrain::threeBrain(path = ants_dir, subject_code = self$subject_code)

      # surfaces
      lh_pial_dst <- file.path(surf_dir, "lh.pial.T1")
      rh_pial_dst <- file.path(surf_dir, "rh.pial.T1")
      if( add_surfaces ) {
        template_brain <- threeBrain::threeBrain(path = file.path(template_path, "fs"),
                                                 subject_code = template_name)
        if(!is.null(template_brain) && length(template_brain$surfaces$pial)) {

          # Left hemisphere
          surf_file <- file.path(template_brain$base_path, "surf", c("lh.pial.T1", "lh.pial"))
          surf_file <- surf_file[file.exists(surf_file)]
          if(length(surf_file)) {
            surf_file <- surf_file[[1]]
            surface <- threeBrain::read.fs.surface(surf_file)
            # apply transform template tkr -> scanner
            scan_ras <- ( template_brain$Norig %*% solve(template_brain$Torig) ) %*% t(cbind(surface$vertices, 1))
            # apply transforms template scanner to native scanner
            native_ras <- self$transform_points_from_template(
              template_ras = t(scan_ras[seq_len(3), , drop = FALSE]),
              template_name = template_name,
              native_type = "T1w",
              verbose = FALSE
            )
            # native scanner to native tkr
            native_ras <- ( native_brain$Torig %*% solve(native_brain$Norig) ) %*% t(cbind(native_ras, 1))
            dir_create2(surf_dir)
            threeBrain::write.fs.surface(filepath = lh_pial_dst, vertex_coords = t(native_ras[seq_len(3), , drop = FALSE]), faces = surface$faces)
          }

          # Right hemisphere
          surf_file <- file.path(template_brain$base_path, "surf", c("rh.pial.T1", "rh.pial"))
          surf_file <- surf_file[file.exists(surf_file)]
          if(length(surf_file)) {
            surf_file <- surf_file[[1]]
            surface <- threeBrain::read.fs.surface(surf_file)
            # apply transform template tkr -> scanner
            scan_ras <- ( template_brain$Norig %*% solve(template_brain$Torig) ) %*% t(cbind(surface$vertices, 1))
            # apply transforms template scanner to native scanner
            native_ras <- self$transform_points_from_template(
              template_ras = t(scan_ras[seq_len(3), , drop = FALSE]),
              template_name = template_name,
              native_type = "T1w",
              verbose = FALSE
            )
            # native scanner to native tkr
            native_ras <- ( native_brain$Torig %*% solve(native_brain$Norig) ) %*% t(cbind(native_ras, 1))
            dir_create2(surf_dir)
            threeBrain::write.fs.surface(filepath = rh_pial_dst, vertex_coords = t(native_ras[seq_len(3), , drop = FALSE]), faces = surface$faces)
          }

        }

        writeLines(con = file.path(ants_dir, "README.txt"), text = c(
          sprintf("Generated by %s at %s.", paste(Sys.info()[['user']], collapse = ""), strftime(Sys.time())),
          sprintf("This folder is created by morphing template `%s` with native T1w MRI via `ANTs` SYN algorithm.", template_name),
          "The surfaces are just coarsed estimation for visualization purposes, and **may not be accurate**.",
          "Please use `FreeSurfer` for better cortical surface reconstruction."
        ))
      } else {
        if(file.exists(lh_pial_dst)) { unlink(lh_pial_dst) }
        if(file.exists(rh_pial_dst)) { unlink(rh_pial_dst) }
      }
      return(invisible())
    },

    #' @description Get '3D' brain model
    #' @param electrodes whether to add electrodes to the viewers; can be
    #' logical, data frame, or a character (path to electrode table). When
    #' the value is \code{TRUE}, the electrode file under \code{project_name}
    #' will be loaded; when \code{electrodes} is a \code{\link{data.frame}},
    #' or path to a \code{'csv'} file, then please specify \code{coord_sys}
    #' on what is the coordinate system used for columns \code{"x"}, \code{"y"},
    #' and \code{"z"}.
    #' @param project_name project name under which the electrode table should
    #' be queried, if \code{electrodes=TRUE}
    #' @param coord_sys coordinate system if \code{electrodes} is a data frame
    #' with columns \code{"x"}, \code{"y"}, and \code{"z"}, available choices
    #' are \code{'scannerRAS'} (defined by 'T1' weighted native 'MRI' image),
    #' \code{'tkrRAS'} (\code{'FreeSurfer'} defined native 'TK-registered'),
    #' \code{'MNI152'} (template 'MNI' coordinate system averaged over 152
    #' subjects; this is the common "'MNI' coordinate space" we often refer to),
    #' and \code{'MNI305'} (template 'MNI' coordinate system averaged over 305
    #' subjects; this coordinate system used by templates such as
    #' \code{'fsaverage'})
    #' @param ... passed to \code{\link[threeBrain]{threeBrain}}
    get_brain = function(electrodes = TRUE, project_name = "YAEL", coord_sys = c("scannerRAS", "tkrRAS", "MNI152", "MNI305"), ...) {
      coord_sys <- match.arg(coord_sys)
      subject <- RAVESubject$new(project_name = project_name, subject_code = self$subject_code, strict = FALSE)
      brain <- threeBrain::threeBrain(path = subject$freesurfer_path, subject_code = self$subject_code, ...)
      if(isTRUE(electrodes)) {
        tryCatch({
          brain$set_electrodes(electrodes = subject$get_electrode_table())
        }, error = function(e){})
        brain <- rave_brain(subject, ...)
      } else if (is.data.frame(electrodes) || is.character(electrodes)) {
        brain$set_electrodes(electrodes = electrodes, coord_sys = coord_sys)
      }
      brain
    }

  ),
  active = list(
    #' @field subject_code 'RAVE' subject code
    subject_code = function() {
      private$.subject_code
    },

    #' @field work_path Working directory ('RAVE' imaging path)
    work_path = function() {
      normalize_path(file.path(raveio_getopt("raw_data_dir"),
                               private$.subject_code,
                               "rave-imaging"), must_work = FALSE)
    }
  )
)

#' Create a 'YAEL' imaging processing instance
#' @description
#' Image registration across different modals. Normalize brain 'T1'-weighted
#' 'MRI' to template brain and generate subject-level atlas files.
#' @param subject character (subject code, or project name with subject code),
#' or \code{\link{RAVESubject}} instance.
#' @returns A processing instance, see \code{\link{YAELProcess}}
#' @examples
#'
#' library(raveio)
#' process <- as_yael_process("testtest2")
#'
#' # This example requires extra demo data & settings.
#' \dontrun{
#'
#' # Import and set original T1w MRI and CT
#' process$set_input_image("/path/to/T1w_MRI.nii", type = "T1w")
#' process$set_input_image("/path/to/CT.nii.gz", type = "CT")
#'
#' # Co-register CT to MRI
#' process$register_to_T1w(image_type = "CT")
#'
#' # Morph T1w MRI to 0.5 mm^3 MNI152 template
#' process$map_to_template(
#'   template_name = "mni_icbm152_nlin_asym_09b",
#'   native_type = "T1w"
#' )
#'
#' }
#'
#'
#' @export
as_yael_process <- function(subject) {
  if(!inherits(subject, 'RAVESubject') && is.character(subject)) {
    if(!grepl("/", subject)) {
      subject <- sprintf("YAEL/%s", subject)
    }
  }
  subject <- restore_subject_instance(subject, strict = FALSE)
  YAELProcess$new(subject_code = subject$subject_code)
}


