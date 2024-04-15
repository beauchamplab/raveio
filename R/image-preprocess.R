#' Process brain images for \code{'YAEL'}
#' @description
#' Aligns \code{'T1w'} with other image types; normalizes \code{'T1w'}
#' 'MRI' to 'MNI152' templates via symmetric non-linear morphs. Create brain
#' custom atlases from templates.
#' @param subject_code 'RAVE' subject code
#' @param t1w_path (required) 'T1' weighted 'MRI' path
#' @param ct_path (optional in general but mandatory for electrode localization)
#' post-surgery 'CT' path
#' @param t1w_contrast_path (optional) 'T1' weighted 'MRI' with contrast
#' (usually used to show the blood vessels)
#' @param fgatir_path (optional) 'fGATIR' (fast gray-matter acquisition 'T1'
#' inversion recovery) image path
#' @param preopct_path (optional) pre-surgery 'CT' path
#' @param t2w_path (optional) 'T2' weighted 'MRI' path
#' @param flair_path (optional) 'FLAIR' (fluid-attenuated inversion recovery)
#' image path
#' @param register_policy whether images should be registered with \code{'T1w'}
#' image; default is \code{"auto"}: automatically run registration algorithm
#' if missing; alternative is \code{"all"}: force the registration algorithm
#' even if mapping files exist
#' @param register_reversed direction of the registration; \code{FALSE}
#' (default) registers other images (such as post-surgery 'CT' to 'T1');
#' set to \code{FALSE} if you would like the \code{'T1'} to be registered into
#' other images. Since 'YAEL' does not re-sample the images, there is no
#' essential difference on the final registration results
#' @param normalize_template names of the templates which the native 'T1' images
#' will be normalized into
#' @param normalize_policy normalization policy; similar to
#' \code{register_policy} but is applied to normalization. Default is
#' \code{"auto"}: automatically run normalization when the mapping is missing,
#' and skip if exists; alternative is \code{"all"}: force to run the
#' normalization.
#' @param normalize_back length of one (select from \code{normalize_template}),
#' which template is to be used to generate native brain mask and transform
#' matrices
#' @param atlases a named list: the names must be template names from
#' \code{normalize_template} and the values must be directories of atlases of
#' the corresponding templates (see 'Examples').
#' @param add_surfaces Whether to add surfaces for the subject; default is
#' \code{FALSE}. The surfaces are created by reversing the normalization from
#' template brain, hence the results will not be accurate. Enable this option
#' only if cortical surface estimation is not critical.
#' @param verbose whether to print out the progress; default is \code{TRUE}
#' @returns Nothing, a subject imaging folder will be created under 'RAVE'
#' raw folder
#' @examples
#'
#' \dontrun{
#'
#'
#' # For T1 preprocessing only
#' yael_preprocess(
#'   subject_code = "patient01",
#'   t1w_path = "/path/to/T1.nii or T1.nii.gz",
#'
#'   # normalize T1 to all 2009 MNI152-Asym brains (a,b,c)
#'   normalize_template = c(
#'     "mni_icbm152_nlin_asym_09a",
#'     "mni_icbm152_nlin_asym_09b",
#'     "mni_icbm152_nlin_asym_09c"
#'   ),
#'
#'   # only normalize if not exists
#'   normalize_policy = "auto",
#'
#'   # use MNI152b to create native processing folder
#'   normalize_back = "mni_icbm152_nlin_asym_09b",
#'
#'   # Atlases generated from different templates have different
#'   # coordinates, hence both folder path and template names must be
#'   # provided
#'   atlases = list(
#'     mni_icbm152_nlin_asym_09b = "/path/to/atlas/folder1",
#'     mni_icbm152_nlin_asym_09c = "/path/to/atlas/folder2"
#'   )
#'
#' )
#'
#' # For T1 and postop CT coregistration only
#' yael_preprocess(
#'   subject_code = "patient01",
#'   t1w_path = "/path/to/T1.nii or T1.nii.gz",
#'   ct_path = "/path/to/CT.nii or CT.nii.gz",
#'
#'   # No normalization
#'   normalize_template = NULL,
#'   normalize_back = NA
#'
#' )
#'
#' # For both T1 and postop CT coregistration and T1 normalization
#' yael_preprocess(
#'   subject_code = "patient01",
#'   t1w_path = "/path/to/T1.nii or T1.nii.gz",
#'   ct_path = "/path/to/CT.nii or CT.nii.gz",
#'
#'   normalize_template = c(
#'     "mni_icbm152_nlin_asym_09a",
#'     "mni_icbm152_nlin_asym_09b",
#'     "mni_icbm152_nlin_asym_09c"
#'   ),
#'
#'   normalize_policy = "auto",
#'
#'   normalize_back = "mni_icbm152_nlin_asym_09b",
#'
#'   atlases = list(
#'     mni_icbm152_nlin_asym_09b = "/path/to/atlas/folder1",
#'     mni_icbm152_nlin_asym_09c = "/path/to/atlas/folder2"
#'   )
#'
#' )
#'
#'
#' }
#'
#' @export
yael_preprocess <- function(
    subject_code, t1w_path, ct_path = NULL,
    t2w_path = NULL, fgatir_path = NULL, preopct_path = NULL,
    flair_path = NULL, t1w_contrast_path = NULL,
    register_policy = c("auto", "all"), register_reversed = FALSE,
    normalize_template = "mni_icbm152_nlin_asym_09a",
    normalize_policy = c("auto", "all"),
    normalize_back = ifelse(length(normalize_template) >= 1, normalize_template[[1]], NA),
    atlases = list(),
    add_surfaces = FALSE, verbose = TRUE
) {

  register_policy <- match.arg(register_policy)
  normalize_policy <- match.arg(normalize_policy)

  normalize_template <- normalize_template[normalize_template %in% c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c")]
  # generate atlases
  atlases <- as.list(atlases)
  for(template_name in normalize_template) {
    rpyANTs::ensure_template(template_name)
    if(!length(atlases[[template_name]])) {
      atlas_folder <- file.path(threeBrain::default_template_directory(), "templates", template_name, "atlases")
      if(dir.exists(atlas_folder)) {
        atlases[[template_name]] <- normalize_path(atlas_folder)
      }
    }
  }

  yael_process <- YAELProcess$new(subject_code = subject_code)
  stopifnot2(
    length(t1w_path) == 1 && file.exists(t1w_path),
    msg = "Please specify T1w image path"
  )

  # DIPSAUS DEBUG START
  # subject_code = "testtest2"
  # t1w_path = "/Users/dipterix/rave_data/raw_dir/AnonSEEG/preprocessing/anat/sub-AnonSEEG_ses-preop_desc-preproc_acq-ax_T1w.nii"
  # ct_path = "/Users/dipterix/rave_data/raw_dir/AnonSEEG/preprocessing/anat/sub-AnonSEEG_ses-postop_desc-preproc_CT.nii"
  # t2w_path = "/Users/dipterix/rave_data/raw_dir/AnonSEEG/preprocessing/anat/sub-AnonSEEG_ses-preop_desc-preproc_acq-iso_T2w.nii"
  # fgatir_path = "/Users/dipterix/rave_data/raw_dir/AnonSEEG/preprocessing/anat/sub-AnonSEEG_ses-preop_desc-preproc_acq-ax_FGATIR.nii"
  # preopct_path = NULL
  # verbose <- TRUE
  # register_policy = "auto"
  # normalize_policy = "auto"
  # register_reversed = FALSE
  # yael_process <- YAELProcess$new(subject_code = subject_code)
  # normalize_template = c("mni_icbm152_nlin_asym_09a")

  logger <- function(..., level = "DEFAULT") {
    if(verbose) {
      catgl(..., level = level)
    }
  }

  logger("Migrating T1w image: ", t1w_path)
  yael_process$set_input_image(path = t1w_path, type = "T1w", overwrite = TRUE)

  if(length(ct_path) && !is.na(ct_path) && nzchar(ct_path)) {
    logger("Migrating CT image: ", ct_path)
    yael_process$set_input_image(path = ct_path, type = "CT", overwrite = TRUE)
  }

  if(length(t2w_path) && !is.na(t2w_path) && nzchar(t2w_path)) {
    logger("Migrating T2w image: ", t2w_path)
    yael_process$set_input_image(path = t2w_path, type = "T2w", overwrite = TRUE)
  }

  if(length(fgatir_path) && !is.na(fgatir_path) && nzchar(fgatir_path)) {
    logger("Migrating fGATIR image: ", fgatir_path)
    yael_process$set_input_image(path = fgatir_path, type = "fGATIR", overwrite = TRUE)
  }

  if(length(preopct_path) && !is.na(preopct_path) && nzchar(preopct_path)) {
    logger("Migrating preop-CT image: ", preopct_path)
    yael_process$set_input_image(path = preopct_path, type = "preopCT", overwrite = TRUE)
  }

  if(length(flair_path) && !is.na(flair_path) && nzchar(flair_path)) {
    logger("Migrating FLAIR image: ", flair_path)
    yael_process$set_input_image(path = flair_path, type = "FLAIR", overwrite = TRUE)
  }

  if(length(t1w_contrast_path) && !is.na(t1w_contrast_path) && nzchar(t1w_contrast_path)) {
    logger("Migrating T1w with contrast image: ", t1w_contrast_path)
    yael_process$set_input_image(path = t1w_contrast_path, type = "T1wContrast", overwrite = TRUE)
  }

  # Coregistration
  lapply(c("T2w", "CT", "FLAIR", "preopCT", "T1wContrast", "fGATIR"), function(native_type) {
    impath <- yael_process$get_input_image(native_type)
    if(!length(impath)) { return() }
    logger("Co-registering [", native_type, "] image with [T1w] image.")
    suppressWarnings({
      pexists <- tryCatch({
        conf <- yael_process$get_native_mapping(image_type = native_type)
        length(conf$mappings) > 0
      }, error = function(e){ FALSE })
    })
    if(!pexists || register_policy == "all") {
      yael_process$register_to_T1w(image_type = native_type,
                                   reverse = register_reversed,
                                   verbose = verbose)
    }
    return()
  })

  # Normalization
  lapply(normalize_template, function(template_name) {
    logger("Normalizing [T1w] image to template [", template_name, "].")
    suppressWarnings({
      pexists <- tryCatch({
        conf <- yael_process$get_template_mapping(template_name = template_name, native_type = "T1w")
        length(conf) > 0
      }, error = function(e){ FALSE })
    })
    if(!pexists || normalize_policy == "all") {
      yael_process$map_to_template(template_name = template_name,
                                   native_type = "T1w",
                                   verbose = verbose)
    }
  })

  if( length(normalize_back) == 1 && !is.na(normalize_back) ) {
    # Generate ANTs folder
    yael_process$construct_ants_folder_from_template(
      template_name = normalize_back,
      add_surfaces = add_surfaces
    )
  }

  for(template_name in names(atlases)) {
    if(template_name != "") {
      tryCatch({
        yael_process$generate_atlas_from_template(
          template_name = template_name,
          atlas_folder = atlases[[template_name]],
          verbose = verbose,
          surfaces = TRUE
        )
      }, error = function(e) {
        warning(e)
      })
    }
  }

}
