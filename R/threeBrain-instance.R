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
    if(recache){
      if( clean_before_cache ){
        fs <- list.files(file.path(fs_path, 'RAVE'), pattern = '\\.json$',
                        all.files = FALSE, recursive = FALSE, full.names = TRUE,
                        ignore.case = TRUE, include.dirs = FALSE, no.. = TRUE)
        lapply(fs, unlink)
      }
      threeBrain::import_from_freesurfer(fs_path, subject_name = subject$subject_code)
    }

    brain <- tryCatch({
      threeBrain <- asNamespace('threeBrain')
      threeBrain$threeBrain(
        path = fs_path, subject_code = subject$subject_code,
        surface_types = surfaces
      )
    }, error = function(e) {
      threeBrain::freesurfer_brain2(
        fs_subject_folder = fs_path, subject_name = subject$subject_code,
        surface_types = surfaces, use_141 = use_141)
    })


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
