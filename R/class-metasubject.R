#' Defines 'RAVE' subject class for meta analyses
#' @description \code{R6} class definition
#' @export
RAVEMetaSubject <- R6::R6Class(
  classname = 'RAVEMetaSubject',
  inherit = RAVESubject,
  class = TRUE,
  portable = FALSE,
  private = list(
    .fake_project = NULL
  ),
  public = list(

    #' @description override print method
    #' @param ... ignored
    print = function(...){
      cat('RAVE meta subject <', private$.project$name, "/", private$.name, '>\n', sep = '')
    },

    #' @description constructor
    #' @param project_name character project name
    #' @param subject_code character subject code
    #' @param strict whether to check if subject folders exist
    initialize = function(project_name, subject_code = NULL, strict = FALSE){
      super$initialize(project_name = project_name, subject_code = subject_code, strict = strict)
      private$.fake_project <- RAVEProject$new("@meta_analysis", strict = FALSE)
    },

    #' @description get subject meta data located in \code{"meta/"} folder
    #' @param meta_type choices are 'electrodes', 'frequencies', 'time_points',
    #' 'epoch', 'references'
    #' @param meta_name if \code{meta_type='epoch'}, read in
    #' \code{'epoch_<meta_name>.csv'}; if \code{meta_type='references'},
    #' read in \code{'reference_<meta_name>.csv'}.
    #' @seealso \code{\link{load_meta2}}
    #' @returns data frame
    meta_data = function(
      meta_type = c('electrodes', 'frequencies', 'time_points',
                    'epoch', 'references'),
      meta_name = 'default'){
      meta_type <- match.arg(meta_type)
      load_meta2(meta_type = meta_type, meta_name = meta_name,
                 project_name = super$project_name, subject_code = super$subject_code)
    }

    # TODO: change save_meta2 and load_meta2

  ),
  active = list(

    #' @field project project instance of current subject; see
    #' \code{\link{RAVEProject}}
    project = function(){
      private$.fake_project
    },

    #' @field project_name character string of project name
    project_name = function(){
      private$.fake_project$name
    },

    #' @field subject_code character string of subject code
    subject_code = function(){
      sprintf('%s/%s', private$.project$name, private$.name)
    },

    #' @field subject_id subject ID: \code{"project/subject"}
    subject_id = function(){
      sprintf('@meta_analysis/%s/%s', private$.project$name, private$.name)
    },

    #' @field path subject root path
    path = function(){
      private$.dirs$subject_path
    },

    #' @field rave_path 'rave' directory under subject root path
    rave_path = function(){
      private$.path
    },

    #' @field meta_path meta data directory for current subject
    meta_path = function(){
      private$.dirs$meta_path
    },

    #' @field freesurfer_path 'FreeSurfer' directory for current subject. If
    #' no path exists, values will be \code{NA}
    freesurfer_path = function(){
      # To find freesurfer directory, here are the paths to search
      # 0. if options('rave.freesurfer_dir') is provided, then XXX/subject/
      # 1. rave_data/project/subject/rave/fs
      # 2. rave_data/project/subject/imaging/fs
      # 3. rave_data/project/subject/fs
      # 4. rave_data/project/subject/subject
      # 5. raw_dir/subject/rave-imaging/fs
      # 6. raw_dir/subject/fs

      re <- as.character(file.path(getOption('rave.freesurfer_dir'), private$.name))
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      # update: check subject/imaging/fs provided by the new pipeline
      re <- as.character(file.path(self$preprocess_settings$raw_path, 'rave-imaging', "fs"))
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re <- as.character(file.path(self$preprocess_settings$raw_path, "fs"))
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      # Previous paths
      re <- as.character(file.path(self$rave_path, 'fs'))
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      # update: check subject/imaging/fs provided by the new pipeline
      re <- as.character(file.path(self$path, 'imaging', "fs"))
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re <- as.character(file.path(self$path, 'fs'))
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re <- as.character(file.path(self$path, private$.name))
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      return(NA)
    },

    #' @field preprocess_path preprocess directory under subject 'rave' path
    preprocess_path = function(){
      private$.dirs$proprocess_path
    },

    #' @field data_path data directory under subject 'rave' path
    data_path = function(){
      private$.dirs$data_path
    },

    #' @field cache_path path to 'FST' copies under subject 'data' path
    cache_path = function(){
      file.path(private$.dirs$data_path, 'cache')
    },

    #' @field pipeline_path path to pipeline scripts under subject's folder
    pipeline_path = function(){
      private$.dirs$pipeline_path
    },

    #' @field note_path path that stores 'RAVE' related subject notes
    note_path = function(){
      private$.dirs$note_path
    },

    #' @field epoch_names possible epoch names
    epoch_names = function(){
      fs <- list.files(self$meta_path, pattern = '^epoch_[a-zA-Z0-9_]+\\.csv$', ignore.case = TRUE)
      stringr::str_match(fs, '^epoch_([a-zA-Z0-9_]+)\\.[cC][sS][vV]$')[,2]
    },

    #' @field reference_names possible reference names
    reference_names = function(){
      fs <- list.files(self$meta_path, pattern = '^reference_[a-zA-Z0-9_]+\\.csv$', ignore.case = TRUE)
      stringr::str_match(fs, '^reference_([a-zA-Z0-9_]+)\\.[cC][sS][vV]$')[,2]
    },

    #' @field reference_path reference path under 'rave' folder
    reference_path = function(){
      private$.dirs$reference_path
    },

    #' @field preprocess_settings preprocess instance; see
    #' \code{\link{RAVEPreprocessSettings}}
    preprocess_settings = function(){
      private$.preprocess
    },

    #' @field blocks subject experiment blocks in current project
    blocks = function(){
      private$.preprocess$blocks
    },

    #' @field electrodes all electrodes, no matter excluded or not
    electrodes = function(){
      private$.preprocess$electrodes
    },

    #' @field raw_sample_rates voltage sample rate
    raw_sample_rates = function(){
      private$.preprocess$sample_rates
    },

    #' @field power_sample_rate power spectrum sample rate
    power_sample_rate = function(){
      private$.preprocess$wavelet_params$downsample_to
    },

    #' @field has_wavelet whether electrodes have wavelet transforms
    has_wavelet = function(){
      private$.preprocess$has_wavelet
    },

    #' @field notch_filtered whether electrodes are Notch-filtered
    notch_filtered = function(){
      private$.preprocess$notch_filtered
    },

    #' @field electrode_types electrode signal types
    electrode_types = function(){
      private$.preprocess$electrode_types
    }
  )
)
