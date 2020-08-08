#' Get \code{\link{RAVESubject}} instance from character
#' @param subject_id character in format \code{"project/subject"}
#' @param strict whether to check if subject directories exist or not
#' @return \code{\link{RAVESubject}} instance
#' @seealso \code{\link{RAVESubject}}
#' @export
as_rave_subject <- function(subject_id, strict = TRUE){
  if(inherits(subject_id, 'RAVESubject')){
    return(subject_id)
  } else {
    RAVESubject$new(subject_id, strict = strict)
  }
}

#' Definition for 'RAVE' subject class
#' @export
RAVESubject <- R6::R6Class(
  classname = 'RAVESubject',
  class = TRUE,
  portable = TRUE,
  private = list(
    .name = character(0),
    .path = character(0),
    .dirs = NULL,
    .project = NULL,
    .preprocess = NULL,
    .cached_config = NULL,
    .reference_tables = list()

  ),
  public = list(

    #' @description override print method
    #' @param ... ignored
    print = function(...){
      cat('RAVE subject <', self$subject_id, '>\n', sep = '')
      cat('')
    },

    #' @description constructor
    #' @param project_name character project name
    #' @param subject_code character subject code
    #' @param strict whether to check if subject folders exist
    initialize = function(project_name, subject_code = NULL, strict = TRUE){
      stopifnot2(is.character(project_name), msg = "RAVESubject: project name and subject code must be characters")
      if(length(subject_code) != 1){
        if(stringr::str_detect(project_name, '/|\\\\')){
          project_name <- stringr::str_trim(
            unlist(stringr::str_split(project_name, '/|\\\\'))
          )
          subject_code <- project_name[2]
          project_name <- project_name[1]
        } else {
          stop(catgl("Subject {project_name} invalid. The format must be project/subject.", .capture = TRUE))
        }
      }
      subject_code <- stringr::str_remove(subject_code, '^sub-')
      private$.project <- RAVEProject$new(project_name, strict = strict)
      private$.name <- subject_code
      private$.dirs <- rave_directories(subject_code, project_name)
      if(!dir.exists(private$.dirs$subject_path) && strict){
        stop(catgl("Subject {project_name}/{subject_code} doesn't exist.", .capture = TRUE))
      }
      private$.path <- private$.dirs$rave_path
      private$.preprocess <- RAVEPreprocessSettings$new(subject = self, read_only = TRUE)
      private$.cached_config <- dipsaus::fastmap2()
    },

    #' @description get subject meta data located in \code{"meta/"} folder
    #' @param meta_type choices are 'electrodes', 'frequencies', 'time_points',
    #' 'epoch', 'references'
    #' @param meta_name if \code{meta_type='epoch'}, read in
    #' \code{'epoch_<meta_name>.csv'}; if \code{meta_type='references'},
    #' read in \code{'reference_<meta_name>.csv'}.
    #' @seealso \code{\link{load_meta2}}
    #' @return data frame
    meta_data = function(
      meta_type = c('electrodes', 'frequencies', 'time_points',
                    'epoch', 'references'),
      meta_name = 'default'){
      meta_type <- match.arg(meta_type)
      load_meta2(meta_type = meta_type, meta_name = meta_name,
                project_name = self$project_name, subject_code = self$subject_code)
    },

    #' @description get valid electrode numbers
    #' @param reference_name character, reference name, see \code{meta_name}
    #' in \code{self$meta_data} or \code{\link{load_meta2}} when
    #' \code{meta_type} is 'reference'
    #' @param refresh whether to reload reference table before obtaining data,
    #' default is false
    #' @return integer vector of valid electrodes
    valid_electrodes = function(reference_name, refresh = FALSE){
      if(refresh){
        private$.reference_tables[[reference_name]] <- self$meta_data(
          meta_type = 'references', meta_name = reference_name)
      } else {
        private$.reference_tables[[reference_name]] %?<-% self$meta_data(
          meta_type = 'references', meta_name = reference_name)
      }
      ref_table <- private$.reference_tables[[reference_name]]
      as.integer(ref_table$Electrode[ref_table$Reference != ''])
    },

    #' @description create subject's directories on hard disk
    #' @param include_freesurfer whether to create 'FreeSurfer' path
    initialize_paths = function(include_freesurfer = TRUE){
      dir_create2(self$rave_path)
      dir_create2(self$preprocess_path)
      dir_create2(self$data_path)
      dir_create2(self$reference_path)
      dir_create2(self$cache_path)
      dir_create2(self$meta_path)

      # save preprocess
      self$preprocess_settings$save()

      if(include_freesurfer){
        if(is.na(self$freesurfer_path) || !dir.exists(self$freesurfer_path)){
          path <- file.path(self$path, 'fs')
          dir_create2(path)
        }
      }
    }

  ),
  active = list(

    #' @field project project instance of current subject; see
    #' \code{\link{RAVEProject}}
    project = function(){
      private$.project
    },

    #' @field project_name character string of project name
    project_name = function(){
      private$.project$name
    },

    #' @field subject_code character string of subject code
    subject_code = function(){
      private$.name
    },

    #' @field subject_id subject ID: \code{"project/subject"}
    subject_id = function(){
      sprintf('%s/%s', private$.project$name, private$.name)
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
      # 2. rave_data/project/subject/fs
      # 3. rave_data/project/subject/subject

      re <- file.path(getOption('rave.freesurfer_dir'), self$subject_code)
      if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re <- file.path(self$rave_path, 'fs')
      if(dir.exists(re) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re <- file.path(self$path, 'fs')
      if(dir.exists(re) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
      re <- file.path(self$path, self$subject_code)
      if(dir.exists(re) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
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
