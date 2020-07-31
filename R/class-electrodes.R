#' Abstract definition of electrode class in RAVE
RAVEAbstarctElectrode <- R6::R6Class(
  classname = 'RAVEAbstarctElectrode',
  portable = TRUE,
  cloneable = FALSE,
  private = list(
    reference_cachename = character(0)
  ),
  public = list(

    #' @field type type of electrode
    type = 'Electrode',

    #' @field subject subject instance (\code{\link{RAVESubject}})
    subject = NULL,

    #' @field number integer stands for electrode number or reference ID
    number = NULL,

    #' @field reference reference electrode, either \code{NULL} for no reference
    #' or an electrode instance inherits \code{RAVEAbstarctElectrode}
    reference = NULL,

    #' @field epoch a \code{\link{RAVEEpoch}} instance
    epoch = NULL,

    #' @field cached_reference character, refer to current cached reference
    cached_reference = NULL,

    #' @field is_reference whether this instance is a reference electrode
    is_reference = FALSE,

    #' @description set cache name, internally used
    #' @param name character, internally used
    .set_cachename = function(name){
      private$reference_cachename = name
    },

    #' @description constructor
    #' @param subject character or \code{\link{RAVESubject}} instance
    #' @param number current electrode number or reference ID
    #' @param is_reference whether instance is a reference
    initialize = function(subject, number, is_reference = FALSE){
      self$subject = ravecore::as_rave_subject(subject)
      self$number = number
      self$reference = NULL
      self$epoch = NULL
      private$reference_cachename = rand_string(6)

      # load cached references
      cache_table = file.path(self$subject$cache_path, 'cached_reference.csv')
      if(file.exists(cache_table)){
        cache_table = safe_read_csv(cache_table)
        self$cached_reference = cache_table$Reference[cache_table$Electrode == number]
      } else {
        rave_error('Cannot find cached_reference.csv')
      }

    },

    #' @description set reference for instance
    #' @param reference \code{NULL} or \code{RAVEAbstarctElectrode} instance
    #' @param type reference electrode type, default is the same as current
    #' instance
    .set_reference = function(reference, type){
      if(missing(type)){
        type = self$type
      }
      stopifnot2(
        is.null(reference) || (
          inherits(reference, 'RAVEAbstarctElectrode') &&
            reference$type == type
        ),
        msg = sprintf('set_reference must receive a %s electrode', sQuote(type))
      )

      self$reference = reference
    },

    #' @description set epoch instance for the electrode
    #' @param epoch characters or \code{\link{RAVEEpoch}} instance. For
    #' characters, make sure \code{"epoch_<name>.csv"} is in meta folder.
    set_epoch = function(epoch){
      if(!inherits(epoch, 'RAVEEpoch')){
        epoch = RAVEEpoch$new(subject = self$subject, name = epoch)
      }
      self$epoch = epoch
    },

    #' @description method to clear cache on hard drive
    #' @param ... passed to child instances
    clear_cache = function(...){},

    #' @description method to clear memory
    #' @param ... passed to child instances
    clear_memory = function(...){}

  ),
  active = list(

    #' @field exists whether electrode exists in subject
    exists = function(){
      self$number %in% self$subject$electrodes
    },

    #' @field preprocess_file path to preprocess 'HDF5' file
    preprocess_file = function(){
      file.path(self$subject$preprocess_path, sprintf('electrode_%s.h5', self$number))
    },

    #' @field power_file path to power 'HDF5' file
    power_file = function(){
      file.path(self$subject$data_path, 'power', sprintf('%s.h5', self$number))
    },

    #' @field power_cached path to power 'FST' file
    power_cached = function(){
      if(is.null(self$reference) || self$reference$number == 'noref'){
        # check if raw exists
        file.path(self$subject$cache_path, 'power', 'raw')
      }
      self$cached_reference
    },

    #' @field phase_file path to phase 'HDF5' file
    phase_file = function(){
      file.path(self$subject$data_path, 'phase', sprintf('%s.h5', self$number))
    },

    #' @field voltage_file path to voltage 'HDF5' file
    voltage_file = function(){
      file.path(self$subject$data_path, 'voltage', sprintf('%s.h5', self$number))
    },

    #' @field reference_name reference electrode name (field: number)
    reference_name = function(){
      if(is.null(self$reference)){
        'noref'
      } else {
        ref = stringr::str_remove_all(self$reference$number, '(\\.h5$)|(^ref_)')
        sprintf('ref_%s', ref)
      }
    },

    #' @field cache_path run-time cache path; \code{NA} if epoch is missing
    cache_path = function(){
      if(!length(self$epoch)){
        return(NA)
      }
      cache_path = rave_options('cache_path')
      # save to cache_path/project/subject/epoch/cachename
      # cachename = reference + elec type
      file.path(cache_path, self$subject$project_name,
                self$subject$subject_code, self$epoch$name,
                private$reference_cachename)

    }
  )
)


