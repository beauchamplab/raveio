#' Abstract definition of electrode class in RAVE
#' @description This class is not intended for direct use. Please
#' create new child classes and implement some key methods.
#' @examples
#' \dontrun{
#'
#' # To run this example, please download demo subject (~700 MB) from
#' # https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta
#'
#' generator <- RAVEAbstarctElectrode
#'
#' # load demo subject electrode 14
#' e <- generator$new("demo/DemoSubject", number = 14)
#'
#' # set epoch
#' e$subject$epoch_names
#' e$set_epoch("auditory_onset")
#' head(e$epoch$table)
#'
#' # set epoch range (-1 to 2 seconds relative to onset)
#' e$trial_intervals <- c(-1,2)
#' # or to set multiple ranges
#' e$trial_intervals <- list(c(-2,-1), c(0, 2))
#'
#' # set reference
#' e$subject$reference_names
#' reference_table <- e$subject$meta_data(
#'   meta_type = "reference",
#'   meta_name = "default")
#' ref_name <- subset(reference_table, Electrode == 14)[["Reference"]]
#'
#' # the reference is CAR type, mean of electrode 13-16,24
#' ref_name
#'
#' # load & set reference
#' ref <- generator$new(e$subject, ref_name, is_reference = TRUE)
#' e$.set_reference(ref, e$type)
#'
#' }
#' @export
RAVEAbstarctElectrode <- R6::R6Class(
  classname = 'RAVEAbstarctElectrode',
  portable = FALSE,
  cloneable = TRUE,
  private = list(
    intervals = list()
  ),
  public = list(

    #' @field type type of electrode
    type = 'Electrode',  # LFP, Mini, EKG, Microwire...

    #' @field subject subject instance (\code{\link{RAVESubject}})
    subject = NULL,

    #' @field number integer stands for electrode number or reference ID
    number = integer(0),

    #' @field reference reference electrode, either \code{NULL} for no reference
    #' or an electrode instance inherits \code{RAVEAbstarctElectrode}
    reference = NULL,

    #' @field epoch a \code{\link{RAVEEpoch}} instance
    epoch = NULL,

    #' @field is_reference whether this instance is a reference electrode
    is_reference = FALSE,

    #' @description constructor
    #' @param subject character or \code{\link{RAVESubject}} instance
    #' @param number current electrode number or reference ID
    #' @param is_reference whether instance is a reference
    initialize = function(subject, number, is_reference = FALSE){
      self$subject <- raveio::as_rave_subject(subject)
      self$number <- number
      self$reference <- NULL
      self$epoch <- NULL
    },

    #' @description set reference for instance
    #' @param reference \code{NULL} or \code{RAVEAbstarctElectrode} instance
    #' @param type reference electrode type, default is the same as current
    #' instance
    .set_reference = function(reference, type){
      if(missing(type)){
        type <- self$type
      }
      stopifnot2(
        is.null(reference) || (
          inherits(reference, 'RAVEAbstarctElectrode') &&
            reference$type == type
        ),
        msg = sprintf('set_reference must receive a %s electrode', sQuote(type))
      )

      self$reference <- reference
      self$reference$epoch <- self$epoch

      self$reference$trial_intervals <- self$trial_intervals

      return(self$reference)
    },

    #' @description set epoch instance for the electrode
    #' @param epoch characters or \code{\link{RAVEEpoch}} instance. For
    #' characters, make sure \code{"epoch_<name>.csv"} is in meta folder.
    set_epoch = function(epoch){
      if(!inherits(epoch, 'RAVEEpoch')){
        epoch <- RAVEEpoch$new(subject = self$subject, name = epoch)
      }
      if(!is.null(self$reference)){
        self$reference$epoch <- epoch
      }
      self$epoch <- epoch
    },

    #' @description method to clear cache on hard drive
    #' @param ... implemented by child instances
    clear_cache = function(...){
      .NotYetImplemented()
    },

    #' @description method to clear memory
    #' @param ... implemented by child instances
    clear_memory = function(...){
      .NotYetImplemented()
    },

    #' @description method to load electrode data
    #' @param type data type such as \code{"power"}, \code{"phase"},
    #' \code{"voltage"}, \code{"wavelet-coefficient"}, or others
    #' depending on child class implementations
    load_data = function(type){
      .NotYetImplemented()
    }

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

    #' @field phase_file path to phase 'HDF5' file
    phase_file = function(){
      file.path(self$subject$data_path, 'phase', sprintf('%s.h5', self$number))
    },

    #' @field voltage_file path to voltage 'HDF5' file
    voltage_file = function(){
      file.path(self$subject$data_path, 'voltage', sprintf('%s.h5', self$number))
    },

    #' @field reference_name reference electrode name
    reference_name = function(){
      if(is.null(self$reference)){
        'noref'
      } else {
        ref <- stringr::str_remove_all(self$reference$number, '(\\.h5$)|(^ref_)')
        sprintf('ref_%s', ref)
      }
    },

    #' @field epoch_name current epoch name
    epoch_name = function(){
      if(!length(self$epoch)){
        stop("No epoch assigned. Please use `$set_epoch` method to set epoch.")
      }
      self$epoch$name
    },

    #' @field cache_root run-time cache path; \code{NA} if epoch or trial
    #' intervals are missing
    cache_root = function(){
      if(!length(self$epoch_name)){
        stop("No epoch assigned. Please use `$set_epoch` method to set epoch.")
      }
      if(!length(self$trial_intervals)){
        stop("No trial intervals added. Please set trial intervals, for example, by:\n",
             "  x$trial_intervals <- list(c(-1,2)) \n",
             "to load 1 second before onset and 2 seconds after onset.")
      }
      intv <- paste(
        sapply(self$trial_intervals, function(x){
          re <- sprintf("%s%.3f", c("M", "", "P")[sign(x) + 2], abs(x))
          re <- stringr::str_remove_all(re, "[.]{0,1}[0]+$")
          paste(re, collapse = "-")
        }),
        collapse = "_"
      )
      cache_path <- raveio::raveio_getopt(
        key = 'tensor_temp_path',
        default = '~/rave_data/cache_dir/')
      # save to cache_path/project/subject/epoch/cachename
      # cachename = reference + elec type

      nb <- stringr::str_remove_all(self$number, "(^ref_)|(\\.h5$)")

      file.path(cache_path, self$subject$project_name,
                self$subject$subject_code, self$epoch$name,
                intv, nb)
    },

    #' @field trial_intervals trial intervals relative to epoch onset
    trial_intervals = function(v){
      if(!missing(v)){
        if(!length(v)){
          private$intervals <- list()
        }
        if(!is.list(v)){
          v <- list(v)
        }
        stopifnot2(all(sapply(v, length) == 2), msg = "`set_intervals` requires intervals of length two")
        private$intervals <- v

        if(!is.null(self$reference)){
          self$reference$trial_intervals <- v
        }

      }
      private$intervals
    }
  )
)

