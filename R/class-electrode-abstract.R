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
#' ref <- generator$new(e$subject, ref_name)
#' e$set_reference(ref)
#'
#' }
#' @export
RAVEAbstarctElectrode <- R6::R6Class(
  classname = 'RAVEAbstarctElectrode',
  portable = FALSE,
  cloneable = TRUE,
  private = list(
    intervals = list(),
    .type = 'Unknown',
    .location = "Others",
    .power_enabled = FALSE,
    .is_reference = FALSE
  ),
  public = list(


    #' @field subject subject instance (\code{\link{RAVESubject}})
    subject = NULL,

    #' @field number integer stands for electrode number or reference ID
    number = integer(0),

    #' @field reference reference electrode, either \code{NULL} for no reference
    #' or an electrode instance inherits \code{RAVEAbstarctElectrode}
    reference = NULL,

    #' @field epoch a \code{\link{RAVEEpoch}} instance
    epoch = NULL,

    #' @description constructor
    #' @param subject character or \code{\link{RAVESubject}} instance
    #' @param number current electrode number or reference ID
    #' @param quiet reserved, whether to suppress warning messages
    initialize = function(subject, number, quiet = FALSE){
      self$subject <- restore_subject_instance(subject)
      self$number <- number
      self$reference <- NULL
      self$epoch <- NULL
    },

    #' @description set reference for instance
    #' @param reference \code{NULL} or \code{RAVEAbstarctElectrode} instance
    #' instance
    set_reference = function(reference){
      stopifnot2(
        is.null(reference) || (
          inherits(reference, 'RAVEAbstarctElectrode') &&
            reference$type == self$type
        ),
        msg = sprintf('set_reference must receive either NULL or a electrode of the same type (%s)', sQuote(self$type))
      )

      self$reference <- reference
      if(!is.null(reference)){

        self_epoch <- !is.null(self$epoch)
        ref_epoch <- !is.null(reference$epoch)

        if(self_epoch && ref_epoch && !identical(self$epoch$name, reference$epoch$name)){
          # compare epoch names
          stop("Electrode ", self$number, " has different epoch name to its reference: ",
                  self$epoch$name, " != ", reference$epoch$name, ".")
        } else if (self_epoch && !ref_epoch){
          self$reference$epoch <- self$epoch
        } else if (!self_epoch && ref_epoch){
          self$epoch <- self$reference$epoch
        }
        self$reference$trial_intervals <- self$trial_intervals
      }

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
    },

    #' @description load electrode block-wise data (with reference),
    #' useful when epoch is absent
    #' @param blocks session blocks
    #' @param type data type such as \code{"power"}, \code{"phase"},
    #' \code{"voltage"}, \code{"wavelet-coefficient"}.
    #' @param simplify whether to simplify the result
    #' @return If \code{simplify} is enabled, and only one block is loaded,
    #' then the result will be a vector (\code{type="voltage"}) or a matrix
    #' (others), otherwise the result will be a named list where the names
    #' are the blocks.
    load_blocks = function(blocks, type, simplify = TRUE) {
      .NotYetImplemented()
    }

  ),
  active = list(

    #' @field type signal type of the electrode, such as 'LFP', 'Spike', and
    #' 'EKG'; default is 'Unknown'
    type = function(){
      private$.type
    },

    #' @field power_enabled whether the electrode can be used in power analyses
    #' such as frequency, or frequency-time analyses;
    #' this usually requires transforming the electrode raw voltage signals
    #' using signal processing methods such as 'Fourier', 'wavelet', 'Hilbert',
    #' 'multi-taper', etc. If an electrode has power data, then it's power data
    #' can be loaded via \code{\link{prepare_subject_power}} method.
    power_enabled = function(){
      private$.power_enabled
    },

    #' @field is_reference whether this instance is a reference electrode
    is_reference = function(){
      private$.is_reference
    },


    #' @field location location type of the electrode, see
    #' \code{\link{LOCATION_TYPES}} for details
    location = function(v){
      if(!missing(v)){
        if(!v %in% LOCATION_TYPES){
          warning("Unsupported electrode location type: ", v, ". Use `Others` instead.")
          v <- "Others"
        }
        private$.location <- v
      }
      private$.location
    },

    #' @field exists whether electrode exists in subject
    exists = function(){
      self$number %in% self$subject$electrodes
    },

    #' @field preprocess_file path to preprocess 'HDF5' file
    preprocess_file = function(){
      file.path(self$subject$preprocess_path, "voltage", sprintf('electrode_%s.h5', self$number))
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

      cache_path <- get("cache_root", envir = asNamespace('raveio'), inherits = FALSE)()
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
        } else {
          private$intervals <- validate_time_window(v)
        }

        if(!is.null(self$reference)){
          self$reference$trial_intervals <- private$intervals
        }

      }
      private$intervals
    }
  )
)


#' @name new_electrode
#' @title Create new electrode channel instance or a reference signal instance
#' @param subject characters, or a \code{\link{RAVESubject}} instance
#' @param number integer in \code{new_electrode}, or characters in
#' \code{new_reference}; see 'Details' and 'Examples'
#' @param signal_type signal type of the electrode or reference; can be
#' automatically inferred, but it is highly recommended to specify a value;
#' see \code{\link{SIGNAL_TYPES}}
#' @param ... other parameters passed to class constructors, respectively
#' @return Electrode or reference instances that inherit
#' \code{\link{RAVEAbstarctElectrode}} class
#' @details In \code{new_electrode}, \code{number} should be a positive
#' valid integer indicating the electrode number. In \code{new_reference},
#' \code{number} can be one of the followings:
#' \describe{
#' \item{\code{'noref'}, or \code{NULL}}{no reference is needed}
#' \item{\code{'ref_X'}}{where \code{'X'} is a single number, then the
#' reference is another existing electrode; this could occur in
#' bipolar-reference cases}
#' \item{\code{'ref_XXX'}}{\code{'XXX'} is a combination of multiple
#' electrodes that can be parsed by \code{\link[dipsaus]{parse_svec}}. This
#' could occur in common average reference, or white matter reference. One
#' example is \code{'ref_13-16,24'}, meaning the reference signal is an
#' average of electrode 13, 14, 15, 16, and 24.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' # Download subject demo/DemoSubject (~500 MB)
#'
#' # Electrode 14 in demo/DemoSubject
#' subject <- as_rave_subject("demo/DemoSubject")
#' e <- new_electrode(subject = subject, number = 14, signal_type = "LFP")
#'
#' # Load CAR reference "ref_13-16,24"
#' ref <- new_reference(subject = subject, number = "ref_13-16,24",
#'                      signal_type = "LFP")
#' e$set_reference(ref)
#'
#'
#' # Set epoch
#' e$set_epoch(epoch = 'auditory_onset')
#'
#' # Set loading window
#' e$trial_intervals <- list(c(-1, 2))
#'
#' # Preview
#' print(e)
#'
#' # Now epoch power
#' power <- e$load_data("power")
#' names(dimnames(power))
#'
#' # Subset power
#' subset(power, Time ~ Time < 0, Electrode ~ Electrode == 14)
#'
#' # Draw baseline
#' tempfile <- tempfile()
#' bl <- power_baseline(power, baseline_windows = c(-1, 0),
#'                      method = "decibel", filebase = tempfile)
#' collapsed_power <- collapse2(bl, keep = c(2,1))
#' # Visualize
#' dname <- dimnames(bl)
#' image(collapsed_power, x = dname$Time, y = dname$Frequency,
#'       xlab = "Time (s)", ylab = "Frequency (Hz)",
#'       main = "Mean power over trial (Baseline: -1~0 seconds)",
#'       sub = glue('Electrode {e$number} (Reference: {ref$number})'))
#' abline(v = 0, lty = 2, col = 'blue')
#' text(x = 0, y = 20, "Audio onset", col = "blue", cex = 0.6)
#'
#' # clear cache on hard disk
#' e$clear_cache()
#' ref$clear_cache()
#'
#' }
#' @export
new_electrode <- function(subject, number, signal_type, ...){
  number <- as.integer(number)
  stopifnot(length(number) && !is.na(number))

  subject <- restore_subject_instance(subject, strict = FALSE)
  signal_type_expected <- subject$electrode_types[subject$electrodes == number]

  if(missing(signal_type)){
    signal_type <- signal_type_expected
  } else {
    signal_type <- match.arg(signal_type, choices = SIGNAL_TYPES)
    if(signal_type_expected != signal_type){
      catgl("Electrode {number} has signal type {signal_type_expected} but loaded as {signal_type}. This might cause some issues later", level = "WARNING")
    }
  }

  generator <- get(sprintf("%s_electrode", signal_type),
                   envir = asNamespace('raveio'),
                   inherits = FALSE)

  if(!inherits(generator, "R6ClassGenerator")){
    stop("Cannot find class definition for electrode with ", signal_type, " signal type.")
  }
  generator$new(subject = subject, number = number, ...)
}

#' @rdname new_electrode
#' @export
new_reference <- function(subject, number, signal_type, ...){
  if(!length(number) || number == "noref"){ return(NULL) }

  subject <- restore_subject_instance(subject, strict = FALSE)

  if(missing(signal_type)){
    elec <- dipsaus::parse_svec(gsub("[^0-9 ,-]", "", number))
    sel <- subject$electrodes %in% elec
    if(!any(sel)){
      stop("Cannot determine the signal type of ", number, ". Please specify `signal_type`")
    }
    signal_type <- subject$electrode_types[sel][[1]]
  } else {
    signal_type <- match.arg(signal_type, choices = SIGNAL_TYPES)
  }

  generator <- get(sprintf("%s_reference", signal_type),
                   envir = asNamespace('raveio'),
                   inherits = FALSE)

  if(!inherits(generator, "R6ClassGenerator")){
    stop("Cannot find class definition for reference with ", signal_type, " signal type.")
  }
  generator$new(subject = subject, number = number, ...)
}
