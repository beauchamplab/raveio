#' Get \code{\link{RAVESubject}} instance from character
#' @param subject_id character in format \code{"project/subject"}
#' @param strict whether to check if subject directories exist or not
#' @param reload whether to reload (update) subject information, default is true
#' @return \code{\link{RAVESubject}} instance
#' @seealso \code{\link{RAVESubject}}
#' @export
as_rave_subject <- function(subject_id, strict = TRUE, reload = TRUE){
  if(inherits(subject_id, 'RAVESubject')){
    if(reload) {
      return(restore_subject_instance(subject_id$subject_id, strict = strict))
    } else {
      return(subject_id)
    }
  } else if(inherits(subject_id, 'RAVEPreprocessSettings')) {
    if(reload) {
      return(restore_subject_instance(subject_id$subject$subject_id, strict = strict))
    } else {
      return(subject_id$subject)
    }
  } else {
    return(restore_subject_instance(subject_id, strict = strict))
  }

}

restore_subject_instance <- function(subject_id, strict = FALSE) {
  if(inherits(subject_id, 'RAVESubject')){
    return(subject_id)
  } else {
    if(inherits(subject_id, "Subject")) {
      # RAVE 1.0 subject instance
      stopifnot2(is.character(subject_id$id),
                 msg = "`as_rave_subject`: Cannot find subject ID from the given input")
      subject_id <- subject_id$id
    }
    RAVESubject$new(subject_id, strict = strict)
  }
}

#' Defines 'RAVE' subject class
#' @description \code{R6} class definition
#' @export
RAVESubject <- R6::R6Class(
  classname = 'RAVESubject',
  class = TRUE,
  portable = FALSE,
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
      dir_create2(self$pipeline_path)
      dir_create2(self$note_path)

      # save preprocess
      self$preprocess_settings$save()

      if(include_freesurfer){
        if(is.na(self$freesurfer_path) || !dir.exists(self$freesurfer_path)){
          path <- as.character(file.path(self$preprocess_settings$raw_path, 'rave-imaging'))
          dir_create2(path)
        }
      }
    },

    #' @description set default key-value pair for the subject, used by 'RAVE'
    #' modules
    #' @param key character
    #' @param value value of the key
    #' @param namespace file name of the note (without post-fix)
    #' @return The same as \code{value}
    set_default = function(key, value, namespace = "default"){

      stopifnot2(is.character(key) && length(key) == 1, msg = "`key` must be a character of length 1")
      stopifnot2(is.character(namespace) && length(namespace) == 1, msg = "`namespace` must be a character of length 1")

      stopifnot2(!grepl("[^A-Za-z0-9_-]", namespace), msg = "`namespace` can only contain letters, digits, dash (-), and/or underscore (_)")


      force(value)
      if(!dir.exists(self$note_path)){
        dir_create2(self$note_path)
      }
      default_path <- file.path(self$note_path, sprintf("%s.json", namespace))
      default_path_backup <- file.path(self$note_path, sprintf("%s.yaml", namespace))
      defaults <- dipsaus::fastmap2()
      if(file.exists(default_path)){
        load_json(default_path, map = defaults)
      } else if (file.exists(default_path_backup)) {
        load_yaml(default_path_backup, map = defaults)
      }

      old_val <- defaults[[key]]
      if(is.null(old_val)) {
        defaults[[key]] <- structure(
          list(), entry_value = value, timestamp = Sys.time(),
          class = "raveio-subject-entry"
        )
      } else if( !identical(attr(old_val, "entry_value"), value) ){
        defaults[[key]] <- structure(
          list(), entry_value = value, timestamp = Sys.time(),
          previous_value = old_val, class = "raveio-subject-entry"
        )
      }

      # defaults[[key]] <- value
      tmpfile <- tempfile()
      on.exit({ unlink(tmpfile) })
      save_json(x = as.list(defaults), con = tmpfile, serialize = TRUE)
      file.copy(tmpfile, default_path, overwrite = TRUE, recursive = FALSE)

      # get backup yaml format
      entry_names <- sort(names(defaults))
      entries <- structure(
        lapply(entry_names, function(nm) {
          val <- defaults[[nm]]
          if(inherits(val, "raveio-subject-entry")) {
            return(attr(val, "entry_value"))
          }
          return(val)
        }),
        names = entry_names
      )
      save_yaml(x = entries, file = tmpfile)
      file.copy(tmpfile, default_path_backup, overwrite = TRUE, recursive = FALSE)
      invisible(value)
    },

    #' @description get default key-value pairs for the subject, used by 'RAVE'
    #' modules
    #' @param ... single key, or a vector of character keys
    #' @param default_if_missing default value is any key is missing
    #' @param simplify whether to simplify the results if there is only one key
    #' to fetch; default is \code{TRUE}
    #' @param namespace file name of the note (without post-fix)
    #' @return A named list of key-value pairs, or if one key is specified and
    #' \code{simplify=TRUE}, then only the value will be returned.
    get_default = function(..., default_if_missing = NULL, simplify = TRUE,
                           namespace = "default"){
      stopifnot2(is.character(namespace) && length(namespace) == 1, msg = "`namespace` must be a character of length 1")
      stopifnot2(!grepl("[^A-Za-z0-9_-]", namespace), msg = "`namespace` can only contain letters, digits, dash (-), and/or underscore (_)")
      default_path <- file.path(self$note_path, sprintf("%s.json", namespace))
      default_path_backup <- file.path(self$note_path, sprintf("%s.yaml", namespace))

      defaults <- dipsaus::fastmap2(missing_default = default_if_missing)

      if(file.exists(default_path)){
        load_json(con = default_path, map = defaults)
      } else if (file.exists(default_path_backup)) {
        load_yaml(default_path_backup, map = defaults)
      }

      re <- defaults[...]
      re <- structure(lapply(re, function(val) {
        if(inherits(val, "raveio-subject-entry")) {
          return(attr(val, "entry_value"))
        } else {
          return(val)
        }
      }), names = names(re))

      if(simplify && length(re) == 1){
        re <- re[[1]]
      }
      re
    },

    #' @description get summary table of all the key-value pairs used by 'RAVE'
    #' modules for the subject
    #' @param namespaces namespaces for the entries; see method
    #' \code{get_default} or \code{set_default}. Default is all possible
    #' namespaces
    #' @param include_history whether to include history entries; default is
    #' false
    #' @return A data frame with four columns: \code{'namespace'} for the group
    #' name of the entry (entries within the same namespace usually share same
    #' module), \code{'timestamp'} for when the entry was registered.
    #' \code{'entry_name'} is the name of the entry. If \code{include_history}
    #' is true, then multiple entries with the same \code{'entry_name'} might
    #' appear since the obsolete entries are included. \code{'entry_value'}
    #' is the value of the corresponding entry.
    get_note_summary = function(namespaces, include_history = FALSE) {

      if(missing(namespaces)) {
        # get all possible namespaces
        namespaces <- list.files(
          path = self$note_path,
          pattern = "\\.(json|yaml)$",
          all.files = TRUE,
          recursive = FALSE,
          full.names = FALSE,
          ignore.case = TRUE,
          include.dirs = FALSE,
          no.. = TRUE
        )
        namespaces <- unique(gsub(pattern = "\\.(json|yaml)$", replacement = "",
                                  x = namespaces, ignore.case = TRUE))
        if("default" %in% namespaces) {
          namespaces <- unique(c("default", namespaces))
        }
      }

      entries <- dipsaus::fastqueue2()
      extract_entries <- function(entry, name, namespace, is_root = FALSE) {
        if(inherits(entry, "raveio-subject-entry")) {
          timestamp <- attr(entry, "timestamp")
          if(!inherits(timestamp, "POSIXct")) {
            timestamp <- NA
          }
          item <- list(
            namespace = namespace,
            name = name,
            timestamp = timestamp,
            value = attr(entry, "entry_value"),
            status = ifelse(is_root, "current", "obsolete")
          )
        } else {
          item <- list(
            namespace = namespace,
            name = name,
            timestamp = NA,
            value = entry,
            status = ifelse(is_root, "current", "obsolete")
          )
        }

        entries$add(item)

        # add previous record
        if( include_history ) {
          previous_entry <- attr(entry, "previous_value")
          if(!is.null(previous_entry)) {
            Recall(entry = previous_entry, name = name, namespace = namespace, is_root = FALSE)
          }
        }
      }

      lapply(namespaces, function(namespace) {
        default_path <- file.path(self$note_path, sprintf("%s.json", namespace))
        default_path_backup <- file.path(self$note_path, sprintf("%s.yaml", namespace))

        defaults <- dipsaus::fastmap2(missing_default = default_if_missing)

        if(file.exists(default_path)){
          try({ load_json(con = default_path, map = defaults) })
        } else if (file.exists(default_path_backup)) {
          try({ load_yaml(default_path_backup, map = defaults) })
        } else { return(NULL) }

        for(nm in names(defaults)) {
          if(nm != "") {
            try({ extract_entries(defaults[[nm]], name = nm, namespace = namespace, is_root = TRUE) })
          }
        }
        return()
      })
      notes <- entries$as_list()

      notes_df <- data.frame(
        timestamp = as.POSIXct(sapply(notes, "[[", "timestamp"), origin = "1960-01-01"),
        namespace = vapply(notes, "[[", "namespace", FUN.VALUE = ""),
        entry_name = vapply(notes, "[[", "name", FUN.VALUE = "")
      )
      notes_df$entry_value <- lapply(notes, "[[", "value")
      notes_df$status <- vapply(notes, "[[", "status", FUN.VALUE = "")

      notes_df
    },


    #' @description check and get subject's epoch information
    #' @param epoch_name epoch name, depending on the subject's meta files
    #' @param as_table whether to convert to \code{\link{data.frame}}; default
    #' is false
    #' @param trial_starts the start of the trial relative to epoch time;
    #' default is 0
    #' @return If \code{as_table} is \code{FALSE}, then returns as
    #' \code{\link{RAVEEpoch}} instance; otherwise returns epoch table; will
    #' raise errors when file is missing or the epoch is invalid.
    get_epoch = function(epoch_name, as_table = FALSE, trial_starts = 0){
      if(length(epoch_name) != 1){
        stop("Only one epoch is allowed at a time.")
      }
      if(!isTRUE(epoch_name %in% self$epoch_names)){
        stop("Subject ", self$subject_id, " has no epoch name called: ", sQuote(epoch_name), "\n  Please check folder\n    ", self$meta_path, "\n  and make sure ", sQuote(sprintf("epoch_%s.csv", epoch_name)), " exists.")
      }
      epoch <- RAVEEpoch$new(subject = self, name = epoch_name)
      if(!length(epoch$trials)){
        stop("Cannot load epoch file correctly: epoch file is missing or corrupted, or there is no trial in the epoch file. A typical RAVE-epoch file contains 4 columns (case-sensitive): Block (characters), Time (numerical), Trial (integer), Condition (characters).")
      }
      # trial starts from -1 sec but only 0.5 seconds are allowed
      invalid_trials <- unlist(lapply(epoch$trials, function(ii){
        info <- epoch$trial_at(ii, df = FALSE)
        if(info$Time + trial_starts < 0){
          return(ii)
        }
        return()
      }))

      if(any(invalid_trials)){
        stop("Trial ", dipsaus::deparse_svec(invalid_trials), " start too soon after the beginning of the sessions (less than ", sprintf("%.2f seconds", -trial_starts), "). Please adjust the trial start time (i.e. ", sQuote("Pre"), " if you are using the RAVE application).")
      }

      if( as_table ){
        epoch <- epoch$table
      }
      epoch
    },

    #' @description check and get subject's reference information
    #' @param reference_name reference name, depending on the subject's meta
    #' file settings
    #' @param simplify whether to only return the reference column
    #' @return If \code{simplify} is true, returns a vector of reference
    #' electrode names, otherwise returns the whole table; will
    #' raise errors when file is missing or the reference is invalid.
    get_reference = function(reference_name, simplify = FALSE){
      if(length(reference_name) != 1){
        stop("Only one reference is allowed at a time.")
      }
      if(!isTRUE(reference_name %in% self$reference_names)){
        stop("Subject ", self$subject_id, " has no reference name called: ", sQuote(reference_name), "\n  Please check folder\n    ", self$meta_path, "\n  and make sure ", sQuote(sprintf("reference_%s.csv", reference_name)), " exists.")
      }

      reference_table <- self$meta_data(meta_type = 'reference', meta_name = reference_name)

      if(!is.data.frame(reference_table)){
        stop("Cannot load reference file correctly. A typical RAVE-reference file contains 4 columns (case-sensitive): Electrode (integer), Group (characters), Reference (characters), Type (characters).")
      }

      if(simplify){
        return(reference_table$Reference)
      }
      reference_table
    },

    #' @description check and get subject's electrode table with electrodes
    #' that are load-able
    #' @param electrodes characters indicating integers such as
    #' \code{"1-14,20-30"}, or integer vector of electrode numbers
    #' @param reference_name see method \code{get_reference}
    #' @param subset whether to subset the resulting data table
    #' @param simplify whether to only return electrodes
    #' @return If \code{simplify} is true, returns a vector of electrodes
    #' that are valid (or won't be excluded) under given reference; otherwise
    #' returns a table. If \code{subset} is true, then the table will be
    #' subset and only rows with electrodes to be loaded will be kept.
    get_electrode_table = function(electrodes, reference_name,
                                   subset = FALSE, simplify = FALSE){
      preproc <- self$preprocess_settings
      all_electrodes <- self$electrodes

      if(!missing(electrodes)){
        # Get electrodes to be loaded
        if(is.character(electrodes)){
          load_electrodes <- dipsaus::parse_svec(electrodes)
        } else {
          load_electrodes <- electrodes
        }
        valid_electrodes <- self$valid_electrodes(reference_name = reference_name)
        # 1. get electrodes to be truly loaded
        load_electrodes <- load_electrodes[load_electrodes %in% valid_electrodes]
        if(!length(load_electrodes)) {
          stop("There is no valid electrodes to be loaded. The valid electrodes are: ", dipsaus::deparse_svec(valid_electrodes), ".")
        }
        sel <- all_electrodes %in% load_electrodes
        if(!all(preproc$has_wavelet[sel])){
          imcomplete <- all_electrodes[all_electrodes %in% load_electrodes & !preproc$has_wavelet]
          stop("The following electrodes do not have power spectrum: \n  ", dipsaus::deparse_svec(imcomplete),
               "\nPlease run wavelet module first.")
        }
        reference_table <- self$get_reference(reference_name, simplify = FALSE)
      } else {
        reference_table <- NULL
      }

      electrode_table <- self$meta_data("electrodes")

      if(!is.data.frame(electrode_table)){

        if(length(self$electrodes)) {
          electrode_table <- data.frame(
            Electrode = self$electrodes,
            Coord_x = 0,
            Coord_y = 0,
            Coord_z = 0,
            Label = "NoLabel",
            SignalType = self$electrode_types
          )
          save_meta2(electrode_table, meta_type = "electrodes",
                             project_name = self$project_name,
                             subject_code = self$subject_code)
          electrode_table <- self$meta_data("electrodes")
          catgl("Cannot load electrode.csv correctly. A basic RAVE-electrode file contains 5 columns (case-sensitive): Electrode (integer), Coord_x (numerical), Coord_y (numerical), Coord_y (numerical), Label (characters). Creating a blank electrode file.", level = "WARNING")
        } else {
          stop("Cannot load electrode.csv correctly. A basic RAVE-electrode file contains 5 columns (case-sensitive): Electrode (integer), Coord_x (numerical), Coord_y (numerical), Coord_y (numerical), Label (characters).")
        }

      }

      if(!is.null(reference_table)){
        electrode_table <- merge(electrode_table, reference_table, by = 'Electrode', all.x = TRUE, all.y = FALSE)
        electrode_table$isLoaded <- electrode_table$Electrode %in% load_electrodes
        if(subset){
          electrode_table <- electrode_table[electrode_table$isLoaded, ]
        }
      }

      if(simplify){
        return(electrode_table$Electrode)
      }
      electrode_table
    },

    #' @description check and get subject's frequency table, time-frequency
    #' decomposition is needed.
    #' @param simplify whether to simplify as vector
    #' @return If \code{simplify} is true, returns a vector of frequencies;
    #' otherwise returns a table.
    get_frequency = function(simplify = TRUE){
      frequency_table <- self$meta_data('frequencies')
      if(!is.data.frame(frequency_table)){
        stop("Cannot load frequency table. Please run wavelet first.")
      }
      if(simplify){
        return(frequency_table$Frequency)
      }
      frequency_table
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
      # 2. rave_data/project/subject/imaging/fs
      # 3. rave_data/project/subject/fs
      # 4. rave_data/project/subject/subject
      # 5. raw_dir/subject/rave-imaging/fs
      # 6. raw_dir/subject/fs

      re <- as.character(file.path(getOption('rave.freesurfer_dir'), self$subject_code))
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
      re <- as.character(file.path(self$path, self$subject_code))
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

initialize_imaging_paths <- function(subject) {
  # subject <- 'demo/DemoSubject'
  subject <- restore_subject_instance(subject, strict = FALSE)
  root_path <- file.path(subject$preprocess_settings$raw_path, "rave-imaging")
  dir_create2(file.path(root_path, "coregistration"))
  dir_create2(file.path(root_path, "log"))
  dir_create2(file.path(root_path, "scripts"))
  dir_create2(file.path(root_path, "inputs"))
  invisible()
}
