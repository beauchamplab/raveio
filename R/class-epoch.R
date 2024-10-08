
#' Definition for epoch class
#' @description Trial epoch, contains the following information: \code{Block}
#' experiment block/session string; \code{Time} trial onset within that block;
#' \code{Trial} trial number; \code{Condition} trial condition. Other optional
#' columns are \code{Event_xxx} (starts with "Event"). See
#' \url{https://openwetware.org/wiki/RAVE:Epoching} or more details.
#' @examples
#'
#' # Please download DemoSubject ~700MB from
#' # https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta
#'
#' \dontrun{
#'
#' # Load meta/epoch_auditory_onset.csv from subject demo/DemoSubject
#' epoch <-RAVEEpoch$new(subject = 'demo/DemoSubject',
#'                       name = 'auditory_onset')
#'
#' # first several trials
#' head(epoch$table)
#'
#' # query specific trial
#' old_trial1 <- epoch$trial_at(1)
#'
#' # Create new trial or change existing trial
#' epoch$set_trial(Block = '008', Time = 10,
#'                 Trial = 1, Condition = 'AknownVmeant')
#' new_trial1 <- epoch$trial_at(1)
#'
#' # Compare new and old trial 1
#' rbind(old_trial1, new_trial1)
#'
#' # To get updated trial table, must update first
#' epoch$update_table()
#' head(epoch$table)
#'
#' }
#'
#' @export
RAVEEpoch <- R6::R6Class(
  classname = 'RAVEEpoch',
  lock_objects = FALSE,
  private = list(
    basic_columns = c("Block", "Time", "Trial", "Condition")
  ),
  public = list(

    #' @field name epoch name, character
    name = character(0),

    #' @field subject \code{RAVESubject} instance
    subject = NULL,

    #' @field data a list of trial information, internally used
    data = NULL,

    #' @field table trial epoch table
    table = NULL,

    #' @field .columns epoch column names, internally used
    .columns = character(0),

    #' @description constructor
    #' @param subject \code{RAVESubject} instance or character
    #' @param name character, make sure \code{"epoch_<name>.csv"} is in meta
    #' folder
    initialize = function(subject, name){
      stopifnot2(stringr::str_detect(name, '^[a-zA-Z0-9_]'),
                 msg = 'epoch name can only contain letters[a-zA-Z] digits[0-9] and underscore[_]')

      self$subject <- restore_subject_instance(subject)
      self$name <- name

      self$data <- dipsaus::fastmap2()

      if(name %in% self$subject$epoch_names){
        # load epoch
        table <- self$subject$meta_data('epoch', name)
        for(ii in seq_len(nrow(table))){
          row <- table[ii,]
          self$data[[as.character(row$Trial)]] <- row
        }

        self$.columns <- names(table)
        self$.columns <- self$.columns[!self$.columns %in% c(private$basic_columns, 'X')]
        self$.columns <- self$.columns[!stringr::str_detect(self$.columns, '^X\\.[0-9]+$')]
      }
      self$update_table()

    },

    #' @description get \code{ith} trial
    #' @param i trial number
    #' @param df whether to return as data frame or a list
    trial_at = function(i, df = TRUE){
      re <- as.list(self$data[[as.character(i)]])
      re <- sapply(self$columns, function(nm){
        re <- re[[nm]]
        if(!length(re) || is.na(re)){
          return(NA)
        }
        if(stringr::str_detect(nm, '^Event_.+$')){
          re <- as.numeric(re)
        }
        re
      }, simplify = FALSE, USE.NAMES = TRUE)
      if(df){
        re <- as.data.frame(re, stringsAsFactors = FALSE)
      }
      re
    },

    #' @description manually update table field
    #' @returns \code{self$table}
    update_table = function(){
      self$table <- do.call('rbind', lapply(self$trials, self$trial_at, df = TRUE))
      self$table
    },

    #' @description set one trial
    #' @param Block block string
    #' @param Time time in second
    #' @param Trial positive integer, trial number
    #' @param Condition character, trial condition
    #' @param ... other key-value pairs corresponding to other optional columns
    set_trial = function(Block, Time, Trial, Condition, ...){
      stopifnot2(Block %in% self$subject$blocks, msg = 'invalid block')
      self$data[[as.character(Trial)]] <- list(Block = Block, Time = Time, Trial = Trial, Condition = Condition, ...)
      names(list(...))
      more_cols <- setdiff(names(list(...)), self$.columns)
      if(length(more_cols)){
        self$.columns <- c(self$.columns, more_cols)
      }
      self$trial_at(Trial)
    },

    #' @description Get epoch column name that represents the desired event
    #' @param event a character string of the event, see
    #' \code{$available_events} for all available events; set to
    #' \code{"trial onset"}, \code{"default"}, or blank to use the default
    #' @param missing what to do if event is missing; default is to warn
    #' @returns If \code{event} is one of \code{"trial onset"},
    #' \code{"default"}, \code{""}, or \code{NULL}, then the result will be
    #' \code{"Time"} column; if the event is found, then return will be the
    #' corresponding event column. When the event is not found and
    #' \code{missing} is \code{"error"}, error will be raised; default is
    #' to return \code{"Time"} column, as it's trial onset and is mandatory.
    get_event_colname = function(event = "",
                                 missing = c("warning", "error", "none")) {
      missing <- match.arg(missing)
      event <- trimws(tolower(paste(event, collapse = " ")))
      if(event %in% c("trial onset", "", "default")) {
        return("Time")
      }
      cname <- sprintf(c("Event_%s", "Event%s"), event)
      cnames <- self$columns
      re <- cnames[tolower(cnames) %in% tolower(cname)]
      if( length(re) ) {
        return(re[[1]])
      }
      msg <- sprintf("Cannot find event `%s`. Returning default `Time`.", event)
      switch(
        missing,
        "warning" = warning(msg),
        "error" = stop(msg)
      )
      return("Time")
    },

    #' @description Get condition column name that represents the desired
    #' condition type
    #' @param condition_type a character string of the condition type, see
    #' \code{$available_condition_type} for all available condition types;
    #' set to \code{"default"} or blank to use the default
    #' @param missing what to do if condition type is missing; default is to
    #' warn if the condition column is not found.
    #' @returns If \code{condition_type} is one of
    #' \code{"default"}, \code{""}, or \code{NULL}, then the result will be
    #' \code{"Condition"} column; if the condition type is found, then return
    #' will be the corresponding condition type column. When the condition type
    #' is not found and \code{missing} is \code{"error"}, error will be raised;
    #' default is to return \code{"Condition"} column, as it's the default
    #' and is mandatory.
    get_condition_colname = function(condition_type, missing = c("warning", "error", "none")) {
      stopifnot(length(condition_type) == 1)
      missing <- match.arg(missing)
      condition_type <- tolower(condition_type)
      if( condition_type %in% c("", "default") ) {
        return("Condition")
      }
      cname <- sprintf(c("Condition_%s", "Condition%s"), condition_type)
      cnames <- self$columns
      re <- cnames[tolower(cnames) %in% tolower(cname)]
      if( length(re) ) {
        return(re[[1]])
      }
      msg <- sprintf("Cannot find condition type `%s`. Returning default `Condition`", condition_type)
      switch(
        missing,
        "warning" = warning(msg),
        "error" = stop(msg)
      )
      return("Condition")
    }

  ),
  active = list(

    #' @field columns columns of trial table
    columns = function(){
      unique(c(private$basic_columns, self$.columns))
    },

    #' @field n_trials total number of trials
    n_trials = function(){
      length(self$data)
    },

    #' @field trials trial numbers
    trials = function(){
      sort(as.integer(names(self$data)))
    },

    #' @field available_events available events other than trial onset
    available_events = function() {
      cnames <- self$columns
      cnames <- cnames[startsWith(cnames, "Event")]
      if(!length(cnames)) { return("") }
      unique(c("", gsub("^Event[_]{0,1}", "", cnames)))
    },

    #' @field available_condition_type available condition type other than
    #' the default
    available_condition_type = function() {
      cnames <- self$columns
      cnames <- cnames[startsWith(cnames, "Condition")]
      if(!length(cnames)) { return("") }
      unique(c("", gsub("^Condition[_]{0,1}", "", cnames)))
    }
  )
)
