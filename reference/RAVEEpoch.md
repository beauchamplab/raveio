# Definition for epoch class

Trial epoch, contains the following information: `Block` experiment
block/session string; `Time` trial onset within that block; `Trial`
trial number; `Condition` trial condition. Other optional columns are
`Event_xxx` (starts with "Event"). See
<https://openwetware.org/wiki/RAVE:Epoching> or more details.

## Value

`self$table`

If `event` is one of `"trial onset"`, `"default"`, `""`, or `NULL`, then
the result will be `"Time"` column; if the event is found, then return
will be the corresponding event column. When the event is not found and
`missing` is `"error"`, error will be raised; default is to return
`"Time"` column, as it's trial onset and is mandatory.

If `condition_type` is one of `"default"`, `""`, or `NULL`, then the
result will be `"Condition"` column; if the condition type is found,
then return will be the corresponding condition type column. When the
condition type is not found and `missing` is `"error"`, error will be
raised; default is to return `"Condition"` column, as it's the default
and is mandatory.

## Public fields

- `name`:

  epoch name, character

- `subject`:

  `RAVESubject` instance

- `data`:

  a list of trial information, internally used

- `table`:

  trial epoch table

- `.columns`:

  epoch column names, internally used

## Active bindings

- `columns`:

  columns of trial table

- `n_trials`:

  total number of trials

- `trials`:

  trial numbers

- `available_events`:

  available events other than trial onset

- `available_condition_type`:

  available condition type other than the default

## Methods

### Public methods

- [`RAVEEpoch$new()`](#method-RAVEEpoch-new)

- [`RAVEEpoch$trial_at()`](#method-RAVEEpoch-trial_at)

- [`RAVEEpoch$update_table()`](#method-RAVEEpoch-update_table)

- [`RAVEEpoch$set_trial()`](#method-RAVEEpoch-set_trial)

- [`RAVEEpoch$get_event_colname()`](#method-RAVEEpoch-get_event_colname)

- [`RAVEEpoch$get_condition_colname()`](#method-RAVEEpoch-get_condition_colname)

- [`RAVEEpoch$clone()`](#method-RAVEEpoch-clone)

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVEEpoch$new(subject, name)

#### Arguments

- `subject`:

  `RAVESubject` instance or character

- `name`:

  character, make sure `"epoch_<name>.csv"` is in meta folder

------------------------------------------------------------------------

### Method `trial_at()`

get `ith` trial

#### Usage

    RAVEEpoch$trial_at(i, df = TRUE)

#### Arguments

- `i`:

  trial number

- `df`:

  whether to return as data frame or a list

------------------------------------------------------------------------

### Method `update_table()`

manually update table field

#### Usage

    RAVEEpoch$update_table()

------------------------------------------------------------------------

### Method `set_trial()`

set one trial

#### Usage

    RAVEEpoch$set_trial(Block, Time, Trial, Condition, ...)

#### Arguments

- `Block`:

  block string

- `Time`:

  time in second

- `Trial`:

  positive integer, trial number

- `Condition`:

  character, trial condition

- `...`:

  other key-value pairs corresponding to other optional columns

------------------------------------------------------------------------

### Method `get_event_colname()`

Get epoch column name that represents the desired event

#### Usage

    RAVEEpoch$get_event_colname(
      event = "",
      missing = c("warning", "error", "none")
    )

#### Arguments

- `event`:

  a character string of the event, see `$available_events` for all
  available events; set to `"trial onset"`, `"default"`, or blank to use
  the default

- `missing`:

  what to do if event is missing; default is to warn

------------------------------------------------------------------------

### Method `get_condition_colname()`

Get condition column name that represents the desired condition type

#### Usage

    RAVEEpoch$get_condition_colname(
      condition_type,
      missing = c("warning", "error", "none")
    )

#### Arguments

- `condition_type`:

  a character string of the condition type, see
  `$available_condition_type` for all available condition types; set to
  `"default"` or blank to use the default

- `missing`:

  what to do if condition type is missing; default is to warn if the
  condition column is not found.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVEEpoch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Please download DemoSubject ~700MB from
# https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta

if (FALSE) { # \dontrun{

# Load meta/epoch_auditory_onset.csv from subject demo/DemoSubject
epoch <-RAVEEpoch$new(subject = 'demo/DemoSubject',
                      name = 'auditory_onset')

# first several trials
head(epoch$table)

# query specific trial
old_trial1 <- epoch$trial_at(1)

# Create new trial or change existing trial
epoch$set_trial(Block = '008', Time = 10,
                Trial = 1, Condition = 'AknownVmeant')
new_trial1 <- epoch$trial_at(1)

# Compare new and old trial 1
rbind(old_trial1, new_trial1)

# To get updated trial table, must update first
epoch$update_table()
head(epoch$table)

} # }
```
