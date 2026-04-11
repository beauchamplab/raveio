# Defines 'RAVE' subject class

`R6` class definition

## Value

data frame

integer vector of valid electrodes

The same as `value`

A named list of key-value pairs, or if one key is specified and
`simplify=TRUE`, then only the value will be returned.

A data frame with four columns: `'namespace'` for the group name of the
entry (entries within the same namespace usually share same module),
`'timestamp'` for when the entry was registered. `'entry_name'` is the
name of the entry. If `include_history` is true, then multiple entries
with the same `'entry_name'` might appear since the obsolete entries are
included. `'entry_value'` is the value of the corresponding entry.

If `as_table` is `FALSE`, then returns as [`RAVEEpoch`](RAVEEpoch.md)
instance; otherwise returns epoch table; will raise errors when file is
missing or the epoch is invalid.

If `simplify` is true, returns a vector of reference electrode names,
otherwise returns the whole table; will raise errors when file is
missing or the reference is invalid.

If `simplify` is true, returns a vector of electrodes that are valid (or
won't be excluded) under given reference; otherwise returns a table. If
`subset` is true, then the table will be subset and only rows with
electrodes to be loaded will be kept.

If `simplify` is true, returns a vector of frequencies; otherwise
returns a table.

A table of pipeline registry

A `PipelineTools` instance

## See also

[`load_meta2`](load_meta2.md)

## Active bindings

- `project`:

  project instance of current subject; see
  [`RAVEProject`](RAVEProject.md)

- `project_name`:

  character string of project name

- `subject_code`:

  character string of subject code

- `subject_id`:

  subject ID: `"project/subject"`

- `path`:

  subject root path

- `rave_path`:

  'rave' directory under subject root path

- `meta_path`:

  meta data directory for current subject

- `imaging_path`:

  root path to imaging processing folder

- `freesurfer_path`:

  'FreeSurfer' directory for current subject. If no path exists, values
  will be `NA`

- `preprocess_path`:

  preprocess directory under subject 'rave' path

- `data_path`:

  data directory under subject 'rave' path

- `cache_path`:

  path to 'FST' copies under subject 'data' path

- `pipeline_path`:

  path to pipeline scripts under subject's folder

- `note_path`:

  path that stores 'RAVE' related subject notes

- `epoch_names`:

  possible epoch names

- `reference_names`:

  possible reference names

- `reference_path`:

  reference path under 'rave' folder

- `preprocess_settings`:

  preprocess instance; see
  [`RAVEPreprocessSettings`](RAVEPreprocessSettings.md)

- `blocks`:

  subject experiment blocks in current project

- `electrodes`:

  all electrodes, no matter excluded or not

- `raw_sample_rates`:

  voltage sample rate

- `power_sample_rate`:

  power spectrum sample rate

- `has_wavelet`:

  whether electrodes have wavelet transforms

- `notch_filtered`:

  whether electrodes are Notch-filtered

- `electrode_types`:

  electrode signal types

- `electrode_composed`:

  composed electrode channels, not actual physically contacts, but is
  generated from those physically ones

## Methods

### Public methods

- [`RAVESubject$print()`](#method-RAVESubject-print)

- [`RAVESubject$new()`](#method-RAVESubject-new)

- [`RAVESubject$meta_data()`](#method-RAVESubject-meta_data)

- [`RAVESubject$valid_electrodes()`](#method-RAVESubject-valid_electrodes)

- [`RAVESubject$initialize_paths()`](#method-RAVESubject-initialize_paths)

- [`RAVESubject$set_default()`](#method-RAVESubject-set_default)

- [`RAVESubject$get_default()`](#method-RAVESubject-get_default)

- [`RAVESubject$get_note_summary()`](#method-RAVESubject-get_note_summary)

- [`RAVESubject$get_epoch()`](#method-RAVESubject-get_epoch)

- [`RAVESubject$get_reference()`](#method-RAVESubject-get_reference)

- [`RAVESubject$get_electrode_table()`](#method-RAVESubject-get_electrode_table)

- [`RAVESubject$get_frequency()`](#method-RAVESubject-get_frequency)

- [`RAVESubject$list_pipelines()`](#method-RAVESubject-list_pipelines)

- [`RAVESubject$load_pipeline()`](#method-RAVESubject-load_pipeline)

- [`RAVESubject$clone()`](#method-RAVESubject-clone)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

override print method

#### Usage

    RAVESubject$print(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVESubject$new(project_name, subject_code = NULL, strict = TRUE)

#### Arguments

- `project_name`:

  character project name

- `subject_code`:

  character subject code

- `strict`:

  whether to check if subject folders exist

------------------------------------------------------------------------

### Method `meta_data()`

get subject meta data located in `"meta/"` folder

#### Usage

    RAVESubject$meta_data(
      meta_type = c("electrodes", "frequencies", "time_points", "epoch", "references"),
      meta_name = "default"
    )

#### Arguments

- `meta_type`:

  choices are 'electrodes', 'frequencies', 'time_points', 'epoch',
  'references'

- `meta_name`:

  if `meta_type='epoch'`, read in `'epoch_<meta_name>.csv'`; if
  `meta_type='references'`, read in `'reference_<meta_name>.csv'`.

------------------------------------------------------------------------

### Method `valid_electrodes()`

get valid electrode numbers

#### Usage

    RAVESubject$valid_electrodes(reference_name, refresh = FALSE)

#### Arguments

- `reference_name`:

  character, reference name, see `meta_name` in `self$meta_data` or
  [`load_meta2`](load_meta2.md) when `meta_type` is 'reference'

- `refresh`:

  whether to reload reference table before obtaining data, default is
  false

------------------------------------------------------------------------

### Method `initialize_paths()`

create subject's directories on hard disk

#### Usage

    RAVESubject$initialize_paths(include_freesurfer = TRUE)

#### Arguments

- `include_freesurfer`:

  whether to create 'FreeSurfer' path

------------------------------------------------------------------------

### Method `set_default()`

set default key-value pair for the subject, used by 'RAVE' modules

#### Usage

    RAVESubject$set_default(key, value, namespace = "default")

#### Arguments

- `key`:

  character

- `value`:

  value of the key

- `namespace`:

  file name of the note (without post-fix)

------------------------------------------------------------------------

### Method `get_default()`

get default key-value pairs for the subject, used by 'RAVE' modules

#### Usage

    RAVESubject$get_default(
      ...,
      default_if_missing = NULL,
      simplify = TRUE,
      namespace = "default"
    )

#### Arguments

- `...`:

  single key, or a vector of character keys

- `default_if_missing`:

  default value is any key is missing

- `simplify`:

  whether to simplify the results if there is only one key to fetch;
  default is `TRUE`

- `namespace`:

  file name of the note (without post-fix)

------------------------------------------------------------------------

### Method `get_note_summary()`

get summary table of all the key-value pairs used by 'RAVE' modules for
the subject

#### Usage

    RAVESubject$get_note_summary(namespaces, include_history = FALSE)

#### Arguments

- `namespaces`:

  namespaces for the entries; see method `get_default` or `set_default`.
  Default is all possible namespaces

- `include_history`:

  whether to include history entries; default is false

------------------------------------------------------------------------

### Method `get_epoch()`

check and get subject's epoch information

#### Usage

    RAVESubject$get_epoch(epoch_name, as_table = FALSE, trial_starts = 0)

#### Arguments

- `epoch_name`:

  epoch name, depending on the subject's meta files

- `as_table`:

  whether to convert to
  [`data.frame`](https://rdrr.io/r/base/data.frame.html); default is
  false

- `trial_starts`:

  the start of the trial relative to epoch time; default is 0

------------------------------------------------------------------------

### Method `get_reference()`

check and get subject's reference information

#### Usage

    RAVESubject$get_reference(reference_name, simplify = FALSE)

#### Arguments

- `reference_name`:

  reference name, depending on the subject's meta file settings

- `simplify`:

  whether to only return the reference column

------------------------------------------------------------------------

### Method `get_electrode_table()`

check and get subject's electrode table with electrodes that are
load-able

#### Usage

    RAVESubject$get_electrode_table(
      electrodes,
      reference_name,
      subset = FALSE,
      simplify = FALSE
    )

#### Arguments

- `electrodes`:

  characters indicating integers such as `"1-14,20-30"`, or integer
  vector of electrode numbers

- `reference_name`:

  see method `get_reference`

- `subset`:

  whether to subset the resulting data table

- `simplify`:

  whether to only return electrodes

------------------------------------------------------------------------

### Method `get_frequency()`

check and get subject's frequency table, time-frequency decomposition is
needed.

#### Usage

    RAVESubject$get_frequency(simplify = TRUE)

#### Arguments

- `simplify`:

  whether to simplify as vector

------------------------------------------------------------------------

### Method `list_pipelines()`

list saved pipelines

#### Usage

    RAVESubject$list_pipelines(
      pipeline_name,
      cache = FALSE,
      check = TRUE,
      all = FALSE
    )

#### Arguments

- `pipeline_name`:

  pipeline ID

- `cache`:

  whether to use cache registry to speed up

- `check`:

  whether to check if the pipelines exist

- `all`:

  whether to list all pipelines; default is false; pipelines with the
  same label but older time-stamps will be hidden

------------------------------------------------------------------------

### Method `load_pipeline()`

load saved pipeline

#### Usage

    RAVESubject$load_pipeline(directory)

#### Arguments

- `directory`:

  pipeline directory name

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESubject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
