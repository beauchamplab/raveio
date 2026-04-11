# Defines 'RAVE' subject class for meta analyses

`R6` class definition

## Value

data frame

## See also

[`load_meta2`](load_meta2.md)

## Super class

[`raveio::RAVESubject`](RAVESubject.md) -\> `RAVEMetaSubject`

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

## Methods

### Public methods

- [`RAVEMetaSubject$print()`](#method-RAVEMetaSubject-print)

- [`RAVEMetaSubject$new()`](#method-RAVEMetaSubject-new)

- [`RAVEMetaSubject$meta_data()`](#method-RAVEMetaSubject-meta_data)

- [`RAVEMetaSubject$clone()`](#method-RAVEMetaSubject-clone)

Inherited methods

- [`raveio::RAVESubject$get_default()`](RAVESubject.html#method-get_default)
- [`raveio::RAVESubject$get_electrode_table()`](RAVESubject.html#method-get_electrode_table)
- [`raveio::RAVESubject$get_epoch()`](RAVESubject.html#method-get_epoch)
- [`raveio::RAVESubject$get_frequency()`](RAVESubject.html#method-get_frequency)
- [`raveio::RAVESubject$get_note_summary()`](RAVESubject.html#method-get_note_summary)
- [`raveio::RAVESubject$get_reference()`](RAVESubject.html#method-get_reference)
- [`raveio::RAVESubject$initialize_paths()`](RAVESubject.html#method-initialize_paths)
- [`raveio::RAVESubject$list_pipelines()`](RAVESubject.html#method-list_pipelines)
- [`raveio::RAVESubject$load_pipeline()`](RAVESubject.html#method-load_pipeline)
- [`raveio::RAVESubject$set_default()`](RAVESubject.html#method-set_default)
- [`raveio::RAVESubject$valid_electrodes()`](RAVESubject.html#method-valid_electrodes)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

override print method

#### Usage

    RAVEMetaSubject$print(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVEMetaSubject$new(project_name, subject_code = NULL, strict = FALSE)

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

    RAVEMetaSubject$meta_data(
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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVEMetaSubject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
