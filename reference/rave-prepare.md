# Prepare 'RAVE' single-subject data

Prepare 'RAVE' single-subject data

## Usage

``` r
prepare_subject_bare0(
  subject,
  electrodes,
  reference_name,
  ...,
  quiet = TRUE,
  repository_id = NULL
)

prepare_subject_bare(
  subject,
  electrodes,
  reference_name,
  ...,
  repository_id = NULL
)

prepare_subject_with_epoch(
  subject,
  electrodes,
  reference_name,
  epoch_name,
  time_windows,
  stitch_events = NULL,
  env = parent.frame(),
  ...
)

prepare_subject_with_blocks(
  subject,
  electrodes,
  reference_name,
  blocks,
  raw = FALSE,
  signal_type = "LFP",
  time_frequency = (!raw && signal_type == "LFP"),
  quiet = raw,
  env = parent.frame(),
  repository_id = NULL,
  ...
)

prepare_subject_phase(
  subject,
  electrodes,
  reference_name,
  epoch_name,
  time_windows,
  stitch_events = NULL,
  signal_type = c("LFP"),
  env = parent.frame(),
  verbose = TRUE,
  ...
)

prepare_subject_power(
  subject,
  electrodes,
  reference_name,
  epoch_name,
  time_windows,
  stitch_events = NULL,
  signal_type = c("LFP"),
  env = parent.frame(),
  verbose = TRUE,
  ...
)

prepare_subject_wavelet(
  subject,
  electrodes,
  reference_name,
  epoch_name,
  time_windows,
  stitch_events = NULL,
  signal_type = c("LFP"),
  env = parent.frame(),
  verbose = TRUE,
  ...
)

prepare_subject_raw_voltage_with_epoch(
  subject,
  electrodes,
  epoch_name,
  time_windows,
  stitch_events = NULL,
  ...,
  quiet = TRUE,
  repository_id = NULL
)

prepare_subject_voltage_with_epoch(
  subject,
  electrodes,
  epoch_name,
  time_windows,
  reference_name,
  stitch_events = NULL,
  ...,
  quiet = TRUE,
  repository_id = NULL
)
```

## Arguments

- subject:

  character of project and subject, such as `"demo/YAB"`, or
  [`RAVESubject`](RAVESubject.md) instance

- electrodes:

  integer vector of electrodes, or a character that can be parsed by
  [`parse_svec`](https://dipterix.org/dipsaus/reference/parse_svec.html)

- reference_name:

  reference name to be loaded

- ...:

  ignored

- quiet:

  whether to quietly load the data

- repository_id:

  used internally

- epoch_name:

  epoch name to be loaded, or a [`RAVEEpoch`](RAVEEpoch.md) instance

- time_windows:

  a list of time windows that are relative to epoch onset time; need to
  pass the validation [`validate_time_window`](validate_time_window.md)

- stitch_events:

  events to stitch, default is `NULL`, meaning when loading data, the
  time is relative to the trial onset (column `"Time"` in the epoch
  file); set to a character of length 2, representing the events if time
  is not relative to trial onset. Please remove the prefix. For example,
  for a column named `"Event_name"`, the event name is `"name"`.

- env:

  environment to evaluate

- blocks:

  one or more session blocks to load

- raw:

  whether to load from original (before processing) data; if true, then
  time-frequency data will not be loaded.

- signal_type:

  electrode signal type (length of one) to be considered; default is
  'LFP'. This option rarely needs to change unless you really want to
  check the power data from other types. For other signal types, check
  [`SIGNAL_TYPES`](raveio-constants.md)

- time_frequency:

  whether to load time-frequency data when preparing block data

- verbose:

  whether to show progress

## Value

A [`fastmap2`](https://dipterix.org/dipsaus/reference/fastmap2.html)
(basically a list) of objects. Depending on the functions called, the
following items may exist in the list:

- `subject`:

  A [`RAVESubject`](RAVESubject.md) instance

- `epoch_name`:

  Same as input `epoch_name`

- `epoch`:

  A [`RAVEEpoch`](RAVEEpoch.md) instance

- `reference_name`:

  Same as input `reference_name`

- `reference_table`:

  A data frame of reference

- `electrode_table`:

  A data frame of electrode information

- `frequency`:

  A vector of frequencies

- `time_points`:

  A vector of time-points

- `power_list`:

  A list of power data of the electrodes

- `power_dimnames`:

  A list of trial indices, frequencies, time points, and electrodes that
  are loaded
