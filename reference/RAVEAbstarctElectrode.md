# Abstract definition of electrode class in RAVE

This class is not intended for direct use. Please create new child
classes and implement some key methods.

## Value

If `simplify` is enabled, and only one block is loaded, then the result
will be a vector (`type="voltage"`) or a matrix (others), otherwise the
result will be a named list where the names are the blocks.

## Public fields

- `subject`:

  subject instance ([`RAVESubject`](RAVESubject.md))

- `number`:

  integer stands for electrode number or reference ID

- `reference`:

  reference electrode, either `NULL` for no reference or an electrode
  instance inherits `RAVEAbstarctElectrode`

- `epoch`:

  a [`RAVEEpoch`](RAVEEpoch.md) instance

- `stitch_events`:

  events to stitch, when loading window is not default to trial onset;
  must be `NULL` or a character vector of length 2

## Active bindings

- `type`:

  signal type of the electrode, such as 'LFP', 'Spike', and 'EKG';
  default is 'Unknown'

- `power_enabled`:

  whether the electrode can be used in power analyses such as frequency,
  or frequency-time analyses; this usually requires transforming the
  electrode raw voltage signals using signal processing methods such as
  'Fourier', 'wavelet', 'Hilbert', 'multi-taper', etc. If an electrode
  has power data, then it's power data can be loaded via
  [`prepare_subject_power`](rave-prepare.md) method.

- `is_reference`:

  whether this instance is a reference electrode

- `location`:

  location type of the electrode, see
  [`LOCATION_TYPES`](raveio-constants.md) for details

- `exists`:

  whether electrode exists in subject

- `preprocess_file`:

  path to preprocess 'HDF5' file

- `power_file`:

  path to power 'HDF5' file

- `phase_file`:

  path to phase 'HDF5' file

- `voltage_file`:

  path to voltage 'HDF5' file

- `reference_name`:

  reference electrode name

- `epoch_name`:

  current epoch name

- `cache_root`:

  run-time cache path; `NA` if epoch or trial intervals are missing

- `trial_intervals`:

  trial intervals relative to epoch onset

## Methods

### Public methods

- [`RAVEAbstarctElectrode$new()`](#method-RAVEAbstarctElectrode-new)

- [`RAVEAbstarctElectrode$set_reference()`](#method-RAVEAbstarctElectrode-set_reference)

- [`RAVEAbstarctElectrode$set_epoch()`](#method-RAVEAbstarctElectrode-set_epoch)

- [`RAVEAbstarctElectrode$clear_cache()`](#method-RAVEAbstarctElectrode-clear_cache)

- [`RAVEAbstarctElectrode$clear_memory()`](#method-RAVEAbstarctElectrode-clear_memory)

- [`RAVEAbstarctElectrode$load_data()`](#method-RAVEAbstarctElectrode-load_data)

- [`RAVEAbstarctElectrode$load_blocks()`](#method-RAVEAbstarctElectrode-load_blocks)

- [`RAVEAbstarctElectrode$clone()`](#method-RAVEAbstarctElectrode-clone)

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVEAbstarctElectrode$new(subject, number, quiet = FALSE)

#### Arguments

- `subject`:

  character or [`RAVESubject`](RAVESubject.md) instance

- `number`:

  current electrode number or reference ID

- `quiet`:

  reserved, whether to suppress warning messages

------------------------------------------------------------------------

### Method `set_reference()`

set reference for instance

#### Usage

    RAVEAbstarctElectrode$set_reference(reference)

#### Arguments

- `reference`:

  `NULL` or `RAVEAbstarctElectrode` instance instance

------------------------------------------------------------------------

### Method `set_epoch()`

set epoch instance for the electrode

#### Usage

    RAVEAbstarctElectrode$set_epoch(epoch, stitch_events = NULL)

#### Arguments

- `epoch`:

  characters or [`RAVEEpoch`](RAVEEpoch.md) instance. For characters,
  make sure `"epoch_<name>.csv"` is in meta folder.

- `stitch_events`:

  events to stitch, default is `NULL`, meaning when loading data, the
  time is relative to the trial onset (column `"Time"` in the epoch
  file); set to a character of length 2, representing the events if time
  is not relative to trial onset. Please remove the prefix. For example,
  for a column named `"Event_name"`, the event name is `"name"`.

------------------------------------------------------------------------

### Method `clear_cache()`

method to clear cache on hard drive

#### Usage

    RAVEAbstarctElectrode$clear_cache(...)

#### Arguments

- `...`:

  implemented by child instances

------------------------------------------------------------------------

### Method `clear_memory()`

method to clear memory

#### Usage

    RAVEAbstarctElectrode$clear_memory(...)

#### Arguments

- `...`:

  implemented by child instances

------------------------------------------------------------------------

### Method `load_data()`

method to load electrode data

#### Usage

    RAVEAbstarctElectrode$load_data(type)

#### Arguments

- `type`:

  data type such as `"power"`, `"phase"`, `"voltage"`,
  `"wavelet-coefficient"`, or others depending on child class
  implementations

------------------------------------------------------------------------

### Method `load_blocks()`

load electrode block-wise data (with reference), useful when epoch is
absent

#### Usage

    RAVEAbstarctElectrode$load_blocks(blocks, type, simplify = TRUE)

#### Arguments

- `blocks`:

  session blocks

- `type`:

  data type such as `"power"`, `"phase"`, `"voltage"`,
  `"wavelet-coefficient"`.

- `simplify`:

  whether to simplify the result

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVEAbstarctElectrode$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{

# To run this example, please download demo subject (~700 MB) from
# https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta

generator <- RAVEAbstarctElectrode

# load demo subject electrode 14
e <- generator$new("demo/DemoSubject", number = 14)

# set epoch
e$subject$epoch_names
e$set_epoch("auditory_onset")
head(e$epoch$table)

# set epoch range (-1 to 2 seconds relative to onset)
e$trial_intervals <- c(-1,2)
# or to set multiple ranges
e$trial_intervals <- list(c(-2,-1), c(0, 2))

# set reference
e$subject$reference_names
reference_table <- e$subject$meta_data(
  meta_type = "reference",
  meta_name = "default")
ref_name <- subset(reference_table, Electrode == 14)[["Reference"]]

# the reference is CAR type, mean of electrode 13-16,24
ref_name

# load & set reference
ref <- generator$new(e$subject, ref_name)
e$set_reference(ref)

} # }
```
