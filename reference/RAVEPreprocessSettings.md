# Defines preprocess configurations

`R6` class definition

## Value

list of electrode type, number, etc.

`NULL` when no channel is composed. When `flat` is `TRUE`, a data frame
of weights with the columns composing electrode channel numbers,
composed channel number, and corresponding weights; if `flat` is
`FALSE`, then a weight matrix;

## Public fields

- `current_version`:

  current configuration setting version

- `path`:

  settings file path

- `backup_path`:

  alternative back up path for redundancy checks

- `data`:

  list of raw configurations, internally used only

- `subject`:

  [`RAVESubject`](RAVESubject.md) instance

- `read_only`:

  whether the configuration should be read-only, not yet implemented

## Active bindings

- `version`:

  configure version of currently stored files

- `old_version`:

  whether settings file is old format

- `blocks`:

  experiment blocks

- `electrodes`:

  electrode numbers

- `sample_rates`:

  voltage data sample rate

- `notch_filtered`:

  whether electrodes are notch filtered

- `has_wavelet`:

  whether each electrode has wavelet transforms

- `data_imported`:

  whether electrodes are imported

- `data_locked`:

  whether electrode, blocks and sample rate are locked? usually when an
  electrode is imported into 'rave', that electrode is locked

- `electrode_locked`:

  whether electrode is imported and locked

- `electrode_composed`:

  composed electrode channels, not actual physically contacts, but is
  generated from those physically ones

- `wavelet_params`:

  wavelet parameters

- `notch_params`:

  Notch filter parameters

- `electrode_types`:

  electrode signal types

- `@freeze_blocks`:

  whether to free block, internally used

- `@freeze_lfp_ecog`:

  whether to freeze electrodes that record 'LFP' signals, internally
  used

- `@lfp_ecog_sample_rate`:

  'LFP' sample rates, internally used

- `all_blocks`:

  characters, all possible blocks even not included in some projects

- `raw_path`:

  raw data path

- `raw_path_type`:

  raw data path type, 'native' or 'bids'

## Methods

### Public methods

- [`RAVEPreprocessSettings$new()`](#method-RAVEPreprocessSettings-new)

- [`RAVEPreprocessSettings$valid()`](#method-RAVEPreprocessSettings-valid)

- [`RAVEPreprocessSettings$has_raw()`](#method-RAVEPreprocessSettings-has_raw)

- [`RAVEPreprocessSettings$set_blocks()`](#method-RAVEPreprocessSettings-set_blocks)

- [`RAVEPreprocessSettings$set_electrodes()`](#method-RAVEPreprocessSettings-set_electrodes)

- [`RAVEPreprocessSettings$set_sample_rates()`](#method-RAVEPreprocessSettings-set_sample_rates)

- [`RAVEPreprocessSettings$migrate()`](#method-RAVEPreprocessSettings-migrate)

- [`RAVEPreprocessSettings$electrode_info()`](#method-RAVEPreprocessSettings-electrode_info)

- [`RAVEPreprocessSettings$save()`](#method-RAVEPreprocessSettings-save)

- [`RAVEPreprocessSettings$get_compose_weights()`](#method-RAVEPreprocessSettings-get_compose_weights)

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVEPreprocessSettings$new(subject, read_only = TRUE)

#### Arguments

- `subject`:

  character or [`RAVESubject`](RAVESubject.md) instance

- `read_only`:

  whether subject should be read-only (not yet implemented)

------------------------------------------------------------------------

### Method `valid()`

whether configuration is valid or not

#### Usage

    RAVEPreprocessSettings$valid()

------------------------------------------------------------------------

### Method `has_raw()`

whether raw data folder exists

#### Usage

    RAVEPreprocessSettings$has_raw()

------------------------------------------------------------------------

### Method `set_blocks()`

set blocks

#### Usage

    RAVEPreprocessSettings$set_blocks(blocks, force = FALSE)

#### Arguments

- `blocks`:

  character, combination of session task and run

- `force`:

  whether to ignore checking. Only used when data structure is not
  native, for example, 'BIDS' format

------------------------------------------------------------------------

### Method `set_electrodes()`

set electrodes

#### Usage

    RAVEPreprocessSettings$set_electrodes(
      electrodes,
      type = SIGNAL_TYPES,
      add = FALSE
    )

#### Arguments

- `electrodes`:

  integer vectors

- `type`:

  signal type of electrodes, see [`SIGNAL_TYPES`](raveio-constants.md)

- `add`:

  whether to add to current settings

------------------------------------------------------------------------

### Method `set_sample_rates()`

set sample frequency

#### Usage

    RAVEPreprocessSettings$set_sample_rates(srate, type = SIGNAL_TYPES)

#### Arguments

- `srate`:

  sample rate, must be positive number

- `type`:

  electrode type to set sample rate. In 'rave', all electrodes with the
  same signal type must have the same sample rate.

------------------------------------------------------------------------

### Method `migrate()`

convert old format to new formats

#### Usage

    RAVEPreprocessSettings$migrate(force = FALSE)

#### Arguments

- `force`:

  whether to force migrate and save settings

------------------------------------------------------------------------

### Method `electrode_info()`

get electrode information

#### Usage

    RAVEPreprocessSettings$electrode_info(electrode)

#### Arguments

- `electrode`:

  integer

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

save settings to hard disk

#### Usage

    RAVEPreprocessSettings$save()

------------------------------------------------------------------------

### Method `get_compose_weights()`

get weights of each composed channels

#### Usage

    RAVEPreprocessSettings$get_compose_weights(flat = TRUE)

#### Arguments

- `flat`:

  whether to flatten the data frame; default is true

## Examples

``` r
# The following example require downloading demo subject (~700 MB) from
# https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta

if (FALSE) { # \dontrun{

conf <- RAVEPreprocessSettings$new(subject = 'demo/DemoSubject')
conf$blocks  # "008" "010" "011" "012"

conf$electrodes   # 5 electrodes

# Electrode 14 information
conf$electrode_info(electrode = 14)

conf$data_imported # All 5 electrodes are imported

conf$data_locked   # Whether block, sample rates should be locked

} # }
```
