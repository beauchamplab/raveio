# Definitions of reference with 'LFP' signal type

Please use a safer [`new_reference`](new_electrode.md) function to
create instances. This documentation is to describe the member methods
of the electrode class `LFP_reference`

## Value

if the reference number if `NULL` or `'noref'`, then returns 0,
otherwise returns a
[`FileArray-class`](https://dipterix.org/filearray/reference/FileArray-class.html)

If `simplify` is enabled, and only one block is loaded, then the result
will be a vector (`type="voltage"`) or a matrix (others), otherwise the
result will be a named list where the names are the blocks.

## Super class

[`raveio::RAVEAbstarctElectrode`](RAVEAbstarctElectrode.md) -\>
`LFP_reference`

## Active bindings

- `exists`:

  whether electrode exists in subject

- `h5_fname`:

  'HDF5' file name

- `valid`:

  whether current electrode is valid: subject exists and contains
  current electrode or reference; subject electrode type matches with
  current electrode type

- `raw_sample_rate`:

  voltage sample rate

- `power_sample_rate`:

  power/phase sample rate

- `preprocess_info`:

  preprocess information

- `power_file`:

  path to power 'HDF5' file

- `phase_file`:

  path to phase 'HDF5' file

- `voltage_file`:

  path to voltage 'HDF5' file

## Methods

### Public methods

- [`LFP_reference$print()`](#method-LFP_reference-print)

- [`LFP_reference$set_reference()`](#method-LFP_reference-set_reference)

- [`LFP_reference$new()`](#method-LFP_reference-new)

- [`LFP_reference$.load_noref_wavelet()`](#method-LFP_reference-.load_noref_wavelet)

- [`LFP_reference$.load_noref_voltage()`](#method-LFP_reference-.load_noref_voltage)

- [`LFP_reference$.load_wavelet()`](#method-LFP_reference-.load_wavelet)

- [`LFP_reference$.load_voltage()`](#method-LFP_reference-.load_voltage)

- [`LFP_reference$load_data()`](#method-LFP_reference-load_data)

- [`LFP_reference$load_blocks()`](#method-LFP_reference-load_blocks)

- [`LFP_reference$clear_cache()`](#method-LFP_reference-clear_cache)

- [`LFP_reference$clear_memory()`](#method-LFP_reference-clear_memory)

- [`LFP_reference$clone()`](#method-LFP_reference-clone)

Inherited methods

- [`raveio::RAVEAbstarctElectrode$set_epoch()`](RAVEAbstarctElectrode.html#method-set_epoch)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print reference summary

#### Usage

    LFP_reference$print()

------------------------------------------------------------------------

### Method `set_reference()`

set reference for current electrode

#### Usage

    LFP_reference$set_reference(reference)

#### Arguments

- `reference`:

  either `NULL` or `LFP_electrode` instance

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    LFP_reference$new(subject, number, quiet = FALSE)

#### Arguments

- `subject, number, quiet`:

  see constructor in [`RAVEAbstarctElectrode`](RAVEAbstarctElectrode.md)

------------------------------------------------------------------------

### Method `.load_noref_wavelet()`

load non-referenced wavelet coefficients (internally used)

#### Usage

    LFP_reference$.load_noref_wavelet(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `.load_noref_voltage()`

load non-referenced voltage (internally used)

#### Usage

    LFP_reference$.load_noref_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

- `srate`:

  voltage signal sample rate

------------------------------------------------------------------------

### Method `.load_wavelet()`

load referenced wavelet coefficients (internally used)

#### Usage

    LFP_reference$.load_wavelet(
      type = c("power", "phase", "wavelet-coefficient"),
      reload = FALSE
    )

#### Arguments

- `type`:

  type of data to load

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `.load_voltage()`

load referenced voltage (internally used)

#### Usage

    LFP_reference$.load_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `load_data()`

method to load electrode data

#### Usage

    LFP_reference$load_data(
      type = c("power", "phase", "voltage", "wavelet-coefficient")
    )

#### Arguments

- `type`:

  data type such as `"power"`, `"phase"`, `"voltage"`,
  `"wavelet-coefficient"`.

------------------------------------------------------------------------

### Method `load_blocks()`

load electrode block-wise data (with reference), useful when epoch is
absent

#### Usage

    LFP_reference$load_blocks(
      blocks,
      type = c("power", "phase", "voltage", "wavelet-coefficient"),
      simplify = TRUE
    )

#### Arguments

- `blocks`:

  session blocks

- `type`:

  data type such as `"power"`, `"phase"`, `"voltage"`,
  `"wavelet-coefficient"`. Note that if type is voltage, then 'Notch'
  filters must be applied; otherwise 'Wavelet' transforms are required.

- `simplify`:

  whether to simplify the result

------------------------------------------------------------------------

### Method `clear_cache()`

method to clear cache on hard drive

#### Usage

    LFP_reference$clear_cache(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clear_memory()`

method to clear memory

#### Usage

    LFP_reference$clear_memory(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LFP_reference$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{

# Download subject demo/DemoSubject


subject <- as_rave_subject("demo/DemoSubject")

# Electrode 14 as reference electrode (Bipolar referencing)
e <- new_reference(subject = subject, number = "ref_14",
                   signal_type = "LFP")

# Reference "ref_13-16,24" (CAR or white-matter reference)
ref <- new_reference(subject = subject, number = "ref_13-16,24",
                     signal_type = "LFP")
ref

# Set epoch
e$set_epoch(epoch = 'auditory_onset')

# Set loading window
e$trial_intervals <- list(c(-1, 2))

# Preview
print(e)

# Now epoch power
power <- e$load_data("power")
names(dimnames(power))

# Subset power
subset(power, Time ~ Time < 0, Electrode ~ Electrode == 14)

# clear cache on hard disk
e$clear_cache()

} # }
```
