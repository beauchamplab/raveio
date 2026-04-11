# Definitions of electrode with 'LFP' signal type

Please use a safer [`new_electrode`](new_electrode.md) function to
create instances. This documentation is to describe the member methods
of the electrode class `LFP_electrode`

## Value

if the reference number if `NULL` or `'noref'`, then returns 0,
otherwise returns a
[`FileArray-class`](https://dipterix.org/filearray/reference/FileArray-class.html)

If `simplify` is enabled, and only one block is loaded, then the result
will be a vector (`type="voltage"`) or a matrix (others), otherwise the
result will be a named list where the names are the blocks.

## Super class

[`raveio::RAVEAbstarctElectrode`](RAVEAbstarctElectrode.md) -\>
`LFP_electrode`

## Active bindings

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

- [`LFP_electrode$print()`](#method-LFP_electrode-print)

- [`LFP_electrode$set_reference()`](#method-LFP_electrode-set_reference)

- [`LFP_electrode$new()`](#method-LFP_electrode-new)

- [`LFP_electrode$.load_noref_wavelet()`](#method-LFP_electrode-.load_noref_wavelet)

- [`LFP_electrode$.load_noref_voltage()`](#method-LFP_electrode-.load_noref_voltage)

- [`LFP_electrode$.load_wavelet()`](#method-LFP_electrode-.load_wavelet)

- [`LFP_electrode$.load_voltage()`](#method-LFP_electrode-.load_voltage)

- [`LFP_electrode$.load_raw_voltage()`](#method-LFP_electrode-.load_raw_voltage)

- [`LFP_electrode$load_data()`](#method-LFP_electrode-load_data)

- [`LFP_electrode$load_blocks()`](#method-LFP_electrode-load_blocks)

- [`LFP_electrode$clear_cache()`](#method-LFP_electrode-clear_cache)

- [`LFP_electrode$clear_memory()`](#method-LFP_electrode-clear_memory)

- [`LFP_electrode$clone()`](#method-LFP_electrode-clone)

Inherited methods

- [`raveio::RAVEAbstarctElectrode$set_epoch()`](RAVEAbstarctElectrode.html#method-set_epoch)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print electrode summary

#### Usage

    LFP_electrode$print()

------------------------------------------------------------------------

### Method `set_reference()`

set reference for current electrode

#### Usage

    LFP_electrode$set_reference(reference)

#### Arguments

- `reference`:

  either `NULL` or `LFP_electrode` instance

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    LFP_electrode$new(subject, number, quiet = FALSE)

#### Arguments

- `subject, number, quiet`:

  see constructor in [`RAVEAbstarctElectrode`](RAVEAbstarctElectrode.md)

------------------------------------------------------------------------

### Method `.load_noref_wavelet()`

load non-referenced wavelet coefficients (internally used)

#### Usage

    LFP_electrode$.load_noref_wavelet(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `.load_noref_voltage()`

load non-referenced voltage (internally used)

#### Usage

    LFP_electrode$.load_noref_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

- `srate`:

  voltage signal sample rate

------------------------------------------------------------------------

### Method `.load_wavelet()`

load referenced wavelet coefficients (internally used)

#### Usage

    LFP_electrode$.load_wavelet(
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

    LFP_electrode$.load_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `.load_raw_voltage()`

load raw voltage (no process)

#### Usage

    LFP_electrode$.load_raw_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `load_data()`

method to load electrode data

#### Usage

    LFP_electrode$load_data(
      type = c("power", "phase", "voltage", "wavelet-coefficient", "raw-voltage")
    )

#### Arguments

- `type`:

  data type such as `"power"`, `"phase"`, `"voltage"`,
  `"wavelet-coefficient"`, and `"raw-voltage"`. For `"power"`,
  `"phase"`, and `"wavelet-coefficient"`, 'Wavelet' transforms are
  required. For `"voltage"`, 'Notch' filters must be applied. All these
  types except for `"raw-voltage"` will be referenced. For
  `"raw-voltage"`, no reference will be performed since the data will be
  the "raw" signal (no processing).

------------------------------------------------------------------------

### Method `load_blocks()`

load electrode block-wise data (with no reference), useful when epoch is
absent

#### Usage

    LFP_electrode$load_blocks(
      blocks,
      type = c("power", "phase", "voltage", "wavelet-coefficient", "raw-voltage"),
      simplify = TRUE
    )

#### Arguments

- `blocks`:

  session blocks

- `type`:

  data type such as `"power"`, `"phase"`, `"voltage"`, `"raw-voltage"`
  (with no filters applied, as-is from imported),
  `"wavelet-coefficient"`. Note that if type is `"raw-voltage"`, then
  the data only needs to be imported; for `"voltage"` data, 'Notch'
  filters must be applied; for all other types, 'Wavelet' transforms are
  required.

- `simplify`:

  whether to simplify the result

------------------------------------------------------------------------

### Method `clear_cache()`

method to clear cache on hard drive

#### Usage

    LFP_electrode$clear_cache(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clear_memory()`

method to clear memory

#### Usage

    LFP_electrode$clear_memory(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LFP_electrode$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Download subject demo/DemoSubject

subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)

if(dir.exists(subject$path)) {

# Electrode 14 in demo/DemoSubject
e <- new_electrode(subject = subject, number = 14, signal_type = "LFP")

# Load CAR reference "ref_13-16,24"
ref <- new_reference(subject = subject, number = "ref_13-16,24",
                     signal_type = "LFP")
e$set_reference(ref)

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

# Draw baseline
tempfile <- tempfile()
bl <- power_baseline(power, baseline_windows = c(-1, 0),
                     method = "decibel", filebase = tempfile)
collapsed_power <- collapse2(bl, keep = c(2,1))
# Visualize
dname <- dimnames(bl)
image(collapsed_power, x = dname$Time, y = dname$Frequency,
      xlab = "Time (s)", ylab = "Frequency (Hz)",
      main = "Mean power over trial (Baseline: -1~0 seconds)",
      sub = glue('Electrode {e$number} (Reference: {ref$number})'))
abline(v = 0, lty = 2, col = 'blue')
text(x = 0, y = 20, "Audio onset", col = "blue", cex = 0.6)

# clear cache on hard disk
e$clear_cache()
ref$clear_cache()

}
```
