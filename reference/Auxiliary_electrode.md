# Class definition for auxiliary channels

Class definition for auxiliary channels

Class definition for auxiliary channels

## Value

If `simplify` is enabled, and only one block is loaded, then the result
will be a vector (`type="voltage"`) or a matrix (others), otherwise the
result will be a named list where the names are the blocks.

## Super class

[`raveio::RAVEAbstarctElectrode`](RAVEAbstarctElectrode.md) -\>
`Auxiliary_electrode`

## Active bindings

- `h5_fname`:

  'HDF5' file name

- `valid`:

  whether current electrode is valid: subject exists and contains
  current electrode or reference; subject electrode type matches with
  current electrode type

- `raw_sample_rate`:

  voltage sample rate

- `preprocess_info`:

  preprocess information

- `voltage_file`:

  path to voltage 'HDF5' file

## Methods

### Public methods

- [`Auxiliary_electrode$print()`](#method-Auxiliary_electrode-print)

- [`Auxiliary_electrode$set_reference()`](#method-Auxiliary_electrode-set_reference)

- [`Auxiliary_electrode$new()`](#method-Auxiliary_electrode-new)

- [`Auxiliary_electrode$.load_noref_voltage()`](#method-Auxiliary_electrode-.load_noref_voltage)

- [`Auxiliary_electrode$.load_raw_voltage()`](#method-Auxiliary_electrode-.load_raw_voltage)

- [`Auxiliary_electrode$load_data()`](#method-Auxiliary_electrode-load_data)

- [`Auxiliary_electrode$load_blocks()`](#method-Auxiliary_electrode-load_blocks)

- [`Auxiliary_electrode$clear_cache()`](#method-Auxiliary_electrode-clear_cache)

- [`Auxiliary_electrode$clear_memory()`](#method-Auxiliary_electrode-clear_memory)

- [`Auxiliary_electrode$clone()`](#method-Auxiliary_electrode-clone)

Inherited methods

- [`raveio::RAVEAbstarctElectrode$set_epoch()`](RAVEAbstarctElectrode.html#method-set_epoch)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print electrode summary

#### Usage

    Auxiliary_electrode$print()

------------------------------------------------------------------------

### Method `set_reference()`

set reference for current electrode

#### Usage

    Auxiliary_electrode$set_reference(reference)

#### Arguments

- `reference`:

  either `NULL` or `LFP_electrode` instance

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    Auxiliary_electrode$new(subject, number, quiet = FALSE)

#### Arguments

- `subject, number, quiet`:

  see constructor in [`RAVEAbstarctElectrode`](RAVEAbstarctElectrode.md)

------------------------------------------------------------------------

### Method `.load_noref_voltage()`

load non-referenced voltage (internally used)

#### Usage

    Auxiliary_electrode$.load_noref_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

- `srate`:

  voltage signal sample rate

------------------------------------------------------------------------

### Method `.load_raw_voltage()`

load raw voltage (no process)

#### Usage

    Auxiliary_electrode$.load_raw_voltage(reload = FALSE)

#### Arguments

- `reload`:

  whether to reload cache

------------------------------------------------------------------------

### Method `load_data()`

method to load electrode data

#### Usage

    Auxiliary_electrode$load_data(type = c("raw-voltage", "voltage"))

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

    Auxiliary_electrode$load_blocks(
      blocks,
      type = c("raw-voltage", "voltage"),
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

    Auxiliary_electrode$clear_cache(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clear_memory()`

method to clear memory

#### Usage

    Auxiliary_electrode$clear_memory(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Auxiliary_electrode$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
