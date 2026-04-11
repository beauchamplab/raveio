# Class definition to load data from 'BlackRock' 'Micro-systems' files

Currently only supports minimum file specification version `2.3`. Please
contact the package maintainer or 'RAVE' team if older specifications
are needed

## Value

absolute file path

absolute file paths

nothing

a data frame

a list of spike 'waveform' (without normalization)

a normalized numeric vector (analog signals with `'uV'` as the unit)

## Public fields

- `block`:

  character, session block ID

## Active bindings

- `base_path`:

  absolute base path to the file

- `version`:

  'NEV' specification version

- `electrode_table`:

  electrode table

- `sample_rate_nev_timestamp`:

  sample rate of 'NEV' data packet time-stamps

- `has_nsx`:

  named vector of 'NSx' availability

- `recording_duration`:

  recording duration of each 'NSx'

- `sample_rates`:

  sampling frequencies of each 'NSx' file

## Methods

### Public methods

- [`BlackrockFile$print()`](#method-BlackrockFile-print)

- [`BlackrockFile$new()`](#method-BlackrockFile-new)

- [`BlackrockFile$nev_path()`](#method-BlackrockFile-nev_path)

- [`BlackrockFile$nsx_paths()`](#method-BlackrockFile-nsx_paths)

- [`BlackrockFile$refresh_data()`](#method-BlackrockFile-refresh_data)

- [`BlackrockFile$get_epoch()`](#method-BlackrockFile-get_epoch)

- [`BlackrockFile$get_waveform()`](#method-BlackrockFile-get_waveform)

- [`BlackrockFile$get_electrode()`](#method-BlackrockFile-get_electrode)

- [`BlackrockFile$clone()`](#method-BlackrockFile-clone)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print user-friendly messages

#### Usage

    BlackrockFile$print()

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    BlackrockFile$new(path, block, nev_data = TRUE)

#### Arguments

- `path`:

  the path to 'BlackRock' file, can be with or without file extensions

- `block`:

  session block ID; default is the file name

- `nev_data`:

  whether to load comments and 'waveforms'

------------------------------------------------------------------------

### Method `nev_path()`

get 'NEV' file path

#### Usage

    BlackrockFile$nev_path()

------------------------------------------------------------------------

### Method `nsx_paths()`

get 'NSx' file paths

#### Usage

    BlackrockFile$nsx_paths(which = NULL)

#### Arguments

- `which`:

  which signal file to get, or `NULL` to return all available paths,
  default is `NULL`; must be integers

------------------------------------------------------------------------

### Method `refresh_data()`

refresh and load 'NSx' data

#### Usage

    BlackrockFile$refresh_data(force = FALSE, verbose = TRUE, nev_data = FALSE)

#### Arguments

- `force`:

  whether to force reload data even if the data has been loaded and
  cached before

- `verbose`:

  whether to print out messages when loading

- `nev_data`:

  whether to refresh 'NEV' extended data; default is false

------------------------------------------------------------------------

### Method `get_epoch()`

get epoch table from the 'NEV' comment data packet

#### Usage

    BlackrockFile$get_epoch()

------------------------------------------------------------------------

### Method `get_waveform()`

get 'waveform' of the spike data

#### Usage

    BlackrockFile$get_waveform()

------------------------------------------------------------------------

### Method `get_electrode()`

get electrode data

#### Usage

    BlackrockFile$get_electrode(electrode, nstype = NULL)

#### Arguments

- `electrode`:

  integer, must be a length of one

- `nstype`:

  which signal bank, for example, `'ns3'`, `'ns5'`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BlackrockFile$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
