# Load from 'BrainVision' file

Read in `'eeg'` or `'ieeg'` data from 'BrainVision' files with `.eeg` or
`.dat` extensions.

## Usage

``` r
read_eeg_header(file)

read_eeg_marker(file)

read_eeg_data(header, path = NULL)
```

## Arguments

- file:

  path to `'vhdr'` header file

- header:

  header object returned by `read_eeg_header`

- path:

  optional, path to data file if original data file is missing or
  renamed; must be absolute path.

## Value

`read_eeg_header` returns a list containing information below:

- raw:

  raw header contents

- common:

  a list of descriptors of header

- channels:

  table of channels, including number, reference, resolution and unit

- sample_rate:

  sampling frequency

- root_path:

  directory to where the data is stored

- channel_counts:

  total channel counts

- markers:

  `NULL` if marker file is missing, or list of marker description and
  table containing 6 columns.

`read_eeg_data` returns header, signal data and data description:

- data:

  a matrix of signal values. Each row is a channel and each column is a
  time point.

## Details

A 'BrainVision' dataset is usually stored separately in header file
(`.vhdr`), marker file (`.vmrk`, optional) and data file (`.eeg` or
`.dat`). These files must store under a same folder to be read into R.

Header data contains channel information. Data "channel" contains
channel name, reference, resolution and physical unit. "resolution"
times digital data values is the physical value of the recorded data.
`read_eeg_data` makes this conversion internally . "unit" is the
physical unit of recordings. By default `'uV'` means micro-volts.

Marker file that ends with `.vmrk` is optional. If the file is indicated
by header file and exists, then a marker table will be included when
reading headers. A marker table contains six columns: marker number,
type, description, start position (in data point), size (duration in
data points), and target channel (0 means applied for all channels).

Signal file name is usually contained within header file. Therefore it
is desired that the signal file name never changed once created.
However, in some cases when the signal files are renamed and cannot be
indexed by header files, please specify `path` to force load signals
from a different file.

## Examples

``` r
header_file <- 'sub-01_ses-01_task-visual_run-01_ieeg.vhdr'

if( file.exists(header_file) ){
  # load a subject header
  header <- read_eeg_header(header_file)

  # load entire signal
  data <- read_eeg_data(header)

  data$description
}
```
