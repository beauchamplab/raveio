# Validate raw files in 'rave' directory

Validate subjects and returns whether the subject can be imported into
'rave'

## Usage

``` r
validate_raw_file(
  subject_code,
  blocks,
  electrodes,
  format,
  data_type = c("continuous"),
  ...
)

IMPORT_FORMATS
```

## Format

An object of class `list` of length 7.

## Arguments

- subject_code:

  subject code, direct folder under 'rave' raw data path

- blocks:

  block character, direct folder under subject folder. For raw files
  following 'BIDS' convention, see details

- electrodes:

  electrodes to verify

- format:

  integer or character. For characters, run `names(IMPORT_FORMATS)`

- data_type:

  currently only support continuous type of signals

- ...:

  other parameters used if validating `'BIDS'` format; see details.

## Value

logical true or false whether the directory is valid. Attributes
containing error reasons or snapshot of the data. The attributes might
be:

- `snapshot`:

  description of data found if passing the validation

- `valid_run_names`:

  For 'BIDS' format, valid `session+task+run` name if passing the
  validation

- `reason`:

  named list where the names are the reason why validation fails and
  values are corresponding sessions or electrodes or both.

## Details

Six types of raw file structures are supported. They can be basically
classified into two categories: 'rave' native raw structure and
'BIDS-iEEG' structure.

In 'rave' native structure, subject folders are stored within the root
directory, which can be obtained via `raveio_getopt('raw_data_dir')`.
Subject directory is the subject code. Inside of subject folder are
block files. In 'rave', term 'block' is the combination of session,
task, and run. Within each block, there should be 'iEEG' data files.

In 'BIDS-iEEG' format, the root directory can be obtained via
`raveio_getopt('bids_data_dir')`. 'BIDS' root folder contains project
folders. This is unlike 'rave' native raw data format. Subject folders
are stored within the project directories. The subject folders start
with `'sub-'`. Within subject folder, there are session folders with
prefix `'ses-'`. Session folders are optional. 'iEEG' data is stored in
`'ieeg'` folder under the session/subject folder. `'ieeg'` folder should
contain at least

- electrodes.tsv:

  `sub-<label>*_electrodes.tsv`

- 'iEEG' description:

  `sub-<label>*_task-<label>_run-<index>_ieeg.json`

- 'iEEG' data file:

  `sub-<label>*_task-<label>_run-<index>_ieeg.<ext>`, in current 'rave',
  only extensions `'.vhdr+.eeg/.dat'` ('BrainVision') or 'EDF' (or plus)
  are supported.

When format is 'BIDS', `project_name` must be specified.

The following formats are supported:

- `'.mat/.h5 file per electrode per block'`:

  'rave' native raw format, each block folder contains multiple 'Matlab'
  or 'HDF5' files. Each file corresponds to a channel/electrode. File
  names should follow `'xxx001.mat'` or `'xxx001.h5'`. The numbers
  before the extension are channel numbers.

- `'Single .mat/.h5 file per block'`:

  'rave' native raw format, each block folder contains **only** one
  'Matlab' or 'HDF5' file. The file name can be arbitrary, but extension
  must be either `'.mat'` or `'.h5'`. Within the file there should be a
  matrix containing all the data. The short dimension of the matrix will
  be channels, and larger side of the dimension corresponds to the time
  points.

- `'Single EDF(+) file per block'`:

  'rave' native raw format, each block folder contains **only** one
  `'.edf'` file.

- `'Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block'`:

  'rave' native raw format, each block folder contains **only** two
  files. The first file is header `'.vhdr'` file. It contains all meta
  information. The second is either `'.eeg'` or `'.dat'` file containing
  the body, i.e. signal entries.

- `'BIDS & EDF(+)'`:

  'BIDS' format. The data file should have `'.edf'` extension

- `'BIDS & BrainVision (.vhdr+.eeg, .vhdr+.dat)'`:

  'BIDS' format. The data file should have `'.vhdr'+'.eeg/.dat'`
  extensions
