# Import data into 'rave' projects

Import files with predefined structures. Supported file formats include
'Matlab', 'HDF5', 'EDF(+)', 'BrainVision' (`'.eeg/.dat/.vhdr'`).
Supported file structures include 'rave' native structure and 'BIDS'
(very limited) format. Please see
<https://openwetware.org/wiki/RAVE:ravepreprocess> for tutorials.

## Usage

``` r
rave_import(
  project_name,
  subject_code,
  blocks,
  electrodes,
  format,
  sample_rate,
  conversion = NA,
  data_type = "LFP",
  task_runs = NULL,
  add = FALSE,
  ...
)
```

## Arguments

- project_name:

  project name, for 'rave' native structure, this can be any character;
  for 'BIDS' format, this must be consistent with 'BIDS' project name.
  For subjects with multiple tasks, see Section "'RAVE' Project"

- subject_code:

  subject code in character. For 'rave' native structure, this is a
  folder name under raw directory. For 'BIDS', this is subject label
  without `"sub-"` prefix

- blocks:

  characters, for 'rave' native format, this is the folder names subject
  directory; for 'BIDS', this is session name with `"ses-"`. Section
  "Block vs. Session" for different meaning of "blocks" in 'rave' and
  'BIDS'

- electrodes:

  integers electrode numbers

- format:

  integer from 1 to 6, or character. For characters, you can get options
  by running `names(IMPORT_FORMATS)`

- sample_rate:

  sample frequency, must be positive

- conversion:

  physical unit conversion, choices are `NA`, `V`, `mV`, `uV`

- data_type:

  electrode signal type; see [`SIGNAL_TYPES`](raveio-constants.md)

- task_runs:

  for 'BIDS' formats only, see Section "Block vs. Session"

- add:

  whether to add electrodes. If set to true, then only new electrodes
  are allowed to be imported, blocks will be ignored and trying to
  import electrodes that have been imported will still result in error.

- ...:

  other parameters

## Value

None

## 'RAVE' Project

A 'rave' project can be very flexible. A project can refer to a task, a
research objective, or "arbitrarily" as long as you find common research
interests among subjects. One subject can appear in multiple projects
with different blocks, hence `project_name` should be objective-based.
There is no concept of "project" in 'rave' raw directory. When importing
data, you choose subset of blocks from subjects forming a project.

When importing 'BIDS' data into 'rave', `project_name` must be
consistent with 'BIDS' project name as a compromise. Once imported, you
may change the project folder name in imported rave data directory to
other names. Because once raw traces are imported, 'rave' data will
become self-contained and 'BIDS' data are no longer required for
analysis. This naming inconsistency will also be ignored.

## Block vs. Session

'rave' and 'BIDS' have different definitions for a "chunk" of signals.
In 'rave', we use "block". it means combination of session (days), task,
and run, i.e. a block of continuous signals captured. Raw data files are
supposed to be stored in file hierarchy of
`<raw-root>/<subject_code>/<block>/<datafiles>`. In 'BIDS', sessions,
tasks, and runs are separated, and only session names are indicated
under subject folder. Because some previous compatibility issues,
argument `'block'` refers to direct folder names under subject
directories. This means when importing data from 'BIDS' format, `block`
argument needs to be session names to comply with `'subject/block'`
structure, and there is an additional mandatory argument `task_runs`
especially designed for 'BIDS' format.

For 'rave' native raw data format, `block` will be as-is once
imported.  
For 'BIDS' format, `task_runs` will be treated as blocks once imported.

## File Formats

Following file structure. Here use project `"demo"` and subject `"YAB"`
and block `"008")`, electrode `14` as an example.

- `format=1`, or `".mat/.h5 file per electrode per block"`:

  folder `<raw>/YAB/008` contains 'Matlab' or 'HDF5' files per
  electrode. Data file name should look like `"xxx_14.mat"`

- `format=2`, or `"Single .mat/.h5 file per block"`:

  `<raw>/YAB/008` contains only one 'Matlab' or 'HDF5' file. Data within
  the file should be a 2-dimensional matrix, where the column 14 is
  signal recorded from electrode 14

- `format=3`, or `"Single EDF(+) file per block"`:

  `<raw>/YAB/008` contains only one `'edf'` file

- `format=4`, or
  ` "Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block"`:

  `<raw>/YAB/008` contains only one `'vhdr'` file, and the data file
  must be inferred from the header file

- `format=5`, or `"BIDS & EDF(+)"`:

  `<bids>/demo/sub-YAB/ses-008/` must contains `*_electrodes.tsv`, each
  run must have channel file. The channel files and electrode file must
  be consistent in names.  
  Argument `task_runs` is mandatory, characters, combination of session,
  task name, and run number. For example, a task header file in BIDS
  with name `'sub-YAB_ses-008_task-visual_run-01_ieeg.edf'` has
  `task_runs` name as `'008-visual-01'`, where the first `'008'` refers
  to session, `'visual'` is task name, and the second `'01'` is run
  number.

- `format=6`, or ` "BIDS & BrainVision (.vhdr+.eeg, .vhdr+.dat)"`:

  Same as previous format `"BIDS & EDF(+)"`, but data files have
  'BrainVision' formats.
