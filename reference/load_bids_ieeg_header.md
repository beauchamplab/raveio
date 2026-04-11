# Read in description files from 'BIDS-iEEG' format

Analyze file structures and import all `json` and `tsv` files. File
specification can be found at
<https://bids-specification.readthedocs.io/en/stable/>, chapter
"Modality specific files", section "Intracranial Electroencephalography"
([doi:10.1038/s41597-019-0105-7](https://doi.org/10.1038/s41597-019-0105-7)
). Please note that this function has very limited support on BIDS
format.

## Usage

``` r
load_bids_ieeg_header(bids_root, project_name, subject_code, folder = "ieeg")
```

## Arguments

- bids_root:

  'BIDS' root directory

- project_name:

  project folder name

- subject_code:

  subject code, do not include `"sub-"` prefix

- folder:

  folder name corresponding to 'iEEG' data. It's possible to analyze
  other folders. However, by default, the function is designed for
  `'ieeg'` folder.

## Value

A list containing the information below:

- subject_code:

  character, removed leading `"sub-"`

- project_name:

  character, project name

- has_session:

  whether session/block names are indicated by the file structure

- session_names:

  session/block names indicated by file structure. If missing, then
  session name will be "default"

- paths:

  a list containing path information

- stimuli_path:

  stimuli path, not used for now

- sessions:

  A named list containing meta information for each session/block. The
  names of the list is task name, and the items corresponding to the
  task contains events and channel information. Miscellaneous files are
  stored in "others" variable.

## Examples

``` r
# Download https://github.com/bids-standard/bids-examples/
# extract to directory ~/rave_data/bids_dir/

bids_root <- '~/rave_data/bids_dir/'
project_name <- 'ieeg_visual'

if(dir.exists(bids_root) &&
   dir.exists(file.path(bids_root, project_name, 'sub-01'))){

  header <- load_bids_ieeg_header(bids_root, project_name, '01')

  print(header)

  # sessions
  names(header$sessions)

  # electrodes
  head(header$sessions$`01`$spaces$unknown_space$table)

  # visual task channel settings
  head(header$sessions$`01`$tasks$`01-visual-01`$channels)

  # event table
  head(header$sessions$`01`$tasks$`01-visual-01`$channels)
}
```
