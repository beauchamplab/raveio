# External shell commands for 'RAVE'

These shell commands are for importing 'DICOM' images to 'Nifti' format,
reconstructing cortical surfaces, and align' the CT' to 'MRI'. The
commands are only tested on 'MacOS' and 'Linux'. On 'Windows' machines,
please use the 'WSL2' system.

## Usage

``` r
cmd_run_3dAllineate(
  subject,
  mri_path,
  ct_path,
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run
)

cmd_execute(
  script,
  script_path,
  command = "bash",
  dry_run = FALSE,
  backup = TRUE,
  args = NULL,
  ...
)

cmd_run_r(
  expr,
  quoted = FALSE,
  verbose = TRUE,
  dry_run = FALSE,
  log_file = tempfile(),
  script_path = tempfile(),
  ...
)

cmd_run_dcm2niix(
  subject,
  src_path,
  type = c("MRI", "CT"),
  merge = c("Auto", "No", "Yes"),
  float = c("Yes", "No"),
  crop = c("No", "Yes", "Ignore"),
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run
)

cmd_run_flirt(
  subject,
  mri_path,
  ct_path,
  dof = 6,
  cost = c("mutualinfo", "leastsq", "normcorr", "corratio", "normmi", "labeldiff", "bbr"),
  search = 90,
  searchcost = c("mutualinfo", "leastsq", "normcorr", "corratio", "normmi", "labeldiff",
    "bbr"),
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run
)

cmd_run_recon_all(
  subject,
  mri_path,
  args = c("-all", "-autorecon1", "-autorecon2", "-autorecon3", "-autorecon2-cp",
    "-autorecon2-wm", "-autorecon2-pial"),
  work_path = NULL,
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run
)

cmd_run_recon_all_clinical(
  subject,
  mri_path,
  work_path = NULL,
  overwrite = FALSE,
  command_path = NULL,
  dry_run = FALSE,
  verbose = dry_run,
  ...
)
```

## Arguments

- subject:

  characters or a [`RAVESubject`](RAVESubject.md) instance

- mri_path:

  the absolute to 'MRI' volume; must in 'Nifti' format

- ct_path:

  the absolute to 'CT' volume; must in 'Nifti' format

- overwrite:

  whether to overwrite existing files; default is false

- command_path:

  command line path if 'RAVE' cannot find the command binary files

- dry_run:

  whether to run in dry-run mode; under such mode, the shell command
  will not execute. This is useful for debugging scripts; default is
  false

- verbose:

  whether to print out the command script; default is true under dry-run
  mode, and false otherwise

- script:

  the shell script

- script_path:

  path to run the script

- command:

  which command to invoke; default is `'bash'`

- backup:

  whether to back up the script file immediately; default is true

- args:

  further arguments in the shell command, especially the 'FreeSurfer'
  reconstruction command

- ...:

  passed to [`system2`](https://rdrr.io/r/base/system2.html), or
  additional arguments

- expr:

  expression to run as command

- quoted:

  whether `expr` is quoted; default is false

- log_file:

  where should log file be stored

- src_path:

  source of the 'DICOM' or 'Nifti' image (absolute path)

- type:

  type of the 'DICOM' or 'Nifti' image; choices are `'MRI'` and `'CT'`

- merge, float, crop:

  `'dcm2niix'` conversion arguments; ignored when the source is in
  'Nifti' format

- dof, cost, search, searchcost:

  parameters used by 'FSL' `'flirt'` command; see their documentation
  for details

- work_path:

  work path for 'FreeSurfer' command;

## Value

A list of data containing the script details:

- `script`:

  script details

- `script_path`:

  where the script should/will be saved

- `dry_run`:

  whether dry-run mode is turned on

- `log_file`:

  path to the log file

- `execute`:

  a function to execute the script
