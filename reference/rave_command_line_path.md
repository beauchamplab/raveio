# Find and execute external command-line tools

Find and execute external command-line tools

## Usage

``` r
normalize_commandline_path(
  path,
  type = c("dcm2niix", "freesurfer", "fsl", "afni", "others"),
  unset = NA
)

cmd_dcm2niix(error_on_missing = TRUE, unset = NA)

cmd_freesurfer_home(error_on_missing = TRUE, unset = NA)

cmd_fsl_home(error_on_missing = TRUE, unset = NA)

cmd_afni_home(error_on_missing = TRUE, unset = NA)

cmd_homebrew(error_on_missing = TRUE, unset = NA)

is_dry_run()
```

## Arguments

- path:

  path to normalize

- type:

  type of command

- unset:

  default to return if the command is not found

- error_on_missing:

  whether to raise errors if command is missing

## Value

Normalized path to the command, or `unset` if command is missing.
