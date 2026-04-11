# Install a subject from the internet, a zip file or a directory

Install a subject from the internet, a zip file or a directory

## Usage

``` r
install_subject(
  path = ".",
  ask = interactive(),
  overwrite = FALSE,
  backup = TRUE,
  use_cache = TRUE,
  dry_run = FALSE,
  force_project = NA,
  force_subject = NA,
  ...
)
```

## Arguments

- path:

  path to subject archive, can be a path to directory, a zip file, or an
  internet address (must starts with `'http'`, or `'ftp'`)

- ask:

  when `overwrite` is false, whether to ask the user if subject exists;
  default is true when running in interactive session; users will be
  prompt with choices; if `ask=FALSE` and `overwrite=FALSE`, then the
  process will end with a warning if the subject exists.

- overwrite:

  whether to overwrite existing subject, see argument `ask` and `backup`

- backup:

  whether to back-up the subject when overwriting the data; default is
  true, which will rename the old subject folders instead of removing;
  set to true to remove existing subject.

- use_cache:

  whether to use cached extraction directory; default is true. Set it to
  `FALSE` if you want a clean installation.

- dry_run:

  whether to dry-run the process instead of actually installing; this
  rehearsal can help you see the progress and prevent you from losing
  data

- force_project, force_subject:

  force set the project or subject; will raise a warning as this might
  mess up some pipelines

- ...:

  passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Examples

``` r
# Please run 2nd example of function archive_subject

if (FALSE) { # \dontrun{

install_subject(path)

} # }
```
