# Archive and share a subject

Archive and share a subject

## Usage

``` r
archive_subject(
  subject,
  path,
  includes = c("orignal_signals", "processed_data", "rave_imaging", "pipelines", "notes",
    "user_generated"),
  config = list(),
  work_path = NULL,
  zip_flags = NULL
)
```

## Arguments

- subject:

  'RAVE' subject to archive

- path:

  path to a zip file to store; if missing or empty, then the path will
  be automatically created

- includes:

  data to include in the archive; default includes all ( original raw
  signals, processed signals, imaging files, stored pipelines, notes,
  and user-generated exports)

- config:

  a list of configurations, including changing subject code, project
  name, or to exclude cache data; see examples

- work_path:

  temporary working path where files are copied; default is temporary
  path. Set this variable explicitly when temporary path is on external
  drives (for example, users have limited storage on local drives and
  cannot hold the entire subject)

- zip_flags:

  [`zip`](https://rdrr.io/r/utils/zip.html) flags

## Examples

``` r
# This example requires you to install demo subject

if (FALSE) { # \dontrun{


# Basic usage
path <- archive_subject('demo/DemoSubject')

# clean up
unlink(path)

# Advanced usage: include all the original signals
# and processed data, no cache data, re-name to
# demo/DemoSubjectLite
path <- archive_subject(
  'demo/DemoSubject',
  includes = c("orignal_signals", "processed_data"),
  config = list(
    rename = list(
      project_name = "demo",
      subject_code = "DemoSubjectLite"
    ),
    orignal_signals = list(
      # include all raw signals
      include_all = TRUE
    ),
    processed_data = list(
      include_cache = FALSE
    )
  )
)

# Clean up temporary zip file
unlink(path)

} # }

```
