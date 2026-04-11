# Import electrode table into subject meta folder

Import electrode table into subject meta folder

## Usage

``` r
import_electrode_table(path, subject, use_fs = NA, dry_run = FALSE, ...)
```

## Arguments

- path:

  path of table file, must be a `'csv'` file

- subject:

  'RAVE' subject ID or instance

- use_fs:

  whether to use 'FreeSurfer' files to calculate other coordinates

- dry_run:

  whether to dry-run the process; if true, then the table will be
  generated but not saved to subject's meta folder

- ...:

  passed to [`read.csv`](https://rdrr.io/r/utils/read.table.html)

## Value

Nothing, the electrode information will be written directly to the
subject's meta directory
