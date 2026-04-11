# Export 'RAVE' data

Export portable data for custom analyses.

## Usage

``` r
rave_export(x, path, ...)

# Default S3 method
rave_export(x, path, format = c("rds", "yaml", "json"), ...)

# S3 method for class 'rave_prepare_subject_raw_voltage_with_epoch'
rave_export(x, path, zip = FALSE, ...)

# S3 method for class 'rave_prepare_subject_voltage_with_epoch'
rave_export(x, path, zip = FALSE, ...)

# S3 method for class 'rave_prepare_power'
rave_export(x, path, zip = FALSE, ...)
```

## Arguments

- x:

  R object or 'RAVE' repositories

- path:

  path to save to

- ...:

  passed to other methods

- format:

  export format

- zip:

  whether to zip the files

## Value

Exported data path

## Examples

``` r
x <- "my data"
path <- tempfile()
rave_export(x, path)
#> [1] "/private/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T/Rtmp9va47M/file490472f567d0"

readRDS(path)
#> [1] "my data"

if (FALSE) { # \dontrun{
  # Needs demo subject
  path <- tempfile()
  x <- prepare_subject_power("demo/DemoSubject")

  # Export power data to path
  rave_export(x, path)
} # }
```
