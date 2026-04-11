# Manipulate cached data on the file systems

Manipulate cached data on the file systems

## Usage

``` r
cache_root(check = FALSE)

clear_cached_files(subject_code, quiet = FALSE)
```

## Arguments

- check:

  whether to ensure the cache root path

- subject_code:

  subject code to remove; default is missing. If `subject_code` is
  provided, then only this subject-related cache files will be removed.

- quiet:

  whether to suppress the message

## Value

`cache_root` returns the root path that stores the 'RAVE' cache data;
`clear_cached_files` returns nothing

## Details

'RAVE' intensively uses cache files. If running on personal computers,
the disk space might be filled up very quickly. These cache files are
safe to remove if there is no 'RAVE' instance running. Function
`clear_cached_files` is designed to remove these cache files. To run
this function, please make sure that all 'RAVE' instances are shutdown.

## Examples

``` r
cache_root()
#> [1] "~/rave_data/cache_dir/"
```
