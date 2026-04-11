# Simple hard disk speed test

Simple hard disk speed test

## Usage

``` r
test_hdspeed(
  path = tempdir(),
  file_size = 1e+06,
  quiet = FALSE,
  abort_if_slow = TRUE,
  use_cache = FALSE
)
```

## Arguments

- path:

  an existing directory where to test speed, default is temporary local
  directory.

- file_size:

  in bytes, default is 1 MB.

- quiet:

  should verbose messages be suppressed?

- abort_if_slow:

  abort test if hard drive is too slow. This usually happens when the
  hard drive is connected via slow internet: if the write speed is less
  than 0.1 MB per second.

- use_cache:

  if hard drive speed was tested before, abort testing and return cached
  results or not; default is false.

## Value

A vector of two: writing and reading speed in MB per seconds.
