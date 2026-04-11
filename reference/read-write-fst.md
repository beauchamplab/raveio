# Read a 'fst' file

Read a 'fst' file

## Usage

``` r
save_fst(x, path, ...)

load_fst(path, ..., as.data.table = TRUE)
```

## Arguments

- x:

  data frame to write to path

- path:

  path to 'fst' file: must not be connection.

- ...:

  passed to `read_fst` or
  [`write_fst`](http://www.fstpackage.org/reference/write_fst.md)

- as.data.table:

  passed to `read_fst` in `fst` package
