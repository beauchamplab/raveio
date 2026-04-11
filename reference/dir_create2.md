# Force creating directory with checks

Force creating directory with checks

## Usage

``` r
dir_create2(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...)
```

## Arguments

- x:

  path to create

- showWarnings, recursive, ...:

  passed to [`dir.create`](https://rdrr.io/r/base/files2.html)

- check:

  whether to check the directory after creation

## Value

Normalized path

## Examples

``` r
path <- file.path(tempfile(), 'a', 'b', 'c')

# The following are equivalent
dir.create(path, showWarnings = FALSE, recursive = TRUE)

dir_create2(path)

```
