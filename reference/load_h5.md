# Lazy Load 'HDF5' File via [`io_read_h5`](http://dipterix.org/ieegio/reference/io_read_h5.md)

Wrapper for class
[`LazyH5`](http://dipterix.org/ieegio/reference/LazyH5.md), which load
data with "lazy" mode - only read part of dataset when needed.

## Usage

``` r
load_h5(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE)
```

## Arguments

- file:

  'HDF5' file

- name:

  `group/data_name` path to dataset (`H5D` data)

- read_only:

  only used if `ram=FALSE`, whether the returned
  [`LazyH5`](http://dipterix.org/ieegio/reference/LazyH5.md) instance
  should be read only

- ram:

  load data to memory immediately, default is false

- quiet:

  whether to suppress messages

## Value

If `ram` is true, then return data as arrays, otherwise return a
[`LazyH5`](http://dipterix.org/ieegio/reference/LazyH5.md) instance.

## See also

[`save_h5`](save_h5.md)

## Examples

``` r
file <- tempfile()
x <- array(1:120, dim = c(4,5,6))

# save x to file with name /group/dataset/1
save_h5(x, file, '/group/dataset/1', quiet = TRUE)

# read data
y <- load_h5(file, '/group/dataset/1', ram = TRUE)
class(y)   # array
#> [1] "array"

z <- load_h5(file, '/group/dataset/1', ram = FALSE)
class(z)   # LazyH5
#> [1] "LazyH5" "R6"    

dim(z)
#> [1] 4 5 6
```
