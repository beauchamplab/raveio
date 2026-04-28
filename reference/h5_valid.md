# Check whether a 'HDF5' file can be opened for read/write

Check whether a 'HDF5' file can be opened for read/write

## Usage

``` r
h5_valid(file, mode = c("r", "w"), close_all = FALSE)
```

## Arguments

- file:

  path to file

- mode:

  `'r'` for read access and `'w'` for write access

- close_all:

  whether to close all connections or just close current connection;
  default is false. Set this to `TRUE` if you want to close all other
  connections to the file

## Value

logical whether the file can be opened.

## Examples

``` r
x <- array(1:27, c(3,3,3))
f <- tempfile()

# No data written to the file, hence invalid
h5_valid(f, 'r')
#> [1] FALSE

save_h5(x, f, 'dset')
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpOZ6CXm/file196535d76ae3 => dset (Dataset Created)
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpOZ6CXm/file196535d76ae3 => dset (Dataset Removed)
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpOZ6CXm/file196535d76ae3 => dset (Dataset Created)
h5_valid(f, 'w')  # TRUE
#> [1] TRUE
h5_valid(f, 'r')  # TRUE
#> [1] TRUE
```
