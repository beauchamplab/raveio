# Read 'Matlab' files

A compatible reader that can read both 'Matlab' files prior and after
version 6.0

## Usage

``` r
read_mat(file, ram = TRUE, engine = c("r", "py"))

read_mat2(file, ram = TRUE, engine = c("r", "py"))
```

## Arguments

- file:

  path to a 'Matlab' file

- ram:

  whether to load data into memory. Only available when the file is in
  'HDF5' format. Default is false and will load arrays, if set to true,
  then lazy-load data. This is useful when array is very large.

- engine:

  method to read the file, choices are `'r'` and `'py'` ('Python'); if
  `'py'` is chosen, make sure
  [`configure_conda`](http://dipterix.org/rpymat/reference/conda-env.md)
  is configured.

## Value

A list of All the data stored in the file

## Details

[`io_read_mat`](http://dipterix.org/ieegio/reference/low-level-read-write.md)
can only read 'Matlab' files prior to version 6. After version 6,
'Matlab' uses 'HDF5' format to store its data, and `read_mat` can handle
both cases.

The performance of `read_mat` can be limited when the file is too big or
has many datasets as it reads all the data contained in 'Matlab' file
into memory.

## See also

[`io_read_mat`](http://dipterix.org/ieegio/reference/low-level-read-write.md),
[`load_h5`](load_h5.md)

## Examples

``` r
# Matlab .mat <= v7.3
x <- matrix(1:16, 4)
f <- tempfile()
ieegio::io_write_mat(list(x = x), con = f)

read_mat(f)
#> Using native approach to load the matlab file (supporting MAT 5.0)...
#> $x
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    5    9   13
#> [2,]    2    6   10   14
#> [3,]    3    7   11   15
#> [4,]    4    8   12   16
#> 
#> attr(,"header")
#> attr(,"header")$description
#> [1] "MATLAB 5.0 MAT-file, Platform: unix, Software: R v4.6.0, Created on: Tue Apr 28 19:44:26 2026                               "
#> 
#> attr(,"header")$version
#> [1] "5"
#> 
#> attr(,"header")$endian
#> [1] "little"
#> 

# Matlab .mat >= v7.3, using hdf5
# Make sure you have installed hdf5r
if( dipsaus::package_installed('hdf5r') ){

f <- tempfile()
save_h5(x, file = f, name = 'x')

read_mat(f)

# For v7.3, you don't have to load all data into RAM
dat <- read_mat(f, ram = FALSE)
dat

dat$x[]

}
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpSRKuDX/file166a449f50c1 => x (Dataset Created)
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpSRKuDX/file166a449f50c1 => x (Dataset Removed)
#> /var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T//RtmpSRKuDX/file166a449f50c1 => x (Dataset Created)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    5    9   13
#> [2,]    2    6   10   14
#> [3,]    3    7   11   15
#> [4,]    4    8   12   16


```
