# Function try to load 'fst' arrays, if not found, read 'HDF5' arrays

Function try to load 'fst' arrays, if not found, read 'HDF5' arrays

## Usage

``` r
load_fst_or_h5(
  fst_path,
  h5_path,
  h5_name,
  fst_need_transpose = FALSE,
  fst_need_drop = FALSE,
  ram = FALSE
)
```

## Arguments

- fst_path:

  'fst' file cache path

- h5_path:

  alternative 'HDF5' file path

- h5_name:

  'HDF5' data name

- fst_need_transpose:

  does 'fst' data need transpose?

- fst_need_drop:

  drop dimensions

- ram:

  whether to load to memory directly or perform lazy loading

## Value

If 'fst' cache file exists, returns
[`LazyFST`](http://dipterix.org/ieegio/reference/LazyFST.md) object,
otherwise returns
[`LazyH5`](http://dipterix.org/ieegio/reference/LazyH5.md) instance

## Details

RAVE stores data with redundancy. One electrode data is usually saved
with two copies in different formats: 'HDF5' and 'fst', where 'HDF5' is
cross-platform and supported by multiple languages such as `Matlab`,
`Python`, etc, while 'fst' format is supported by R only, with super
high read/write speed. `load_fst_or_h5` checks whether the presence of
'fst' file, if failed, then it reads data from persistent 'HDF5' file.
