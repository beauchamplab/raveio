# Save data to comma separated value files with backups

Save comma separated value files, if file exists, backup original file.

## Usage

``` r
safe_write_csv(x, file, ..., quiet = FALSE)
```

## Arguments

- x, file, ...:

  pass to `write.csv`

- quiet:

  whether to suppress overwrite message

## Value

Normalized path of `file`

## Examples

``` r
f <- tempfile()
x <- data.frame(a = 1:10)

# File not exists, same as write file, returns normalized `f`
safe_write_csv(x, f)
#> [1] "/private/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T/RtmpOZ6CXm/file19652bd8cea4"

# Check whether file exists
file.exists(f)
#> [1] TRUE

# write again, and the old file will be copied
safe_write_csv(x, f)
#> [1] "/private/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T/RtmpOZ6CXm/file19652bd8cea4"
```
