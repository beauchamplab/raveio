# Generate and automatically cache a file array

Avoid repeating yourself

## Usage

``` r
cache_to_filearray(
  fun,
  filebase,
  globals,
  dimension,
  type = "auto",
  partition_size = 1L,
  verbose = FALSE,
  ...
)
```

## Arguments

- fun:

  function that can be called with no mandatory arguments; the result
  should be in a matrix or an array

- filebase:

  where to store the array

- globals:

  names of variables such that any changes should trigger a new
  evaluation of `fun`. This argument is highly recommended to be set
  explicitly (with atomic variables) though the function automatically
  calculates the global variables

- dimension:

  what is the supposed dimension, default is automatically calculated
  from array. If specified explicitly and the file array dimension is
  inconsistent, a new calculation will be triggered.

- type:

  file array type, default is `"auto"`; can be explicitly specified; see
  [`filearray_create`](https://dipterix.org/filearray/reference/filearray.html).
  Inconsistent type will trigger a new calculation.

- partition_size:

  file array partition size; default is `1`; set it to `NA` to generate
  it automatically. Notice inconsistent partition size will not trigger
  calculation if the key variables remain the same

- verbose:

  whether to verbose debug information

- ...:

  passed to
  [`findGlobals`](https://globals.futureverse.org/reference/globalsOf.html)

## Examples

``` r

c <- 2
b <- list(d = matrix(1:9,3))
filebase <- tempfile()

f <- function() {
  message("New calculation")
  re <- c + b$d
  dimnames(re) <- list(A=1:3, B = 11:13)

  # `extra` attribute will be saved
  attr(re, "extra") <- "extra meta data"
  re
}

# first time running
arr <- cache_to_filearray( f, filebase = filebase )
#> New calculation

# cached, no re-run
arr <- cache_to_filearray( f, filebase = filebase )

# file array object
arr
#> Reference class object of class "FileArray"
#> Mode: readwrite 
#> Dimension: 3x3 
#> Partition count: 3 
#> Partition size: 1 
#> Storage type: double (internal size: 8)
#> Location: /private/var/folders/tb/y368xp_x10s3ty1b_mtl5mxr0000gn/T/RtmpOZ6CXm/file19656f40d548 

# read into memory
arr[]
#>    B
#> A   11 12 13
#>   1  3  6  9
#>   2  4  7 10
#>   3  5  8 11

# read extra data
arr$get_header("extra")
#> [1] "extra meta data"

# get digest results
arr$get_header("raveio::filearray_cache")
#> <Digest from code + variables>
#>   variable names: $, +, :, <-, attr, ...
#>   MD5: 4d5806e91c5826c2e57574b332a81112

## Clean up this example
unlink(filebase, recursive = TRUE)
```
