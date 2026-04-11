# Collapse high-dimensional tensor array

Collapse high-dimensional tensor array

## Usage

``` r
collapse2(x, keep, method = c("mean", "sum"), ...)

# S3 method for class 'FileArray'
collapse2(x, keep, method = c("mean", "sum"), ...)

# S3 method for class 'Tensor'
collapse2(x, keep, method = c("mean", "sum"), ...)

# S3 method for class 'array'
collapse2(x, keep, method = c("mean", "sum"), ...)
```

## Arguments

- x:

  R array,
  [`FileArray-class`](https://dipterix.org/filearray/reference/FileArray-class.html),
  or [`Tensor`](Tensor.md) object

- keep:

  integer vector, the margins to keep

- method:

  character, calculates mean or sum of the array when collapsing

- ...:

  passed to other methods

## Value

A collapsed array (or a vector or matrix), depending on `keep`

## See also

[`collapse`](https://dipterix.org/dipsaus/reference/collapse.html)

## Examples

``` r
x <- array(1:16, rep(2, 4))

collapse2(x, c(3, 2))
#>      [,1] [,2]
#> [1,]  5.5  7.5
#> [2,]  9.5 11.5

# Alternative method, but slower when `x` is a large array
apply(x, c(3, 2), mean)
#>      [,1] [,2]
#> [1,]  5.5  7.5
#> [2,]  9.5 11.5

# filearray
y <- filearray::as_filearray(x)

collapse2(y, c(3, 2))
#>      [,1] [,2]
#> [1,]  5.5  7.5
#> [2,]  9.5 11.5

collapse2(y, c(3, 2), "sum")
#>      [,1] [,2]
#> [1,]   22   30
#> [2,]   38   46

# clean up
y$delete(force = TRUE)
```
