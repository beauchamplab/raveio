# Get value or return default if invalid

Get value or return default if invalid

## Usage

``` r
get_val2(x, key = NA, default = NULL, na = FALSE, min_len = 1L, ...)
```

## Arguments

- x:

  a list, or environment, or just any R object

- key:

  the name to obtain from `x`. If `NA`, then return x. Default is `NA`

- default:

  default value if

- na, min_len, ...:

  passed to [`is_valid_ish`](is_valid_ish.md)

## Value

values of the keys or default is invalid

## Examples

``` r
x <- list(a=1, b = NA, c = character(0))

# ------------------------ Basic usage ------------------------

# no key, returns x if x is valid
get_val2(x)
#> $a
#> [1] 1
#> 
#> $b
#> [1] NA
#> 
#> $c
#> character(0)
#> 

get_val2(x, 'a', default = 'invalid')
#> [1] 1



# get 'b', NA is not filtered out
get_val2(x, 'b', default = 'invalid')
#> [1] NA

# get 'b', NA is considered invalid
get_val2(x, 'b', default = 'invalid', na = TRUE)
#> [1] "invalid"



# get 'c', length 0 is allowed
get_val2(x, 'c', default = 'invalid', min_len = 0)
#> character(0)

# length 0 is forbidden
get_val2(x, 'c', default = 'invalid', min_len = 1)
#> [1] "invalid"

```
