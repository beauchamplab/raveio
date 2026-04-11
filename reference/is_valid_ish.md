# Check if data is close to “valid"

Check if data is close to “valid"

## Usage

``` r
is_valid_ish(
  x,
  min_len = 1,
  max_len = Inf,
  mode = NA,
  na = TRUE,
  blank = FALSE,
  all = FALSE
)
```

## Arguments

- x:

  data to check

- min_len, max_len:

  minimal and maximum length

- mode:

  which storage mode (see [`mode`](https://rdrr.io/r/base/mode.html))
  should `x` be considered valid. Default is `NA`: disabled.

- na:

  whether `NA` values considered invalid?

- blank:

  whether blank string considered invalid?

- all:

  if `na` or `blank` is true, whether all element of `x` being invalid
  will result in failure?

## Value

logicals whether input `x` is valid

## Examples

``` r
# length checks
is_valid_ish(NULL)                     # FALSE
#> [1] FALSE
is_valid_ish(integer(0))               # FALSE
#> [1] FALSE
is_valid_ish(integer(0), min_len = 0)  # TRUE
#> [1] TRUE
is_valid_ish(1:10, max_len = 9)        # FALSE
#> [1] FALSE

# mode check
is_valid_ish(1:10)                     # TRUE
#> [1] TRUE
is_valid_ish(1:10, mode = 'numeric')   # TRUE
#> [1] TRUE
is_valid_ish(1:10, mode = 'character') # FALSE
#> [1] FALSE

# NA or blank checks
is_valid_ish(NA)                     # FALSE
#> [1] FALSE
is_valid_ish(c(1,2,NA), all = FALSE) # FALSE
#> [1] FALSE
is_valid_ish(c(1,2,NA), all = TRUE)  # TRUE as not all elements are NA
#> [1] TRUE

is_valid_ish(c('1',''), all = FALSE)  # TRUE
#> [1] TRUE
is_valid_ish(1:3, all = FALSE)        # TRUE as 1:3 are not characters
#> [1] TRUE

```
