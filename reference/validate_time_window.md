# Validate time windows to be used

Make sure the time windows are valid intervals and returns a reshaped
window list

## Usage

``` r
validate_time_window(time_windows)
```

## Arguments

- time_windows:

  vectors or a list of time intervals

## Value

A list of time intervals (ordered, length of 2)

## Examples

``` r

# Simple time window
validate_time_window(c(-1, 2))
#> [[1]]
#> [1] -1  2
#> 

# Multiple windows
validate_time_window(c(-1, 2, 3, 5))
#> [[1]]
#> [1] -1  2
#> 
#> [[2]]
#> [1] 3 5
#> 

# alternatively
validate_time_window(list(c(-1, 2), c(3, 5)))
#> [[1]]
#> [1] -1  2
#> 
#> [[2]]
#> [1] 3 5
#> 
validate_time_window(list(list(-1, 2), list(3, 5)))
#> [[1]]
#> [1] -1  2
#> 
#> [[2]]
#> [1] 3 5
#> 


if (FALSE) { # \dontrun{

# Incorrect usage (will raise errors)

  # Invalid interval (length must be two for each intervals)
  validate_time_window(list(c(-1, 2, 3, 5)))

  # Time intervals must be in ascending order
  validate_time_window(c(2, 1))

} # }

```
