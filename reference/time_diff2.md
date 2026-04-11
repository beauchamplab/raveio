# Calculate time difference in seconds

Calculate time difference in seconds

## Usage

``` r
time_diff2(start, end, units = "secs", label = "")
```

## Arguments

- start, end:

  start and end of timer

- units:

  passed to
  [`time_delta`](https://dipterix.org/dipsaus/reference/time_delta.html)

- label:

  `rave-units` label for display purpose.

## Value

A number inherits `rave-units` class.

## See also

[`as_rave_unit`](as_rave_unit.md)

## Examples

``` r
start <- Sys.time()
Sys.sleep(0.1)
end <- Sys.time()
dif <- time_diff2(start, end, label = 'Running ')
print(dif, digits = 4)
#> Running 0.1377 secs

is.numeric(dif)
#> [1] TRUE

dif + 1
#> Running 1.14 secs
```
