# Convert numeric number into print-friendly format

Convert numeric number into print-friendly format

## Usage

``` r
as_rave_unit(x, unit, label = "")
```

## Arguments

- x:

  numeric or numeric vector

- unit:

  the unit of `x`

- label:

  prefix when printing `x`

## Value

Still numeric, but print-friendly class

## Examples

``` r
sp <- as_rave_unit(1024, 'GB', 'Hard-disk space is ')
print(sp, digits = 0)
#> Hard-disk space is 1024 GB

sp - 12
#> Hard-disk space is 1012.00 GB

as.character(sp)
#> [1] "Hard-disk space is 1024.00 GB"

as.numeric(sp)
#> [1] 1024

# Vectorize
sp <- as_rave_unit(c(500,200), 'MB/s', c('Writing: ', 'Reading: '))
print(sp, digits = 0, collapse = '\n')
#> Writing: 500 MB/s
#> Reading: 200 MB/s
```
