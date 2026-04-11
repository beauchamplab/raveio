# Write named list to file

Write named list to file

## Usage

``` r
save_yaml(x, file, ..., sorted = FALSE)
```

## Arguments

- x:

  a named list,
  [`fastmap2`](https://dipterix.org/dipsaus/reference/fastmap2.html), or
  anything that can be transformed into named list via `as.list`

- file, ...:

  passed to
  [`write_yaml`](https://yaml.r-lib.org/reference/write_yaml.html)

- sorted:

  whether to sort the results by name; default is false

## Value

Normalized file path

## See also

[`fastmap2`](https://dipterix.org/dipsaus/reference/fastmap2.html),
[`load_yaml`](load_yaml.md),
[`read_yaml`](https://yaml.r-lib.org/reference/read_yaml.html),
[`write_yaml`](https://yaml.r-lib.org/reference/write_yaml.html)

## Examples

``` r

x <- list(a = 1, b = 2)
f <- tempfile()

save_yaml(x, f)

load_yaml(f)
#> <Map, size=2, keys=[b, a]>

map <- dipsaus::fastmap2(missing_default = NA)
map$c <- 'lol'
load_yaml(f, map = map)
#> <Map, size=3, keys=[b, a, c]>

map$a
#> [1] 1
map$d
#> [1] NA

```
