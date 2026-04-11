# A port to [`read_yaml`](https://yaml.r-lib.org/reference/read_yaml.html)

For more examples, see [`save_yaml`](save_yaml.md).

## Usage

``` r
load_yaml(file, ..., map = NULL)
```

## Arguments

- file, ...:

  passed to
  [`read_yaml`](https://yaml.r-lib.org/reference/read_yaml.html)

- map:

  [`fastmap2`](https://dipterix.org/dipsaus/reference/fastmap2.html)
  instance or `NULL`

## Value

A [`fastmap2`](https://dipterix.org/dipsaus/reference/fastmap2.html). If
`map` is provided then return map, otherwise return newly created one

## See also

[`fastmap2`](https://dipterix.org/dipsaus/reference/fastmap2.html),
[`save_yaml`](save_yaml.md),
[`read_yaml`](https://yaml.r-lib.org/reference/read_yaml.html),
[`write_yaml`](https://yaml.r-lib.org/reference/write_yaml.html)
