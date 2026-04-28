# Save or load R object in 'JSON' format

Save or load R object in 'JSON' format

## Usage

``` r
save_json(
  x,
  con = stdout(),
  ...,
  digits = ceiling(-log10(.Machine$double.eps)),
  pretty = TRUE,
  serialize = TRUE
)

load_json(con, ..., map = NULL)
```

## Arguments

- x:

  R object to save

- con:

  file or connection

- ...:

  other parameters to pass into
  [`toJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  or
  [`fromJSON`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)

- digits:

  number of digits to save

- pretty:

  whether the output should be pretty

- serialize:

  whether to save a serialized version of `x`; see 'Examples'.

- map:

  a map to save the results

## Value

`save_json` returns nothing; `load_json` returns an R object.

## Examples

``` r

# Serialize
save_json(list(a = 1, b = function(){}))
#> {
#>   "type": "list",
#>   "attributes": {
#>     "names": {
#>       "type": "character",
#>       "attributes": {},
#>       "value": ["a", "b"]
#>     }
#>   },
#>   "value": [
#>     {
#>       "type": "double",
#>       "attributes": {},
#>       "value": [1]
#>     },
#>     {
#>       "type": "closure",
#>       "attributes": {},
#>       "value": [
#>         {
#>           "type": "language",
#>           "attributes": {},
#>           "value": ["{", "}"]
#>         }
#>       ]
#>     }
#>   ]
#> }

# use toJSON
save_json(list(a = 1, b = function(){}), serialize = FALSE)
#> {
#>   "a": [1],
#>   "b": ["function () ", "{", "}"]
#> }


# Demo of using serializer
f1 <- tempfile(fileext = ".json")
save_json(x ~ y + 1, f1)

load_json(f1)
#> x ~ y + 1
#> <environment: 0x126f29820>

unlink(f1)

```
