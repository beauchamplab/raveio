# Export data frame to different common formats

Stores and load data in various of data format. See 'Details' for
limitations.

## Usage

``` r
export_table(
  x,
  file,
  format = c("auto", "csv", "csv.zip", "h5", "fst", "json", "rds", "yaml"),
  ...
)

import_table(
  file,
  format = c("auto", "csv", "csv.zip", "h5", "fst", "json", "rds", "yaml"),
  ...
)
```

## Arguments

- x:

  data table to be saved to `file`

- file:

  file to store the data

- format:

  data storage format, default is `'auto'` (infer from the file
  extension); other choices are `'csv'`, `'csv.zip'`, `'h5'`, `'fst'`,
  `'json'`, `'rds'`, `'yaml'`

- ...:

  parameters passed to other functions

## Value

The normalized path for `export_table`, and a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) for
`import_table`

## Details

The format `'rds'`, `'h5'`, `'fst'`, `'json'`, and `'yaml'` try to
preserve the first-level column attributes. Factors will be preserved in
these formats. Such property does not exist in `'csv'`, `'csv.zip'`
formats.

Open-data formats are `'h5'`, `'csv'`, `'csv.zip'`, `'json'`, `'yaml'`.
These formats require the table elements to be native types (numeric,
character, factor, etc.).

`'rds'`, `'h5'`, and `'fst'` can store large data sets. `'fst'` is the
best choice is performance and file size are the major concerns. `'rds'`
preserves all the properties of the table.

## Examples

``` r
x <- data.table::data.table(
  a = rnorm(10),
  b = letters[1:10],
  c = 1:10,
  d = factor(LETTERS[1:10])
)

f <- tempfile(fileext = ".csv.zip")

export_table(x = x, file = f)

y <- import_table(file = f)

str(x)
#> Classes ‘data.table’ and 'data.frame':   10 obs. of  4 variables:
#>  $ a: num  0.201 -2.068 0.718 -1.061 -0.753 ...
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  1 2 3 4 5 6 7 8 9 10
#>  $ d: Factor w/ 10 levels "A","B","C","D",..: 1 2 3 4 5 6 7 8 9 10
#>  - attr(*, ".internal.selfref")=<pointer: 0x1310114e0> 
str(y)
#> Classes ‘data.table’ and 'data.frame':   10 obs. of  4 variables:
#>  $ a: num  0.201 -2.068 0.718 -1.061 -0.753 ...
#>  $ b: chr  "a" "b" "c" "d" ...
#>  $ c: int  1 2 3 4 5 6 7 8 9 10
#>  $ d: chr  "A" "B" "C" "D" ...
#>  - attr(*, ".internal.selfref")=<pointer: 0x1310114e0> 

# clean up
unlink(f)

```
