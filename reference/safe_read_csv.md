# Read comma separated value files with given column classes

Read comma separated value files with given column classes

## Usage

``` r
safe_read_csv(
  file,
  header = TRUE,
  sep = ",",
  colClasses = NA,
  skip = 0,
  quote = "\"",
  ...,
  stringsAsFactors = FALSE
)
```

## Arguments

- file, header, sep, colClasses, skip, quote, stringsAsFactors, ...:

  passed to `read.csv`

## Value

A data frame

## Details

Reading a comma separated value file using builtin function `read.csv`
might result in some unexpected behavior. `safe_read_csv` does some
preprocessing on the format so that it take cares of the following
cases.

1\. If `skip` exceeds the maximum rows of the data, return a blank data
frame instead of raising error.

2\. If row names are included in the file, `colClasses` automatically
skip that column and starts from the second column

3\. If length of `colClasses` does not equal to the number of columns,
instead of cycling the class types, we set those columns to be `NA` type
and let `read.csv` decide the default types.

4\. `stringsAsFactors` is by default `FALSE` to be consistent with R
4.0, if the function is called in R 3.x.

## Examples

``` r
f <- tempfile()
x <- data.frame(a = letters[1:10], b = 1:10, c = 2:11)

# ------------------ Auto-detect row names ------------------
# Write with rownames
utils::write.csv(x, f, row.names = LETTERS[2:11])

# read csv with base library utils
table1 <- utils::read.csv(f, colClasses = c('character', 'character'))

# 4 columns including row names
str(table1)
#> 'data.frame':    10 obs. of  4 variables:
#>  $ X: chr  "B" "C" "D" "E" ...
#>  $ a: chr  "a" "b" "c" "d" ...
#>  $ b: chr  "1" "2" "3" "4" ...
#>  $ c: chr  "2" "3" "4" "5" ...

# read csv via safe_read_csv
table2 <- safe_read_csv(f, colClasses = c('character', 'character'))

# row names are automatically detected, hence 3 columns
# Only first columns are characters, the third column is auto
# detected as numeric
str(table2)
#> 'data.frame':    10 obs. of  3 variables:
#>  $ a: chr  "a" "b" "c" "d" ...
#>  $ b: chr  "1" "2" "3" "4" ...
#>  $ c: int  2 3 4 5 6 7 8 9 10 11

# read table without row names
utils::write.csv(x, f, row.names = FALSE)
table2 <- safe_read_csv(f, colClasses = c('character', 'character'))

# still 3 columns, and row names are 1:nrow
str(table2)
#> 'data.frame':    10 obs. of  3 variables:
#>  $ a: chr  "a" "b" "c" "d" ...
#>  $ b: chr  "1" "2" "3" "4" ...
#>  $ c: int  2 3 4 5 6 7 8 9 10 11

# --------------- Blank data frame when nrow too large ---------------
# instead of raising errors, return blank data frame
safe_read_csv(f, skip = 1000)
#> data frame with 0 columns and 0 rows

```
