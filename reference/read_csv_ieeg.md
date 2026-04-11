# Read comma separated value file and ignore headers

Resolved some irregular 'iEEG' format where the header could be missing.

## Usage

``` r
read_csv_ieeg(file, nrows = Inf, drop = NULL)
```

## Arguments

- file:

  comma separated value file to read from. The file must contains all
  numerical values

- nrows:

  number of rows to read

- drop:

  passed to [`fread`](https://rdrr.io/pkg/data.table/man/fread.html)

## Details

The function checks the first two rows of comma separated value file If
the first row has different
[`storage.mode`](https://rdrr.io/r/base/mode.html) than the second row,
then the first row is considered header, otherwise header is treated
missing. Note `file` must have at least two rows.
