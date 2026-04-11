# R6 Class for large Tensor (Array) in Hybrid Mode

can store on hard drive, and read slices of GB-level data in seconds

## Value

self

the sliced data

a data frame with the dimension names as index columns and `value_name`
as value column

original array

the collapsed data

## Public fields

- `dim`:

  dimension of the array

- `dimnames`:

  dimension names of the array

- `use_index`:

  whether to use one dimension as index when storing data as multiple
  files

- `hybrid`:

  whether to allow data to be written to disk

- `last_used`:

  timestamp of the object was read

- `temporary`:

  whether to remove the files once garbage collected

## Active bindings

- `varnames`:

  dimension names (read-only)

- `read_only`:

  whether to protect the swap files from being changed

- `swap_file`:

  file or files to save data to

## Methods

### Public methods

- [`Tensor$do_finalize()`](#method-Tensor-do_finalize)

- [`Tensor$print()`](#method-Tensor-print)

- [`Tensor$.use_multi_files()`](#method-Tensor-.use_multi_files)

- [`Tensor$new()`](#method-Tensor-new)

- [`Tensor$subset()`](#method-Tensor-subset)

- [`Tensor$flatten()`](#method-Tensor-flatten)

- [`Tensor$to_swap()`](#method-Tensor-to_swap)

- [`Tensor$to_swap_now()`](#method-Tensor-to_swap_now)

- [`Tensor$get_data()`](#method-Tensor-get_data)

- [`Tensor$set_data()`](#method-Tensor-set_data)

- [`Tensor$collapse()`](#method-Tensor-collapse)

- [`Tensor$operate()`](#method-Tensor-operate)

------------------------------------------------------------------------

### Method `do_finalize()`

release resource and remove files for temporary instances

#### Usage

    Tensor$do_finalize()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

print out the data dimensions and snapshot

#### Usage

    Tensor$print(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `.use_multi_files()`

Internally used, whether to use multiple files to cache data instead of
one

#### Usage

    Tensor$.use_multi_files(mult)

#### Arguments

- `mult`:

  logical

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    Tensor$new(
      data,
      dim,
      dimnames,
      varnames,
      hybrid = FALSE,
      use_index = FALSE,
      swap_file = temp_tensor_file(),
      temporary = TRUE,
      multi_files = FALSE
    )

#### Arguments

- `data`:

  numeric array

- `dim`:

  dimension of the array

- `dimnames`:

  dimension names of the array

- `varnames`:

  characters, names of `dimnames`

- `hybrid`:

  whether to enable hybrid mode

- `use_index`:

  whether to use the last dimension for indexing

- `swap_file`:

  where to store the data in hybrid mode files to save data by index;
  default stores in `raveio_getopt('tensor_temp_path')`

- `temporary`:

  whether to remove temporary files when existing

- `multi_files`:

  if `use_index` is true, whether to use multiple

------------------------------------------------------------------------

### Method [`subset()`](https://rdrr.io/r/base/subset.html)

subset tensor

#### Usage

    Tensor$subset(..., drop = FALSE, data_only = FALSE, .env = parent.frame())

#### Arguments

- `...`:

  dimension slices

- `drop`:

  whether to apply [`drop`](https://rdrr.io/r/base/drop.html) on subset
  data

- `data_only`:

  whether just return the data value, or wrap them as a `Tensor`
  instance

- `.env`:

  environment where `...` is evaluated

------------------------------------------------------------------------

### Method `flatten()`

converts tensor (array) to a table (data frame)

#### Usage

    Tensor$flatten(include_index = FALSE, value_name = "value")

#### Arguments

- `include_index`:

  logical, whether to include dimension names

- `value_name`:

  character, column name of the value

------------------------------------------------------------------------

### Method `to_swap()`

Serialize tensor to a file and store it via
[`write_fst`](http://www.fstpackage.org/reference/write_fst.md)

#### Usage

    Tensor$to_swap(use_index = FALSE, delay = 0)

#### Arguments

- `use_index`:

  whether to use one of the dimension as index for faster loading

- `delay`:

  if greater than 0, then check when last used, if not long ago, then do
  not swap to hard drive. If the difference of time is greater than
  `delay` in seconds, then swap immediately.

------------------------------------------------------------------------

### Method `to_swap_now()`

Serialize tensor to a file and store it via
[`write_fst`](http://www.fstpackage.org/reference/write_fst.md)
immediately

#### Usage

    Tensor$to_swap_now(use_index = FALSE)

#### Arguments

- `use_index`:

  whether to use one of the dimension as index for faster loading

------------------------------------------------------------------------

### Method `get_data()`

restore data from hard drive to memory

#### Usage

    Tensor$get_data(drop = FALSE, gc_delay = 3)

#### Arguments

- `drop`:

  whether to apply [`drop`](https://rdrr.io/r/base/drop.html) to the
  data

- `gc_delay`:

  seconds to delay the garbage collection

------------------------------------------------------------------------

### Method `set_data()`

set/replace data with given array

#### Usage

    Tensor$set_data(v)

#### Arguments

- `v`:

  the value to replace the old one, must have the same dimension

- `notice`:

  the a tensor is an environment. If you change at one place, the data
  from all other places will change. So use it carefully.

------------------------------------------------------------------------

### Method `collapse()`

apply mean, sum, or median to collapse data

#### Usage

    Tensor$collapse(keep, method = "mean")

#### Arguments

- `keep`:

  which dimensions to keep

- `method`:

  `"mean"`, `"sum"`, or `"median"`

------------------------------------------------------------------------

### Method `operate()`

apply the tensor by anything along given dimension

#### Usage

    Tensor$operate(
      by,
      fun = .Primitive("/"),
      match_dim,
      mem_optimize = FALSE,
      same_dimension = FALSE
    )

#### Arguments

- `by`:

  R object

- `fun`:

  function to apply

- `match_dim`:

  which dimensions to match with the data

- `mem_optimize`:

  optimize memory

- `same_dimension`:

  whether the return value has the same dimension as the original
  instance

## Examples

``` r
if(!is_on_cran()){

# Create a tensor
ts <- Tensor$new(
  data = 1:18000000, c(3000,300,20),
  dimnames = list(A = 1:3000, B = 1:300, C = 1:20),
  varnames = c('A', 'B', 'C'))

# Size of tensor when in memory is usually large
# `lobstr::obj_size(ts)` -> 8.02 MB

# Enable hybrid mode
ts$to_swap_now()

# Hybrid mode, usually less than 1 MB
# `lobstr::obj_size(ts)` -> 814 kB

# Subset data
start1 <- Sys.time()
subset(ts, C ~ C < 10 & C > 5, A ~ A < 10)
#> Dimension:  9 x 300 x 4
#> - A: 1, 2, 3, 4, 5, 6,...
#> - B: 1, 2, 3, 4, 5, 6,...
#> - C: 6, 7, 8, 9
end1 <- Sys.time(); end1 - start1
#> Time difference of 0.188035 secs

# Join tensors
ts <- lapply(1:20, function(ii){
  Tensor$new(
    data = 1:9000, c(30,300,1),
    dimnames = list(A = 1:30, B = 1:300, C = ii),
    varnames = c('A', 'B', 'C'), use_index = 2)
})
ts <- join_tensors(ts, temporary = TRUE)

}
#> NOT_CRAN is TRUE/true (not on CRAN)
```
