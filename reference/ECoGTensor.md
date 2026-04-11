# 'iEEG/ECoG' Tensor class inherit from [`Tensor`](Tensor.md)

Four-mode tensor (array) especially designed for 'iEEG/ECoG' data. The
Dimension names are: `Trial`, `Frequency`, `Time`, and `Electrode`.

## Value

a data frame with the dimension names as index columns and `value_name`
as value column

an `ECoGTensor` instance

## Author

Zhengjia Wang

## Super class

[`raveio::Tensor`](Tensor.md) -\> `ECoGTensor`

## Methods

### Public methods

- [`ECoGTensor$flatten()`](#method-ECoGTensor-flatten)

- [`ECoGTensor$new()`](#method-ECoGTensor-new)

Inherited methods

- [`raveio::Tensor$.use_multi_files()`](Tensor.html#method-.use_multi_files)
- [`raveio::Tensor$collapse()`](Tensor.html#method-collapse)
- [`raveio::Tensor$do_finalize()`](Tensor.html#method-do_finalize)
- [`raveio::Tensor$get_data()`](Tensor.html#method-get_data)
- [`raveio::Tensor$operate()`](Tensor.html#method-operate)
- [`raveio::Tensor$print()`](Tensor.html#method-print)
- [`raveio::Tensor$set_data()`](Tensor.html#method-set_data)
- [`raveio::Tensor$subset()`](Tensor.html#method-subset)
- [`raveio::Tensor$to_swap()`](Tensor.html#method-to_swap)
- [`raveio::Tensor$to_swap_now()`](Tensor.html#method-to_swap_now)

------------------------------------------------------------------------

### Method `flatten()`

converts tensor (array) to a table (data frame)

#### Usage

    ECoGTensor$flatten(include_index = TRUE, value_name = "value")

#### Arguments

- `include_index`:

  logical, whether to include dimension names

- `value_name`:

  character, column name of the value

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    ECoGTensor$new(
      data,
      dim,
      dimnames,
      varnames,
      hybrid = FALSE,
      swap_file = temp_tensor_file(),
      temporary = TRUE,
      multi_files = FALSE,
      use_index = TRUE,
      ...
    )

#### Arguments

- `data`:

  array or vector

- `dim`:

  dimension of data, mush match with `data`

- `dimnames`:

  list of dimension names, equal length as `dim`

- `varnames`:

  names of `dimnames`, recommended names are: `Trial`, `Frequency`,
  `Time`, and `Electrode`

- `hybrid`:

  whether to enable hybrid mode to reduce RAM usage

- `swap_file`:

  if hybrid mode, where to store the data; default stores in
  `raveio_getopt('tensor_temp_path')`

- `temporary`:

  whether to clean up the space when exiting R session

- `multi_files`:

  logical, whether to use multiple files instead of one giant file to
  store data

- `use_index`:

  logical, when `multi_files` is true, whether use index dimension as
  partition number

- `...`:

  further passed to [`Tensor`](Tensor.md) constructor
