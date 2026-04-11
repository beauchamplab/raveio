# Ingest signals according to 'RAVE' repository epoch

Ingest signals according to 'RAVE' repository epoch

## Usage

``` r
ingest_regressor(
  repository,
  source,
  source_type = c("channel", "file", "r_object"),
  filter = NULL,
  ...
)
```

## Arguments

- repository:

  'RAVE' repository, for example, generated from
  [`prepare_subject_power`](rave-prepare.md). The repository must
  contain epoch information.

- source:

  data source, see `source_type`

- source_type:

  type of the data source, choices are

  `'channel'`

  :   `source` should be interpreted as subject channels (integer or
      series of integers)

  `'file'`

  :   `source` should be interpreted as file path; the file format must
      be either 'Matlab' or 'HDF5', with data names to be the blocks in
      the "epoch" file. The sample rate of underlying signals must
      coincide with the sample rate presented in the repository

  `'r_object'`

  :   `source` should be a list of R objects, where the names are blocks
      in the "epoch" file and the data are signal traces; The sample
      rate of underlying signals must coincide with the sample rate
      presented in the repository

- filter:

  `NULL` or function; only used if `source_type` is `'channel'`. For
  function, `filter` must have two exact arguments, with the first
  argument taking the signal data matrix (time point by channel) and
  second argument taking sample rates of length 2. The first sample rate
  is the original sampling frequency of the data; the second sample rate
  is sampling frequency derived from the repository.

- ...:

  passed to internal functions.

## Value

A matrix of time by trial
