# Returns a list of 'RAVE' directories

This function is internally used and should not be called directly.

## Usage

``` r
rave_directories(
  subject_code,
  project_name,
  blocks = NULL,
  .force_format = c("", "native", "BIDS")
)
```

## Arguments

- subject_code:

  'RAVE' subject code

- project_name:

  'RAVE' project name

- blocks:

  session or block names, optional

- .force_format:

  format of the data, default is automatically detected.

## Value

A list of directories
