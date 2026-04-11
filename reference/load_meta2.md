# Load 'RAVE' subject meta data

Load 'RAVE' subject meta data

## Usage

``` r
load_meta2(meta_type, project_name, subject_code, subject_id, meta_name)
```

## Arguments

- meta_type:

  electrodes, epochs, time_points, frequencies, references ...

- project_name:

  project name

- subject_code:

  subject code

- subject_id:

  "project_name/subject_code"

- meta_name:

  only used if meta_type is epochs or references

## Value

A data frame of the specified meta type or `NULL` is no meta data is
found.
