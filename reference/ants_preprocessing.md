# Process 'T1' weighted 'MRI' using `'ANTs'`

This function is soft-deprecated. Use
[`yael_preprocess`](yael_preprocess.md) instead.

## Usage

``` r
ants_preprocessing(
  work_path,
  image_path,
  resample = FALSE,
  verbose = TRUE,
  template_subject = raveio_getopt("threeBrain_template_subject")
)
```

## Arguments

- work_path:

  working directory, all intermediate images will be stored here

- image_path:

  input image path

- resample:

  whether to resample the input image before processing

- verbose:

  whether to verbose the processing details

- template_subject:

  template mapping, default is derived from
  [`raveio_getopt`](http://dipterix.org/ravepipeline/reference/raveio-option.md)

## Value

Nothing. All images are saved to `work_path`
