# Convert 'BlackRock' 'NEV/NSx' files

Convert 'BlackRock' 'NEV/NSx' files

## Usage

``` r
convert_blackrock(
  file,
  block = NULL,
  subject = NULL,
  to = NULL,
  epoch = c("comment", "digital_inputs", "recording", "configuration", "log",
    "button_trigger", "tracking", "video_sync"),
  format = c("mat", "hdf5"),
  header_only = FALSE,
  ...
)
```

## Arguments

- file:

  path to any 'NEV/NSx' file

- block:

  the block name, default is file name

- subject:

  subject code to save the files; default is `NULL`

- to:

  save to path, must be a directory; default is under the file path. If
  `subject` is provided, then the default is `subject` raw directory
  path

- epoch:

  what type of events should be included in epoch file; default include
  comment, digital inputs, recording trigger, configuration change, log
  comment, button trigger, tracking, and video trigger.

- format:

  output format, choices are `'mat'` or `'hdf5'`

- header_only:

  whether just to generate channel and epoch table; default is false

- ...:

  ignored for enhanced backward compatibility

## Value

The results will be stored in directory specified by `to`. Please read
the output message carefully.
