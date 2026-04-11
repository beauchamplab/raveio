# Compose a "phantom" channel from existing electrodes

In some cases, for example, deep-brain stimulation ('DBS'), it is often
needed to analyze averaged electrode channels from segmented 'DBS'
leads, or create bipolar contrast between electrode channels, or to
generate non-equally weighted channel averages for 'Laplacian'
reference. `compose_channel` allows users to generate a phantom channel
that does not physically exist, but is treated as a normal electrode
channel in 'RAVE'.

## Usage

``` r
compose_channel(
  subject,
  number,
  from,
  weights = rep(1/length(from), length(from)),
  normalize = FALSE,
  force = FALSE,
  label = sprintf("Composed-%s", number),
  signal_type = c("auto", "LFP", "Spike", "EKG", "Auxiliary", "Unknown")
)
```

## Arguments

- subject:

  'RAVE' subject

- number:

  new channel number, must be positive integer, cannot be existing
  electrode channel numbers

- from:

  a vector of electrode channels that is used to compose this new
  channel, must be non-empty; see `weights` if these channels are not
  equally weighted.

- weights:

  numerical weights used on each `from` channels; the length of
  `weights` must equals to the length of `from`; default is equally
  weighted for each channel (mean of `from` channels).

- normalize:

  whether to normalize the weights such that the composed channel has
  the same variance as `from` channels; default is false

- force:

  whether to overwrite existing composed channel if it exists; default
  is false. By specifying `force=TRUE`, users are risking breaking the
  data integrity since any analysis based on the composed channel is no
  longer reproducible. Also users cannot overwrite original channels
  under any circumstances.

- label:

  the label for the composed channel; will be stored at
  `'electrodes.csv'`

- signal_type:

  signal type of the composed channel; default is `'auto'` (same as the
  first `from` channel); other choices see
  [`SIGNAL_TYPES`](raveio-constants.md)

## Value

Nothing

## Examples

``` r
library(raveio)


# Make sure demo subject exists in this example, just want to make
# sure the example does not error out
if(
  interactive() && "demo" %in% get_projects() &&
  "DemoSubject" %in% as_rave_project('demo')$subjects() &&
  local({
    subject <- as_rave_subject("demo/DemoSubject")
    !100 %in% subject$electrodes
  })
) {

  # the actual example code:
  # new channel 100 = 2 x channel 14 - (channe 15 + 16)
  compose_channel(
    subject = "demo/DemoSubject",
    number = 100,
    from = c(14, 15, 16),
    weights = c(2, -1, -1),
    normalize = FALSE
  )

}



```
