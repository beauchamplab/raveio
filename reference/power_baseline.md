# Calculate power baseline

Calculate power baseline

## Usage

``` r
power_baseline(
  x,
  baseline_windows,
  method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
  units = c("Trial", "Frequency", "Electrode"),
  ...
)

# S3 method for class 'rave_prepare_power'
power_baseline(
  x,
  baseline_windows,
  method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
  units = c("Frequency", "Trial", "Electrode"),
  electrodes,
  ...
)

# S3 method for class 'FileArray'
power_baseline(
  x,
  baseline_windows,
  method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
  units = c("Frequency", "Trial", "Electrode"),
  filebase = NULL,
  ...
)

# S3 method for class 'array'
power_baseline(
  x,
  baseline_windows,
  method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
  units = c("Trial", "Frequency", "Electrode"),
  ...
)

# S3 method for class 'ECoGTensor'
power_baseline(
  x,
  baseline_windows,
  method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
  units = c("Trial", "Frequency", "Electrode"),
  filebase = NULL,
  hybrid = TRUE,
  ...
)
```

## Arguments

- x:

  R array,
  [`filearray`](https://dipterix.org/filearray/reference/filearray.html),
  [`ECoGTensor`](ECoGTensor.md), or `'rave_prepare_power'` object
  created by [`prepare_subject_power`](rave-prepare.md).

- baseline_windows:

  list of baseline window (intervals)

- method:

  baseline method; choices are `'percentage'`, `'sqrt_percentage'`,
  `'decibel'`, `'zscore'`, `'sqrt_zscore'`; see 'Details' in
  [`baseline_array`](https://dipterix.org/dipsaus/reference/baseline_array.html)

- units:

  the unit of the baseline; see 'Details'

- ...:

  passed to other methods

- electrodes:

  the electrodes to be included in baseline calculation; for power
  repository object produced by
  [`prepare_subject_power`](rave-prepare.md) only; default is all
  available electrodes in each of `signal_types`

- filebase:

  where to store the output; default is `NULL` and is automatically
  determined

- hybrid:

  whether the array will be

## Value

Usually the same type as the input: for arrays,
[`filearray`](https://dipterix.org/filearray/reference/filearray.html),
or [`ECoGTensor`](ECoGTensor.md), the outputs are also the same type
with the same dimensions; for `'rave_prepare_power'` repositories, the
results will be stored in its `'baselined'` element; see 'Examples'.

## Details

The arrays must be four-mode tensor and must have valid named
[`dimnames`](https://rdrr.io/r/base/dimnames.html). The dimension names
must be `'Trial'`, `'Frequency'`, `'Time'`, `'Electrode'`, case
sensitive.

The `baseline_windows` determines the baseline windows that are used to
calculate time-points of baseline to be included. This can be one or
more intervals and must pass the validation function
[`validate_time_window`](validate_time_window.md).

The `units` determines the unit of the baseline. It can be one or more
of `'Trial'`, `'Frequency'`, `'Electrode'`. The default value is all of
them, i.e., baseline for each combination of trial, frequency, and
electrode. To share the baseline across trials, please remove `'Trial'`
from `units`. To calculate baseline that should be shared across
electrodes (e.g. in some mini-electrodes), remove `'Electrode'` from the
`units`.

## Examples

``` r
if (FALSE) { # \dontrun{
# The following code need to download additional demo data
# Please see https://rave.wiki/ for more details

library(raveio)
repo <- prepare_subject_power(
  subject = "demo/DemoSubject",
  time_windows = c(-1, 3),
  electrodes = c(14, 15))

##### Direct baseline on the repository
power_baseline(x = repo, method = "decibel",
               baseline_windows = list(c(-1, 0), c(2, 3)))
power_mean <- repo$power$baselined$collapse(
  keep = c(2,1), method = "mean")
image(power_mean, x = repo$time_points, y = repo$frequency,
      xlab = "Time (s)", ylab = "Frequency (Hz)",
      main = "Mean power over trial (Baseline: -1~0 & 2~3)")
abline(v = 0, lty = 2, col = 'blue')
text(x = 0, y = 20, "Aud-Onset", col = "blue", cex = 0.6)

##### Alternatively, baseline on electrode instances
baselined <- lapply(repo$power$data_list, function(inst) {
  re <- power_baseline(inst, method = "decibel",
                       baseline_windows = list(c(-1, 0), c(2, 3)))
  collapse2(re, keep = c(2,1), method = "mean")
})
power_mean2 <- (baselined[[1]] + baselined[[2]]) / 2

# Same with precision difference
max(abs(power_mean2 - power_mean)) < 1e-6


} # }
```
