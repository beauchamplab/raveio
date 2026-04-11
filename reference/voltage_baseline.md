# Calculate voltage baseline

Calculate voltage baseline

## Usage

``` r
voltage_baseline(
  x,
  baseline_windows,
  method = c("percentage", "zscore", "subtract_mean"),
  units = c("Trial", "Electrode"),
  ...
)

# S3 method for class 'rave_prepare_subject_raw_voltage_with_epoch'
voltage_baseline(
  x,
  baseline_windows,
  method = c("percentage", "zscore", "subtract_mean"),
  units = c("Trial", "Electrode"),
  electrodes,
  baseline_mean,
  baseline_sd,
  ...
)

# S3 method for class 'rave_prepare_subject_voltage_with_epoch'
voltage_baseline(
  x,
  baseline_windows,
  method = c("percentage", "zscore", "subtract_mean"),
  units = c("Trial", "Electrode"),
  electrodes,
  baseline_mean,
  baseline_sd,
  ...
)

# S3 method for class 'FileArray'
voltage_baseline(
  x,
  baseline_windows,
  method = c("percentage", "zscore", "subtract_mean"),
  units = c("Trial", "Electrode"),
  filebase = NULL,
  ...
)

# S3 method for class 'array'
voltage_baseline(
  x,
  baseline_windows,
  method = c("percentage", "zscore", "subtract_mean"),
  units = c("Trial", "Electrode"),
  ...
)
```

## Arguments

- x:

  R array,
  [`filearray`](https://dipterix.org/filearray/reference/filearray.html),
  or `'rave_prepare_power'` object created by
  [`prepare_subject_raw_voltage_with_epoch`](rave-prepare.md).

- baseline_windows:

  list of baseline window (intervals)

- method:

  baseline method; choices are `'percentage'` and `'zscore'`; see
  'Details' in
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

- baseline_mean, baseline_sd:

  internally used by 'RAVE' repository, provided baseline is not
  contained in the data. This is useful for calculating the baseline
  with data from other blocks.

- filebase:

  where to store the output; default is `NULL` and is automatically
  determined

## Value

The same type as the inputs

## Details

The arrays must be three-mode tensor and must have valid named
[`dimnames`](https://rdrr.io/r/base/dimnames.html). The dimension names
must be `'Trial'`, `'Time'`, `'Electrode'`, case sensitive.

The `baseline_windows` determines the baseline windows that are used to
calculate time-points of baseline to be included. This can be one or
more intervals and must pass the validation function
[`validate_time_window`](validate_time_window.md).

The `units` determines the unit of the baseline. It can be either or
both of `'Trial'`, `'Electrode'`. The default value is both, i.e.,
baseline for each combination of trial and electrode.

## Examples

``` r
if (FALSE) { # \dontrun{
# The following code need to download additional demo data
# Please see https://rave.wiki/ for more details

library(raveio)
repo <- prepare_subject_raw_voltage_with_epoch(
  subject = "demo/DemoSubject",
  time_windows = c(-1, 3),
  electrodes = c(14, 15))

##### Direct baseline on repository
voltage_baseline(
  x = repo, method = "zscore",
  baseline_windows = list(c(-1, 0), c(2, 3))
)

voltage_mean <- repo$raw_voltage$baselined$collapse(
  keep = c(1,3), method = "mean")
matplot(voltage_mean, type = "l", lty = 1,
        x = repo$raw_voltage$dimnames$Time,
        xlab = "Time (s)", ylab = "Voltage (z-scored)",
        main = "Mean coltage over trial (Baseline: -1~0 & 2~3)")
abline(v = 0, lty = 2, col = 'darkgreen')
text(x = 0, y = -0.5, "Aud-Onset ", col = "darkgreen", cex = 0.6, adj = c(1,1))

##### Alternatively, baseline on each electrode channel
voltage_mean2 <- sapply(repo$raw_voltage$data_list, function(inst) {
  re <- voltage_baseline(
    x = inst, method = "zscore",
    baseline_windows = list(c(-1, 0), c(2, 3)))
  rowMeans(re[])
})

# Same with floating difference
max(abs(voltage_mean - voltage_mean2)) < 1e-8


} # }
```
