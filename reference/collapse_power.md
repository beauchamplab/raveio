# Collapse power array with given analysis cubes

Collapse power array with given analysis cubes

## Usage

``` r
collapse_power(x, analysis_index_cubes)

# S3 method for class 'array'
collapse_power(x, analysis_index_cubes)

# S3 method for class 'FileArray'
collapse_power(x, analysis_index_cubes)
```

## Arguments

- x:

  a
  [`FileArray-class`](https://dipterix.org/filearray/reference/FileArray-class.html)
  array, must have 4 modes in the following sequence `Frequency`,
  `Time`, `Trial`, and `Electrode`

- analysis_index_cubes:

  a list of analysis indices for each mode

## Value

a list of collapsed (mean) results

- `freq_trial_elec`:

  collapsed over time-points

- `freq_time_elec`:

  collapsed over trials

- `time_trial_elec`:

  collapsed over frequencies

- `freq_time`:

  collapsed over trials and electrodes

- `freq_elec`:

  collapsed over trials and time-points

- `freq_trial`:

  collapsed over time-points and electrodes

- `time_trial`:

  collapsed over frequencies and electrodes

- `time_elec`:

  collapsed over frequencies and trials

- `trial_elec`:

  collapsed over frequencies and time-points

- `freq`:

  power per frequency, averaged over other modes

- `time`:

  power per time-point, averaged over other modes

- `trial`:

  power per trial, averaged over other modes

## Examples

``` r
if(!is_on_cran()) {

# Generate a 4-mode tensor array
x <- filearray::filearray_create(
  tempfile(), dimension = c(16, 100, 20, 5),
  partition_size = 1
)
x[] <- rnorm(160000)
dnames <- list(
  Frequency = 1:16,
  Time = seq(0, 1, length.out = 100),
  Trial = 1:20,
  Electrode = 1:5
)
dimnames(x) <- dnames

# Collapse array
results <- collapse_power(x, list(
  overall = list(),
  A = list(Trial = 1:5, Frequency = 1:6),
  B = list(Trial = 6:10, Time = 1:50)
))

# Plot power over frequency and time
groupB_result <- results$B


image(t(groupB_result$freq_time),
      x = dnames$Time[groupB_result$cube_index$Time],
      y = dnames$Frequency[groupB_result$cube_index$Frequency],
      xlab = "Time (s)",
      ylab = "Frequency (Hz)",
      xlim = range(dnames$Time))

x$delete(force = TRUE)


}
#> NOT_CRAN is TRUE/true (not on CRAN)

```
