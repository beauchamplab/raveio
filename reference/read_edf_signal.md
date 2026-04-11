# Read 'EDF(+)' or 'BDF(+)' file signals

Read 'EDF(+)' or 'BDF(+)' file signals

## Usage

``` r
read_edf_signal(
  path,
  signal_numbers = NULL,
  convert_volt = c("NA", "V", "mV", "uV")
)
```

## Arguments

- path:

  file path, passed to `readEdfHeader`

- signal_numbers:

  channel/electrode numbers

- convert_volt:

  convert voltage (electric potential) to a new unit, `NA` means no
  conversion, other choices are `'V'`, `'mV'`, and `'uV'`.

## Value

A list containing header information, signal lists, and
channel/electrode names. If `signal_numbers` is specified, the
corresponding names should appear as `selected_signal_names`.
`get_signal()` can get physical signals after unit conversion.
