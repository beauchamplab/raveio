# Read 'EDF(+)' or 'BDF(+)' file headers

Wrapper of
[`readEdfHeader`](https://rdrr.io/pkg/edfReader/man/readEdfHeader.html),
but added some information

## Usage

``` r
read_edf_header(path)
```

## Arguments

- path:

  file path, passed to `readEdfHeader`

## Value

A list is header information of an 'EDF/BDF' file.

## Details

The added names are: `isAnnot2`, `sampleRate2`, and `unit2`. To avoid
conflict with other names, there is a "2" appended to each names.
`isAnnot2` indicates whether each channel is annotation channel or
recorded signals. `sampleRate2` is a vector of sample rates for each
channels. `unit2` is physical unit of recorded signals. For 'iEEG' data,
this is electric potential unit, and choices are `'V'` for volt, `'mV'`
for millivolt, and `'uV'` for micro-volt. For more details, see
<https://www.edfplus.info/specs/edftexts.html>

## See also

[`readEdfHeader`](https://rdrr.io/pkg/edfReader/man/readEdfHeader.html)
