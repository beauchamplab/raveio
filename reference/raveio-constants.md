# The constant variables

The constant variables

## Usage

``` r
SIGNAL_TYPES

LOCATION_TYPES

MNI305_to_MNI152

YAEL_IMAGE_TYPES
```

## Format

An object of class `character` of length 5.

An object of class `character` of length 5.

An object of class `matrix` (inherits from `array`) with 4 rows and 4
columns.

An object of class `character` of length 10.

## Details

`SIGNAL_TYPES` has the following options: `'LFP'`, `'Spike'`, `'EKG'`,
`'Auxiliary'`, or `'Unknown'`. As of 'raveio' `0.0.6`, only `'LFP'` (see
[`LFP_electrode`](LFP_electrode.md)) signal type is supported.

`LOCATION_TYPES` is a list of the electrode location types: `'iEEG'`
(this includes the next two), `'sEEG'` (stereo), `'ECoG'` (surface),
`'EEG'` (scalp), `'Others'`. See field `'location'` in
[`RAVEAbstarctElectrode`](RAVEAbstarctElectrode.md)

`MNI305_to_MNI152` is a 4-by-4 matrix converting `'MNI305'` coordinates
to `'MNI152'` space. The difference of these two spaces is: `'MNI305'`
is an average of 305 human subjects, while `'MNI152'` is the average of
152 people. These two coordinates differs slightly. While most of the
'MNI' coordinates reported by 'RAVE' and 'FreeSurfer' are in the
`'MNI305'` space, many other programs are expecting `'MNI152'`
coordinates.
