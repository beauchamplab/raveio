# Generate common average reference signal for 'RAVE' subjects

To properly run this function, please install `ravetools` package.

## Usage

``` r
generate_reference(subject, electrodes)
```

## Arguments

- subject:

  subject ID or [`RAVESubject`](RAVESubject.md) instance

- electrodes:

  electrodes to calculate the common average; these electrodes must run
  through 'Wavelet' first

## Value

A reference instance returned by [`new_reference`](new_electrode.md)
with signal type determined automatically.

## Details

The goal of generating common average signals is to capture the common
movement from all the channels and remove them out from electrode
signals.

The common average signals will be stored at subject reference
directories. Two exact same copies will be stored: one in 'HDF5' format
such that the data can be read universally by other programming
languages; one in
[`filearray`](https://dipterix.org/filearray/reference/filearray.html)
format that can be read in R with super fast speed.
