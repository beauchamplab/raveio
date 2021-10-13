<img src="https://raw.githubusercontent.com/beauchamplab/raveio/master/inst/raveio-logo.png" height="116px" align="right" />

# File IO Package for 'RAVE' Project

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/raveio)](https://CRAN.R-project.org/package=raveio)
[![R-check](https://github.com/beauchamplab/raveio/workflows/R-CMD-check/badge.svg)](https://github.com/beauchamplab/raveio/actions)
[![DOI](https://raw.githubusercontent.com/dipterix/threeBrain/master/inst/doi.svg)](https://doi.org/10.1016/j.neuroimage.2020.117341)
<!-- badges: end -->

'RAVE' is a R project that aims at providing interactive analysis and visualization of intracranial Electroencephalography. Developed by `Beauchamp's Lab`, supported by NIH `1R24MH117529`

This package is part of `rave` family that provides readers from multiple file formats, including:
* `EDF(+)`, `BrainVision`, `BIDS-iEEG`, `Matlab` files commonly used by neuroscientists
* `HDF5`, common file format used by 'Matlab', 'Python', 'C++', and 'R'
* `FST`, a file format that supports GB-level read/write speed

Internally support 'RAVE' format; see [wiki page](https://openwetware.org/wiki/RAVE).

## Installation

`raveio` is part of `rave`. Please install `rave` directly from the [wiki page](https://openwetware.org/wiki/RAVE).

To install `raveio` alone, simply type R command:

```r
install.packages("raveio")
```

## Citation

Please cite the following paper for publication use:

* `Magnotti, JF, Wang, Z, Beauchamp, MS. RAVE: comprehensive open-source software for reproducible analysis and visualization of intracranial EEG data. NeuroImage, 223, p.117341.` ([DOI](https://doi.org/10.1016/j.neuroimage.2020.117341))

Please consider citing the following paper as well:

* `Metzger BA, Magnotti JF, Wang Z, Nesbitt E, Karas PJ, Yoshor D, Beauchamp MS. Responses to Visual Speech in Human Posterior Superior Temporal Gyrus Examined with iEEG Deconvolution. Journal of Neuroscience` ([DOI](https://doi.org/10.1523/JNEUROSCI.0279-20.2020))
* `Karas PJ, Magnotti JF, Metzger BA, Zhu LL, Smith KB, Yoshor D, Beauchamp MS. The visual speech head start improves perception and reduces superior temporal cortex responses to auditory speech. eLife 2019;8:e48116` ([DOI](https://doi.org/10.7554/eLife.48116))

You can type `print(citation('raveio'), bibtex = TRUE)` to print details.
