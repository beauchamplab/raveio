# Check if current session is on 'CRAN'

Use this function only for examples and test. The goal is to comply with
the 'CRAN' policy. Do not use it in normal functions to cheat. Violating
'CRAN' policy will introduce instability to your code. Make sure reading
Section 'Details' before using this function.

## Usage

``` r
is_on_cran(if_interactive = FALSE, verbose = FALSE)
```

## Arguments

- if_interactive:

  whether interactive session will be considered as on 'CRAN'; default
  is `FALSE`

- verbose:

  whether to print out reason of return; default is no

## Value

A logical whether current environment should be considered as on 'CRAN'.

## Details

According to 'CRAN' policy, package examples and test functions may only
use maximum 2 'CPU' cores. Examples running too long should be
suppressed. Normally package developers will use
[`interactive()`](https://rdrr.io/r/base/interactive.html) to avoid
running examples or parallel code on 'CRAN'. However, when checked
locally, these examples will be skipped too. Coding bug in those
examples will not be reported.

The objective is to allow 'RAVE' package developers to write and test
examples locally or on integrated development environment (such as
'Github'), while suppressing them on 'CRAN'. In such way, bugs in the
examples will be revealed and fixed promptly.

Do not use this function inside of the package functions to cheat or
slip illegal code under the eyes of 'CRAN' folks. This will increase
their work load and introduce instability to your code. Also this
function is subject to deletion in the future.
