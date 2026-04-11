# Enhanced progress with logger message

For best performance, please install `'ravedash'`. This function can
replace
[`progress2`](https://dipterix.org/dipsaus/reference/progress2.html).

## Usage

``` r
progress_with_logger(
  title,
  max = 1,
  ...,
  quiet = FALSE,
  session = shiny::getDefaultReactiveDomain(),
  shiny_auto_close = FALSE,
  outputId = NULL,
  log
)
```

## Arguments

- title, max, ..., quiet, session, shiny_auto_close:

  see
  [`progress2`](https://dipterix.org/dipsaus/reference/progress2.html)

- outputId:

  will be used if package `'shidashi'` is installed, otherwise will be
  ignored

- log:

  function, `NULL`, or missing; default is missing, which will use
  `logger` function in the package `'ravedash'`, or
  [`cat2`](https://dipterix.org/dipsaus/reference/cat2.html) if
  `'ravedash'` is not installed. If `log=NULL`, then the message will be
  suppressed in 'shiny' applications. If a function provided, then the
  function will be called.

## Value

A list, see
[`progress2`](https://dipterix.org/dipsaus/reference/progress2.html)
