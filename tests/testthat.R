# Let warnings fail loud
library(testthat)
library(raveio)


if( is_on_cran(verbose = TRUE) ) {
  message("Detected CRAN, using max 2 cores and suppress tests/examples that are time-consuming")
} else {
  message("Testing locally, run extra tests with multiple cores")
}

print(Sys.getenv())


test_check("raveio")

