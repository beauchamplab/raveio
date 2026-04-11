# Let warnings fail loud
library(testthat)
library(raveio)

print(Sys.getenv())


test_check("raveio")

