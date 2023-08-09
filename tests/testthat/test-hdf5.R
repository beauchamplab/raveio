require(testthat)
test_that("HDF5 IO", {

  x <- array(1:24, c(1,2,3,1,4,1))

  f <- tempfile()
  on.exit({ unlink(f) })

  save_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")

  y <- load_h5(file = f, name = "data")
  expect_equal(
    dim(y),
    dim(x)
  )

  expect_equal(
    dim(load_h5(file = f, name = "data", ram = TRUE)),
    dim(x)
  )
  expect_equal(
    dim(y[]),
    dim(x)
  )
  expect_equal(
    dim(y[drop = TRUE]),
    dim(drop(x))
  )

  env <- new.env()
  env$idx <- c(FALSE, TRUE, TRUE)
  expect_equal(
    with(env, {
      y[1,,idx,,,]
    }),
    x[1,,c(2,3),,,,drop=FALSE]
  )
  expect_equal(
    y[1,,1,,4,,drop=TRUE],
    x[1,,1,,4,,drop=TRUE]
  )

  x <- 1:24
  save_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- load_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(1,24))
  save_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- load_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- array(1:24, c(24,1))
  save_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- load_h5(file = f, name = "data")

  expect_equal(x, y[])

  x <- numeric(0)
  save_h5(x, file = f, name = "data", quiet = TRUE, ctype = "numeric")
  y <- load_h5(file = f, name = "data")

  expect_equal(x, y[])

})
