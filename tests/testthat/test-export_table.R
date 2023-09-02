test_that("save and load tables", {
  fmts <- c("csv", "h5", "fst", "json", "rds", "yaml")
  x <- data.table::data.table(
    a = rnorm(10),
    b = letters[1:10],
    c = 1:10,
    d = factor(LETTERS[1:10])
  )


  nms <- names(x)
  for(fmt in fmts) {
    file <- tempfile(fileext = sprintf(".%s", fmt))
    export_table(x = x, file = file)
    y <- import_table(file)
    testthat::expect_true(is.data.frame(y))
    testthat::expect_equal(names(y), nms)
    if(fmt %in% "csv") {
      nms1 <- nms[1:3]
    } else {
      nms1 <- nms
    }
    for(nm in nms1) {
      testthat::expect_equal(y[[nm]], x[[nm]], tolerance = 1e-4, label = sprintf("import_table[fmt:%s,nm:%s]", fmt, nm))
    }

  }
})
