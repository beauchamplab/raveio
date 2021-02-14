require(testthat)

test_that("Subject class & derivatives", {
  skip_on_cran()

  on.exit({ load_setting() })

  raveio_setopt('data_dir', '~/rave_data/data_dir/', .save = FALSE)
  raveio_setopt('raw_data_dir', '~/rave_data/raw_dir/', .save = FALSE)
  raveio_setopt('file_structure', 'native', .save = FALSE)

  skip_if_not("demo" %in% get_projects())

  sub <- as_rave_subject('demo/YAB')


  expect_true(length(sub$blocks) && all(sub$blocks %in% c("008", "010", "011", "012")))

  expect_equal(
    sub$cache_path,
    normalizePath('~/rave_data/data_dir/demo/YAB/rave/data/cache')
  )

  expect_equal(
    sub$data_path,
    normalizePath('~/rave_data/data_dir/demo/YAB/rave/data/')
  )

  expect_equal(
    sub$subject_id,
    'demo/YAB'
  )

  load_setting()
  on.exit({})

})
