library(testthat)

test_that("rave-data-types", {

  subject <- raveio::as_rave_subject("demo/DemoSubject", strict = FALSE)
  skip_if_not(dir.exists(subject$path))

  # check reference electrodes
  e <- new_electrode(subject = "demo/DemoSubject", number = 14)
  e$set_epoch("auditory_onset")
  e$trial_intervals <- c(-1, 2)

  # test rave data type
  inst <- e$.load_noref_wavelet()
  expect_identical(inst$get_header("rave_data_type"), "wavelet-coefficient")
  inst$.mode <- "readwrite"
  # test if cached
  rand_str <- rand_string(10)
  inst$set_header("random_string", rand_str)
  inst <- e$.load_noref_wavelet()
  expect_identical(inst$get_header("random_string"), rand_str)

  # test rave data type
  inst <- e$.load_noref_voltage()
  expect_identical(inst$get_header("rave_data_type"), "voltage")
  inst$.mode <- "readwrite"
  # test if cached
  rand_str <- rand_string(10)
  inst$set_header("random_string", rand_str)
  inst <- e$.load_noref_voltage()
  expect_identical(inst$get_header("random_string"), rand_str)


  # test rave data type
  for(type in c("power", "phase", "wavelet-coefficient", "voltage", "raw-voltage")) {
    inst <- e$load_data(type = type)
    expect_identical(inst$get_header("rave_data_type"), type)
    inst$.mode <- "readwrite"
    # test if cached
    rand_str <- rand_string(10)
    inst$set_header("random_string", rand_str)
    inst <- e$load_data(type = type)
    expect_identical(inst$get_header("random_string"), rand_str)
  }







  # check reference electrodes
  e <- new_reference(subject = "demo/DemoSubject", number = 14)
  e$set_epoch("auditory_onset")
  e$trial_intervals <- c(-1, 2)

  # test rave data type
  inst <- e$.load_noref_wavelet()
  expect_identical(inst$get_header("rave_data_type"), "wavelet-coefficient")
  inst$.mode <- "readwrite"
  # test if cached
  rand_str <- rand_string(10)
  inst$set_header("random_string", rand_str)
  inst <- e$.load_noref_wavelet()
  expect_identical(inst$get_header("random_string"), rand_str)

  # test rave data type
  inst <- e$.load_noref_voltage()
  expect_identical(inst$get_header("rave_data_type"), "voltage")
  inst$.mode <- "readwrite"
  # test if cached
  rand_str <- rand_string(10)
  inst$set_header("random_string", rand_str)
  inst <- e$.load_noref_voltage()
  expect_identical(inst$get_header("random_string"), rand_str)


  # test rave data type
  for(type in c("power", "phase", "wavelet-coefficient", "voltage")) {
    inst <- e$load_data(type = type)
    expect_identical(inst$get_header("rave_data_type"), type)
    inst$.mode <- "readwrite"
    # test if cached
    rand_str <- rand_string(10)
    inst$set_header("random_string", rand_str)
    inst <- e$load_data(type = type)
    expect_identical(inst$get_header("random_string"), rand_str)
  }

})
