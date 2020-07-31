test_that("RAVE preprocess pipeline 2.0", {
  # Skip on cran because data is local
  skip_on_cran()

  data_dir <- tempfile()
  dir_create2(data_dir)

  on.exit({
    unlink(data_dir, recursive = TRUE)
    load_setting(reset_temp = TRUE)
  })

  raveio::raveio_setopt('data_dir', data_dir, .save = FALSE)
  raveio::raveio_setopt('file_structure', 'native', .save = FALSE)
  raveio::raveio_setopt('max_worker', 1L, .save = FALSE)

  # Skip if cannot find subject
  self <- raveio:::RAVEPreprocessSettings$new(subject = 'demo/YAB', read_only = FALSE)
  skip_if_not(self$has_raw(), message = 'Raw subject missing for test, skipping')

  self$path
  dipsaus::capture_expr({
    # create subject import
    self$subject$initialize_paths()
    self$save()

    # Now import electrodes
    # pretending electrode 1-12 are EEG, 13-44 are ECoG, 45-60 are LFP

    # 1. Initial settings
    self$set_blocks(blocks = '008')
    self$set_electrodes(electrodes = 45:60, type = 'LFP', add = TRUE)
    self$set_electrodes(electrodes = 13:44, type = 'ECoG', add = TRUE)
    self$set_electrodes(electrodes = 1:12, type = 'EEG', add = TRUE)

    self$set_sample_rates(200, type = 'EEG')
    self$set_sample_rates(2000, type = 'LFP')

    expect_equal(self$electrodes, 1:60)
    expect_equal(self$electrode_types, rep(c('EEG', 'ECoG', 'LFP'), c(12, 32, 16)))
    expect_equal(self$sample_rates, rep(c(200, 2000), c(12, 48)))

    self$save()
    # 2. import EEG
    raveio::rave_import(
      project_name = self$subject$project_name,
      subject_code = self$subject$subject_code,
      blocks = self$blocks,
      sample_rate = 200,
      electrodes = 1:12,
      format = 1,
      add = TRUE, data_type = 'EEG'
    )

    expect_false(self$`@freeze_blocks`)
    expect_false(self$`@freeze_lfp_ecog`)

    for(e in 1:12){
      self$data[[e]]$data_imported <- TRUE
    }

    expect_equal(sum(self$data_imported), 12)

    expect_true(self$`@freeze_blocks`)
    expect_false(self$`@freeze_lfp_ecog`)

    raveio::rave_import(
      project_name = self$subject$project_name,
      subject_code = self$subject$subject_code,
      blocks = self$blocks,
      sample_rate = 2000,
      electrodes = 13:44,
      format = 1, add = TRUE,
      data_type = 'ECoG'
    )

    expect_false(self$data_locked)

    for(e in 13:44){
      self$data[[e]]$data_imported <- TRUE
    }

    expect_equal(sum(self$data_imported), 44)

    expect_true(self$`@freeze_blocks`)
    expect_true(self$`@freeze_lfp_ecog`)


    raveio::rave_import(
      project_name = self$subject$project_name,
      subject_code = self$subject$subject_code,
      blocks = self$blocks,
      sample_rate = 2000,
      electrodes = 45:60,
      format = 1, add = TRUE,
      data_type = 'LFP'
    )

    for(e in 45:60){
      self$data[[e]]$data_imported <- TRUE
    }

    expect_equal(sum(self$data_imported), 60)

    expect_true(all(self$data_imported))

    self$electrode_types
    self$sample_rates
    self$save()

    expect_true(self$data_locked)

    expect_equal(self$sample_rates, rep(c(200,2000), c(12, 48)))
    expect_equal(self$electrode_types, rep(c('EEG', 'ECoG', 'LFP'), c(12, 32, 16)))

    expect_true(self$`@freeze_blocks`)

    expect_true(self$`@freeze_lfp_ecog`)

    expect_equal(self$`@lfp_ecog_sample_rate`, 2000)


  })

})
