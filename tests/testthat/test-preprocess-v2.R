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

  skip_if_not("demo" %in% get_projects())

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
    self$set_electrodes(electrodes = 45:60, type = 'EKG', add = TRUE)
    self$set_electrodes(electrodes = 13:44, type = 'LFP', add = TRUE)
    self$set_electrodes(electrodes = 1:12, type = 'Unknown', add = TRUE)

    self$set_sample_rates(200, type = 'EKG')
    self$set_sample_rates(2000, type = 'LFP')

    expect_equal(self$electrodes, 1:60)
    expect_equal(self$electrode_types, rep(c('Unknown', 'LFP', 'EKG'), c(12, 32, 16)))
    expect_equal(self$sample_rates, rep(c(NA, 2000, 200), c(12, 32, 16)))

    self$save()
    # 2. import EKG
    raveio::rave_import(
      project_name = self$subject$project_name,
      subject_code = self$subject$subject_code,
      blocks = self$blocks,
      sample_rate = 2000,
      electrodes = 13:44,
      format = 1,
      add = TRUE,
      data_type = 'LFP'
    )

    expect_false(self$`@freeze_blocks`)
    expect_false(self$`@freeze_lfp_ecog`)

    for(e in 1:44){
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
      data_type = 'LFP' # Should be EKG, but not implemented yet
    )

    expect_false(self$data_locked)

    for(e in 45:60){
      self$data[[e]]$data_imported <- TRUE
    }

    expect_equal(sum(self$data_imported), 60)

    expect_true(self$`@freeze_blocks`)
    expect_true(self$`@freeze_lfp_ecog`)


    expect_true(all(self$data_imported))

    self$electrode_types
    self$sample_rates
    self$save()

    expect_true(self$data_locked)

    expect_equal(self$sample_rates, rep(c(NA, 2000, 200), c(12, 32, 16)))
    expect_equal(self$electrode_types, rep(c('Unknown', 'LFP', 'EKG'), c(12, 32, 16)))

    expect_true(self$`@freeze_blocks`)

    expect_true(self$`@freeze_lfp_ecog`)

    expect_equal(self$`@lfp_ecog_sample_rate`, 2000)


  })

})


test_that("RAVE 2.0 LFP electrode classes", {

  skip_if_not(
    dipsaus::package_installed("rave") &&
      "demo" %in% get_projects()
  )

  suppressWarnings({
    dipsaus::capture_expr({
    dipsaus::capture_expr({
      epoch_name <- "auditory_onset"
      reference_name <- "default"
      subject_id <- "demo/DemoSubject"
      elec <- 14
      rave <- asNamespace('rave')
      env <- rave$rave_prepare(subject = subject_id, electrodes = elec, time_range = c(1,2), data_types = c("power", "voltage"), epoch = epoch_name, reference = reference_name, attach = FALSE)

      power <- env$module_tools$get_power(referenced = TRUE)
      volt <- env$module_tools$get_voltage(referenced = TRUE)
      expected_power <- aperm(power$get_data(), c(2, 3, 1, 4))
      expected_volt <- aperm(volt$get_data(), c(2, 1, 3))
      e <- new_electrode(subject = subject_id, number = elec)
      ref_table <- e$subject$get_reference(reference_name = reference_name)
      ref_name <- ref_table$Reference[ref_table$Electrode == elec]
      ref <- new_reference(subject = subject_id, number = ref_name, signal_type = "LFP")

      e$set_reference(ref)
      e$set_epoch(e$subject$get_epoch(epoch_name))
      e$trial_intervals <- c(-1, 2)

      newpower <- e$load_data("power")
      expect_true({
        max(abs(range(newpower[drop = FALSE] / expected_power - 1))) < 1e-4
      })

      newvolt <- e$load_data("voltage")
      expect_true({
        max(abs(range(newvolt[drop = FALSE] / expected_volt - 1))) < 1e-7
      })
    }, type = "message")
    })
  })


})
