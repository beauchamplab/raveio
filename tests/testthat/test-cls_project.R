# test project data

library(testthat)

test_that("Project class & derivatives", {
  skip_on_cran()

  on.exit({
    load_setting()
  })

  raveio_setopt('file_structure', 'native', .save = FALSE)
  raveio_setopt('data_dir', '~/rave_data/data_dir', .save = FALSE)

  skip_if_not("demo" %in% get_projects())

  project <- as_rave_project('demo')

  expect_true(project$has_subject('YAB'))
  expect_false(project$has_subject('blahblahblah'))

  expect_equal(project$name, 'demo')
  expect_equal(project$path, normalizePath('~/rave_data/data_dir/demo'))

  expect_equal(
    project$group_path('power_explorer'),
    normalizePath('~/rave_data/data_dir/demo/_project_data/power_explorer')
  )

  expect_true('demo' %in% get_projects())

  # use BIDS
  # raveio_setopt('file_structure', 'BIDS', .save = FALSE)
  # raveio_setopt('data_dir', '~/rave_data/bids_dir', .save = FALSE)
  #
  # project <- as_rave_project('ieeg_visual_multimodal')
  #
  # expect_true(project$has_subject('som682'))
  #
  # expect_equal(project$name, 'ieeg_visual_multimodal')
  # expect_equal(project$path, normalizePath('~/rave_data/bids_dir/ieeg_visual_multimodal/derivatives/rave/ieeg_visual_multimodal/', mustWork = FALSE))
  #
  # expect_equal(
  #   project$group_path('power_explorer'),
  #   normalizePath('~/rave_data/bids_dir/ieeg_visual_multimodal/derivatives/rave/ieeg_visual_multimodal/_project_data/power_explorer', mustWork = FALSE)
  # )
  #
  # expect_true('ieeg_visual_multimodal' %in% get_projects())

})
