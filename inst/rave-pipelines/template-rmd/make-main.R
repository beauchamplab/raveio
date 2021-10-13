library(targets)

...targets <- local({
  source("common.R", local = TRUE)

  # load & combine pipelines
  targets <- raveio::load_targets(
    "make-TEMPLATE.R"
  )

  # reorder for debug use, or simply return `targets`
  targets

})

...targets
