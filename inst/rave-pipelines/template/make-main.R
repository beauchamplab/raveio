library(targets)

source("common.R")

# load & combine pipelines
...targets <- raveio::load_targets(
  "make-initialize.R",
  "make-imports.R",
  "make-TEMPLATE.R"
)

# set cache procedures
...targets$load_initialize_settings$cue$mode <- "thorough"
...targets$load_settings_import$cue$mode <- "thorough"
...targets$load_epoch$cue$mode <- "thorough"
...targets$load_reference_table$cue$mode <- "thorough"
...targets$load_data$cue$mode <- "thorough"

...targets
