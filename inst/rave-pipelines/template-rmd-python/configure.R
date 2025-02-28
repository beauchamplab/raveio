source("common.R")

unlink("_targets.yaml")

targets::tar_config_set(
  project = target_name,
  script = "make-main.R",
  store = target_directory
)


