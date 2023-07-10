#' File whose name starts with `shared-` will be automatically
#' loaded by the pipeline. Hence you can put global static objects
#' here, for example,
#'   - define functions that may be used multiple times,
#'   - declare variables that can be re-used
#'   - import packages that will be used via `library()`, or `targets::tar_option_set` (see below)
NULL

# Set pipeline options
targets::tar_option_set(

  # R libraries to load
  packages = c("raveio")

  # memory strategy.
  # "persistent": the target stays in memory until the end of the pipeline
  #   unless storage is "worker", in which case targets unloads the value
  #   from memory right after storing it in order to avoid sending copious
  #   data over a network.
  # "transient": the target gets unloaded after every new target completes.
  # The former conserves bandwidth, and the latter conserves local storage.

  # memory = 'persistent'

  # When `scheduler` is "future" or "clustermq":
  # "main": the target's return value is sent back to the host machine and
  #   saved/uploaded locally.
  # "worker": the worker saves/uploads the value.
  #
  # Set to worker if your nodes might generate large amount of data

  # storage = 'worker'

  # When `scheduler` is "future" or "clustermq":
  # "main": the target's dependencies are loaded on the host machine and sent
  #   to the worker before the target builds.
  # "worker": the worker loads the targets dependencies.

  # retrieval = "worker"

)
