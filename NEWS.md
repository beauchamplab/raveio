raveio 0.0.7
=======

Major changes:

* Added `PipelineTools` class and wrapped constructor `raveio::pipeline` to load common utility tools needed to run the pipeline
* Added module templates for `RAVE-2.0`
* Allow `import_electrode_table` to import table without replacing existing electrode files with `dry_run` option

raveio 0.0.6
=======

Major changes:

* Added `read_mat2` to allow reading `Matlab` files stored in `HDF5` formats
* Allow subject instances to store and retrieve default key-value pairs via `set_default` and `get_default` methods
* Disable `multicore` by default, so users can use their own `plan` provided by the `future` package
* Added location and signal types to electrodes
* Added function to perform baseline via `power_baseline` directly from repository
* Added multiple repository types for `RAVE` subjects with identical key as signature
* Added `with_future_parallel` to enable `multicore` features in the expressions
* Added reference signal class
* Use `ravedash` log system provided if the package is installed
* Speed-up `raveio_getopt` and `raveio_setopt`
* Added `validate_time_window` to return intervals of time windows
* Added `save_json` and `load_json` to handle `JSON` format using `jsonlite` package
* Allow to set `threeBrain` template brain
* Added `import_electrode_table` to import electrode table with coordinates in `T1` or `MNI` space to `tkrRAS`
* Electrodes can load corresponding block data
* Added `normalize_commandline_path` and `cmd_*` functions to search for external system commands such as `FreeSurfer`, `FSL-FLIRT`, `dcm2niix`
* Added `backup_file` to back up existing files instead of overwriting
* Allow to download and install `rave-server` as services (currently only works on `OSX`)


Changes to pipeline framework: 

* Implemented and matured reproducible pipeline framework
* Added R-markdown template to build pipelines
* Allow pipelines to run in another process and can kill the process anytime
* Pipelines run in `async` mode works in `shiny` applications
* Created an `R6` class for pipeline results in `promises` way
* Allow to clear cache files at subject level
* `Async` pipelines can have callback functions at each check, useful for monitoring the progress

Enhancements:

* `save_yaml` can write to connections
* `rave_imports` runs in parallel in `native2` format
* Added an option to disable fork-clusters (enabled by default on `OSX` and `Linux`)
* Allows `EDF` files to be partially read
* Added data format entry to the preprocess instance, allowing following modules to be aware of the raw data format
* `ravetools` respects `tensor_temp_path` as its temporary path


raveio 0.0.5
=======

Major changes: 

* Rewind back to `hdf5r` as it passes the `CRAN` checks now
* Fixed `HDF5` bugs
* Added pipeline functions to self-expand `RAVE`
* Added pipeline templates

raveio 0.0.4
=======

Major changes: 

* removed `lazyarray`, `pryr`
* fixed `get_ram` errors when system command not found
* disabled support on `Solaris`
* changed `hdf5r` to `rhdf5`

raveio 0.0.3
=======

Initial `CRAN` submission.

