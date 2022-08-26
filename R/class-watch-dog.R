RAVEWatchDog <- R6::R6Class(
  classname = "RAVEWatchDog",
  portable = TRUE,
  private = list(
    pipeline_names = c(
      "import_lfp_native",
      "notch_filter",
      "wavelet_module",
      'reference_module'
    ),
    .raw_path = character(0),
    .job_name = character(0),
    .watch_path = character(0),
    .time_threshold = NULL,
    .project_name = character(0),
    .file_pattern = "^([a-zA-Z0-9]+)_datafile_([a-zA-Z0-9]+)\\.nev$",

    .registry_cache = NULL,
    .set_status = function(item) {
      stopifnot(
        is.data.frame(item) && nrow(item) == 1 &&
          setequal(names(item), c("Filename", "Subject", "Block",
                                  "Status", "Details", "LastModified",
                                  "TaskStarted", "TaskEnded", "Directory"))
      )
      registry <- self$load_registry(update = TRUE)
      sel <- registry$Filename == item$Filename
      if(length(sel) && any(sel)) {
        registry <- registry[!sel, ]
      }
      # Directly binding two items will result in error (with NA)
      item$LastModified <- as.POSIXlt(item$LastModified)
      item$TaskStarted <- as.POSIXlt(item$TaskStarted)
      item$TaskEnded <- as.POSIXlt(item$TaskEnded)
      registry <- rbind(item, registry)
      registry <- registry[order(registry$TaskStarted, na.last = TRUE, decreasing = TRUE), ]
      private$.registry_cache <- registry

      # save to csv
      registry$LastModified <- strftime(registry$LastModified)
      registry$TaskStarted <- strftime(registry$TaskStarted)
      registry$TaskEnded <- strftime(registry$TaskEnded)
      dir_create2(dirname(self$registry_path))
      safe_write_csv(registry, file = self$registry_path, row.names = FALSE, quiet = TRUE)
    },

    .get_status = function(file, update = FALSE) {
      stopifnot(length(file) == 1 && file.exists(file.path(private$.watch_path, file)))

      mtime <- file.mtime(file.path(private$.watch_path, file))

      # get subject code, block ID
      fname <- filenames(file)
      m <- gregexec(self$file_pattern, fname, ignore.case = TRUE, useBytes = TRUE)[[1]]
      ml <- attr(m, "match.length")

      error_item <- data.frame(
        Filename = file,
        Subject = NA,
        Block = NA,
        Status = "parse-error",
        Details = "Cannot parse subject code or block ID",
        LastModified = mtime,
        TaskStarted = NA,
        TaskEnded = NA,
        Directory = private$.watch_path
      )
      if(length(m) != 3) {
        return(error_item)
      }

      subject_code <- substr(fname, m[[2]], m[[2]] + ml[[2]] - 1)
      block <- substr(fname, m[[3]], m[[3]] + ml[[3]] - 1)
      if(!nchar(subject_code) || !nchar(block)) {
        return(error_item)
      }

      if(startsWith(block, "0")) {
        block <- sprintf("block%s", block)
      }

      registry <- self$load_registry(update = update)
      if(file %in% registry$Filename) {
        sel <- which(registry$Filename == file)
        item <- registry[sel[[1]], ]
        item$LastModified <- mtime
      } else {
        item <- data.frame(
          Filename = file,
          Subject = subject_code,
          Block = block,
          Status = "ready",
          Details = "Just found the subject",
          LastModified = mtime,
          TaskStarted = NA,
          TaskEnded = NA,
          Directory = private$.watch_path
        )
      }
      item

    },

    .queue = NULL,
    .jobs = NULL,
    .max_jobs = numeric(0L)
  ),
  public = list(

    cache_path = character(0),

    NSType_LFP = "ns3",
    NSType_stimuli = "ns5",

    initialize = function(watch_path, job_name = "RAVEWatchDog"){
      if(!grepl("^[a-zA-Z0-9-]+$", job_name)) {
        stop("Watch dog name can only contain letters, digits, and dash [-].")
      }
      private$.job_name <- job_name
      private$.watch_path <- normalizePath(watch_path, mustWork = FALSE)

      # Default values
      private$.max_jobs <- 1L
      private$.jobs <- dipsaus::fastmap2()

      # We do not allow users to change raw_data_dir once created
      private$.raw_path <- raveio_getopt("raw_data_dir")

      # Automatically set cache path, it can be changed
      self$cache_path <- file.path(cache_root(), "_automation_", job_name)

      dir_create2(self$log_path)
    },

    load_registry = function(update = TRUE) {
      if(!is.data.frame(private$.registry_cache)) {
        update <- TRUE
      }

      if(update) {
        if(file.exists(self$registry_path)) {
          tbl <- utils::read.csv(self$registry_path, header = TRUE,
                                 colClasses = "character")
          colnms <- c("Filename", "Subject", "Block",
                      "Status", "Details", "LastModified",
                      "TaskStarted", "TaskEnded", "Directory")
          if(all(colnms %in% names(tbl))) {
            tbl <- tbl[, colnms]
            tbl$LastModified <- as.POSIXlt(tbl$LastModified)
            tbl$TaskStarted <- as.POSIXlt(tbl$TaskStarted)
            tbl$TaskEnded <- as.POSIXlt(tbl$TaskEnded)
            # re-order
            tbl <- tbl[order(tbl$TaskStarted, decreasing = TRUE),]
            private$.registry_cache <- tbl
            return(tbl)
          }
        }
        private$.registry_cache <- data.frame(
          Filename = character(0L),
          Subject = character(0L),
          Block = character(0L),
          Status = character(0L),
          Details = character(0L),
          LastModified = as.POSIXlt(character(0L)),
          TaskStarted = as.POSIXlt(character(0L)),
          TaskEnded = as.POSIXlt(character(0L)),
          Directory = character(0L)
        )
      }

      private$.registry_cache

    },

    check_file_registry = function() {

      # check the watch path
      fs <- list.files(
        private$.watch_path,
        all.files = FALSE,
        full.names = FALSE,
        include.dirs = FALSE,
        recursive = TRUE
      )
      fs <- fs[grepl(self$file_pattern, filenames(fs), ignore.case = TRUE)]

      if(!length(fs)) { return(character(0L)) }

      mtime <- file.mtime(file.path(private$.watch_path, fs))
      sel <- mtime >= self$time_threshold

      if(!any(sel)) { return(character(0L)) }

      fs <- fs[sel]

      registry <- self$load_registry()

      ignored_files <- registry$Filename[!registry$Status %in% c("queued")]

      fs <- fs[!fs %in% ignored_files]

      return(fs)

    },

    add_to_queue = function(files, force = FALSE) {
      files <- unique(files)
      files <- files[!is.na(files)]

      if(!length(files)) {
        return(length(private$.queue))
      }

      if(length(files) > 1) {
        lapply(files, function(file) {
          self$add_to_queue(file)
        })
        return(length(private$.queue))
      }

      # extract information
      file <- files

      tryCatch({
        item <- private$.get_status(file)

        if(force && item$Status == "running") {
          stop("A process is working on the block. Only one process can work on a block at a time.")
        }

        if(item$Status == "parse-error") {
          private$.set_status(item)
          stop(item$Details)
        }

        if(force || item$Status == "ready") {
          item$Details <- ""
          item$Status <- "queued"
          private$.set_status(item)
          if(force) {
            private$.queue <- unique(c(file, private$.queue))
            catgl("File [{file}] prepended to the task queue", level = "INFO")
          } else {
            private$.queue <- unique(c(private$.queue, file))
            catgl("File [{file}] appended to the task queue", level = "INFO")
          }
        } else if (item$Status == "queued") {
          if(!file %in% private$.queue) {
            private$.queue <- unique(c(private$.queue, file))
          }
        }
      }, error = function(e) {
        catgl("Cannot add file [{file}] to queue. Reason: ",
                      e$message, level = "ERROR")
      })


      return(length(private$.queue))

    },

    check_job_status = function() {
      jobs <- private$.jobs
      nms <- names(jobs)
      lapply(nms, function(file) {
        check <- jobs[[file]]
        if(!is.function(check)) {
          private$.queue <- private$.queue[!private$.queue %in% file]
          jobs[["@remove"]](file)
          catgl("Cannot find process handler of file [{file}]. Removed from queue", level = "WARNING")
          return()
        }
        code <- check()
        if(code == 0) {
          item <- private$.get_status(file)
          item$Status <- "finished"
          item$Details <- ""
          item$TaskEnded <- as.POSIXlt(Sys.time())
          private$.set_status(item)
          # remove from queue
          private$.queue <- private$.queue[!private$.queue %in% file]
          jobs[["@remove"]](file)
          catgl("File [{file}] finished. Removed from queue", level = "INFO")
          return()
        }
        if(code < 0) {
          item <- private$.get_status(file)
          item$Status <- "errored"
          item$Details <- paste(attr(code, "rs_exec_error"), collapse = "")
          item$TaskEnded <- as.POSIXlt(Sys.time())
          private$.set_status(item)
          # remove from queue
          private$.queue <- private$.queue[!private$.queue %in% file]
          jobs[["@remove"]](file)
          catgl("File [{file}] errored (reason: {item$Details}). Removed from queue", level = "ERROR")
          return()
        }
      })
      length(jobs)
    },

    get_pipeline_default_settings = function() {
      re <- lapply(private$pipeline_names, function(pname) {
        pipeline <- pipeline(pname, paths = file.path(R_user_dir('raveio', "data"), "pipelines"))
        settings <- dipsaus::list_to_fastmap2(pipeline$get_settings())
        settings[["@remove"]](c(
          "import_setup__project_name",
          "import_setup__subject_code",
          "force_import",
          "skip_validation",
          "import_channels__sample_rate",
          "import_channels__electrodes",
          "import_channels__electrode_file",
          "import_blocks__format",
          "import_blocks__session_block",
          "project_name",
          "subject_code",
          "electrode_group",
          "changes"
        ))
        if(pname == "notch_filter") {
          settings$diagnostic_plot_params$path <- NULL
        }
        as.list(settings, sorted = TRUE)
      })
      names(re) <- private$pipeline_names
      re
    },

    create_settings_file = function(overwrite = FALSE) {
      settings_path <- file.path(self$log_path, "settings.yaml")
      if(!overwrite && file.exists(settings_path)) {
        stop("Existing settings file already created. If you want to overwrite that file, use `overwrite=TRUE`")
      }
      backup_file(settings_path, remove = FALSE)
      save_yaml(self$get_pipeline_default_settings(), file = settings_path)
      catgl("A settings file has been created at [{settings_path}]", level = "INFO")
      settings_path
    },

    get_pipeline_settings = function(pname, file, brfile) {
      item <- private$.get_status(file)

      # load blackrock file
      electrode_table <- brfile$electrode_table
      electrodes <- electrode_table$Electrode[electrode_table$NSType == self$NSType_LFP]

      # load pipeline
      pipeline <- pipeline(pname, paths = file.path(R_user_dir('raveio', "data"), "pipelines"))
      settings <- dipsaus::list_to_fastmap2(pipeline$get_settings())

      # override user-defined settings
      settings_path <- file.path(self$log_path, "settings.yaml")
      if(file.exists(settings_path)) {
        tmp <- load_yaml(settings_path)
        dipsaus::list_to_fastmap2(as.list(tmp[[pname]]), settings)
      }

      # pipeline-specific settings
      subject_code <- sprintf("%s__%s", item$Subject, item$Block)

      switch (
        pname,
        "import_lfp_native" = {
          settings$import_setup__project_name <- self$project_name
          settings$import_setup__subject_code <- subject_code
          settings$force_import <- TRUE
          settings$skip_validation <- FALSE
          srate <- brfile$sample_rates[[self$NSType_LFP]]
          settings$import_channels__sample_rate <- srate
          settings$import_channels__electrodes <- dipsaus::deparse_svec(electrodes)
          settings$import_channels__electrode_file <- "auto"
          settings$import_blocks__format <- names(IMPORT_FORMATS)[unlist(IMPORT_FORMATS) == 'native_blackrock']
          settings$import_blocks__session_block <- item$Block
        },
        "notch_filter" = {
          graph_path <- file.path(private$.raw_path, item$Subject, item$Block, "notch-diagnostic-plots")
          settings$project_name <- self$project_name
          settings$subject_code <- subject_code

          graph_path <- dir_create2(graph_path)
          settings$diagnostic_plot_params$path <- file.path(graph_path, "notch-diagnostic-plots.pdf")
        },
        "wavelet_module" = {
          settings$project_name <- self$project_name
          settings$subject_code <- subject_code
        },
        "reference_module" = {
          settings$project_name <- self$project_name
          settings$subject_code <- subject_code
          settings$reference_name <- "[new reference]"
        },
        {}
      )

      return(settings)

    },

    process_file = function(file) {

      item <- private$.get_status(file)
      brfile <- BlackrockFile$new(path = file.path(private$.watch_path, file), block = item$Block)

      # prepare working directory
      workdir <- file.path(self$cache_path, paste0(file, ".workdir"))
      if(dir.exists(workdir)) {
        unlink(workdir, force = TRUE, recursive = TRUE)
      }
      workdir <- dir_create2(workdir)


      # copy pipelines
      for(pname in private$pipeline_names) {
        pipeline <- pipeline(pname, paths = file.path(R_user_dir('raveio', "data"), "pipelines"))
        dest <- file.path(workdir, "pipelines", pipeline$pipeline_name)
        pipeline_fork(
          src = pipeline$pipeline_path,
          dest = dest,
          activate = FALSE
        )
        pipeline <- pipeline(pname, paths = file.path(workdir, "pipelines"))
        settings <- self$get_pipeline_settings(pname = pname, file = file, brfile = brfile)
        pipeline$set_settings(.list = settings)
        catgl("Set pipeline [{pname}]:", level = "DEFAULT")
        save_yaml(settings, file = stdout())
      }

      # copy files
      block_path <- file.path(private$.raw_path, item$Subject, item$Block)
      dir_create2(block_path)
      fs <- paste0(brfile$base_path, c(".nev", ".ccf", paste0(".ns", 1:9)))
      fs <- fs[file.exists(fs)]
      for(f in fs) {
        file.copy(f, file.path(block_path, basename(f)), overwrite = TRUE, recursive = FALSE, copy.date = TRUE)
      }

      fake_path <- file.path(private$.raw_path, sprintf("%s__%s", item$Subject, item$Block))
      fake_path <- dir_create2(fake_path)
      if(!file.exists(file.path(fake_path, item$Block))) {
        if(dipsaus::get_os() == "windows") {
          file.copy(block_path, to = fake_path, recursive = TRUE, copy.date = TRUE)
        } else {
          file.symlink(block_path, to = fake_path)
        }
      }

      # Make sure the subject surface files can be loaded properly?
      if(!file.exists(file.path(fake_path, 'rave-imaging'))) {
        # check if original subject has the fs recon
        imaging_path_orig <- file.path(private$.raw_path, item$Subject, 'rave-imaging')
        if(file.exists(imaging_path_orig)) {
          if(dipsaus::get_os() == "windows" || !dir.exists(imaging_path_orig)) {
            # On windows, symlink does not work well so just copy
            # On Unix, if rave-imaging is a symlink, then R (4.0) will treat
            # the path as a file. Just copy over
            file.copy(imaging_path_orig, to = fake_path,
                      recursive = TRUE, copy.date = TRUE)
          } else {
            file.symlink(imaging_path_orig, to = fake_path)
          }
        }
      }

      # set to running
      catgl("Start processing [{file}]", level = "INFO")
      item$Status <- "running"
      item$Details <- ""
      item$TaskStarted <- as.POSIXlt(Sys.time())
      item$TaskEnded <- as.POSIXlt(NA)
      private$.set_status(item)
      private$.jobs[[file]] <- dipsaus::rs_exec(
        name = file,
        focus_on_console = TRUE,
        rs = TRUE,
        wait = FALSE,
        quoted = TRUE,
        nested_ok = TRUE,
        expr = bquote({

          workdir <- .(workdir)
          cwd <- getwd()

          setwd(workdir)
          on.exit({ setwd(cwd) }, add = TRUE, after = FALSE)
          raveio <- asNamespace('raveio')

          if(dipsaus::package_installed('ravedash')){
            ravedash <- do.call('asNamespace', list('ravedash'))
            ravedash$set_logger_path(root_path = .(self$log_path), max_files = 10L)
            ravedash$logger_threshold("trace", type = 'file')
            ravedash$logger_threshold("trace", type = 'console')
          } else {
            ravedash <- NULL
          }
          blackrock_src <- .(file)

          pname <- "import_lfp_native"
          pipeline <- raveio$pipeline(pname, paths = file.path(workdir, "pipelines"))
          raveio$catgl("[{blackrock_src}]: Running pipeline: [{pname}] at [{pipeline$pipeline_path}]", level = "INFO")
          pipeline$run(async = FALSE, as_promise = FALSE,
                       scheduler = "none", type = "smart")
          raveio$catgl("[{blackrock_src}]: [{pname}] finished", level = "INFO")
          pname <- "notch_filter"
          pipeline <- raveio$pipeline(pname, paths = file.path(workdir, "pipelines"))
          raveio$catgl("[{blackrock_src}]: Running pipeline: [{pname}] at [{pipeline$pipeline_path}]", level = "INFO")
          pipeline$run(names = "apply_notch", async = FALSE, as_promise = FALSE,
                       scheduler = "none", type = "smart")

          pname <- "wavelet_module"
          pipeline <- raveio$pipeline(pname, paths = file.path(workdir, "pipelines"))
          raveio$catgl("[{blackrock_src}]: Running pipeline: [{pname}] at [{pipeline$pipeline_path}]", level = "INFO")
          pipeline$run(async = FALSE, as_promise = FALSE,
                       scheduler = "none", type = "smart")

          subject <- pipeline$read("subject")

          # generate reference if exists
          pname <- "reference_module"
          pipeline <- raveio$pipeline(pname, paths = file.path(workdir, "pipelines"))
          raveio$catgl("[{blackrock_src}]: Running pipeline: [{pname}] (reference_table_initial) at [{pipeline$pipeline_path}]", level = "INFO")
          # check subject's localization
          elec_path <- .(file.path(private$.raw_path, item$Subject, "rave-imaging", "electrodes.csv"))
          if(!file.exists(elec_path)) {
            elec_path <- .(file.path(raveio_getopt("data_dir"), private$.project_name, item$Subject, "rave", "meta", "electrodes.csv"))
          }

          if(!file.exists(elec_path)) {
            # list all projects, try to find
            all_projects <- raveio$get_projects(refresh = TRUE)
            elec_path <- file.path(raveio_getopt("data_dir"), all_projects,
                                        .(item$Subject), "rave", "meta", "electrodes.csv")
            elec_path <- elec_path[!is.na(elec_path) & file.exists(elec_path)]
          }
          if(length(elec_path)) {
            elec_path <- elec_path[[1]]
          }

          if(length(elec_path) == 1 && !is.na(elec_path) && file.exists(elec_path)) {
            tryCatch({
              elec_path <- elec_path[[1]]
              elec_table <- utils::read.csv(elec_path)
              elec_table$Electrode <- as.integer(elec_table$Electrode)
              if(length(elec_table$Electrode) == length(subject$electrodes)) {
                o <- order(elec_table$Electrode)
                elec_table <- elec_table[o, ]
                elec_table$Electrode <- sort(subject$electrodes)
              }

              raveio$safe_write_csv(elec_table, file.path(subject$meta_path, "electrodes.csv"),
                                    row.names = FALSE)

            }, error = function(e) {
              if(is.environment(ravedash)) {
                ravedash$logger_error_condition(e, level = "warning")
              } else {
                warning(e)
              }
            })
          }
          pipeline$run(names = "reference_table_initial",
                       async = FALSE, as_promise = FALSE,
                       scheduler = "none", type = "smart")
          unsaved_meta <- file.path(subject$meta_path, "reference__unsaved.csv")
          target_meta <- file.path(subject$meta_path, "reference_auto_generated.csv")
          if(file.exists(unsaved_meta) && !file.exists(target_meta)) {
            file.copy(unsaved_meta, target_meta, overwrite = TRUE)
          }

          # make subject backward-compatible
          raveio$catgl("[{blackrock_src}]: Making data format backward-compatible", level = "INFO")
          raveio$rave_subject_format_conversion(subject$subject_id)
          raveio$catgl("[{blackrock_src}]: Done", evel = "INFO")

        })
      )

    },

    scan = function() {

      files <- self$check_file_registry()
      self$add_to_queue(files)

      # check job status
      njobs <- self$check_job_status()
      inactives <- private$.queue[!private$.queue %in% names(private$.jobs)]

      if(length(inactives) && njobs < private$.max_jobs) {
        # schedule job
        navails <- private$.max_jobs - njobs
        inactives <- inactives[seq_len(min(navails, length(inactives)))]
        for(file in inactives) {
          self$process_file(file)
        }
      }

    },

    reset_registry = function() {
      if(!interactive()) {
        stop("Cannot reset registry in non-interactive mode")
      }
      ans <- dipsaus::ask_yesno(sprintf("Clearing registry for [%s]?", private$.job_name))
      if(isTRUE(ans)) {
        unlink(self$registry_path, force = TRUE)
      }
    },

    watch = function(interval = 5) {
      interval <- as.numeric(interval)
      if(!isTRUE(interval >= 1)) {
        stop("Min interval must be >= 1 seconds")
      }

      if(dipsaus::package_installed('ravedash')){
        ravedash <- do.call('asNamespace', list('ravedash'))
        ravedash$set_logger_path(root_path = self$log_path, max_files = 10L)
        ravedash$logger_threshold("trace", type = 'file')
        ravedash$logger_threshold("trace", type = 'console')
      } else {
        ravedash <- NULL
      }

      on.exit({
        if(is.environment(ravedash)) {
          ravedash$set_logger_path(NULL)
        }
      }, add = TRUE, after = TRUE)

      # make sure directories are there
      dir_create2(self$log_path)
      dir_create2(self$cache_path)

      while(TRUE) {
        tryCatch({
          self$scan()
        }, error = function(e) {
          catgl("Error raised while the master process rescans/schedules tasks. Reason: {paste(e$message, collapse = '\n')}\nWill try again later",
                level = "ERROR")
        })
        Sys.sleep(interval)
      }
    }

  ),
  active = list(

    log_path = function() {
      file.path(private$.raw_path, "_automation", private$.job_name)
    },

    registry_path = function() {
      file.path(self$log_path, "registry.csv")
    },

    time_threshold = function(v) {
      if(!missing(v)) {

        if(length(v) != 1 || is.na(v)) {
          stop("Cannot set time threshold with invalid time")
        }
        tm <- v
        if(is.character(tm)) {
          v <- as.POSIXlt(tm)
          if(is.na(v)) {
            stop("`time_threshold` must have format of [year-month-day hour:minute:second]. For example, '2022-08-03 16:38:00'")
          }
        } else if (!inherits(tm, "POSIXlt")) {
          stop("`time_threshold` must be characters or a `POSIXlt` time object")
        }

        private$.time_threshold <- v

      }
      private$.time_threshold
    },

    project_name = function(v) {
      if(!missing(v)) {
        if(!length(v)) {
          private$.project_name <- character(0)
        } else if(length(v) > 1) {
          stop("Project name must have length of 1")
        } else if(!grepl("^[a-zA-Z0-9_]", v)) {
          stop("Project name can only contain letters, digits, and underscore [_]")
        } else {
          private$.project_name <- as.character(v)
        }
      }
      pn <- private$.project_name
      if(!length(pn)) {
        pn <- "automated"
      }
      pn
    },

    file_pattern = function(v) {
      if(!missing(v)) {
        v <- v[[1]]
        m <- gregexpr("(\\([^\\(\\)]+\\))", v, ignore.case = TRUE)
        m <- unique(m[[1]])
        if(length(m) < 2) {
          stop("File pattern must be a regular expression containing at least two keyword extractors so I can decide the subject code and session block ID. For example, regular expression ['^([a-zA-Z0-9]+)_datafile_([a-zA-Z0-9]+)\\.nev$'] matches [YAB_datafile_001.nev]. RAVE will set subject code to [YAB], and block ID as [001].")
        }
        private$.file_pattern <- v
      }
      private$.file_pattern
    },

    queued = function() {
      private$.queue
    },

    max_jobs = function(v) {
      if(!missing(v)) {
        errored <- TRUE
        if(length(v) == 1) {
          v <- as.integer(v)
          if(isTRUE(v > 0)) {
            private$.max_jobs <- v
            errored <- FALSE
          }
        }

        if(errored) {
          stop("Cannot set `max_jobs`, the value must be a positive integer")
        }

      }
      private$.max_jobs
    }

  )
)


#' Monitors 'BlackRock' output folder and automatically import data into 'RAVE'
#' @description Automatically import 'BlackRock' files from designated folder
#' and perform 'Notch' filters, 'Wavelet' transform; also generate epoch,
#' reference files.
#' @param watch_path the folder to watch
#' @param project_name the project name to generate
#' @param task_name the watcher's name
#' @param scan_interval scan the directory every \code{scan_interval} seconds,
#' cannot be lower than 1
#' @param time_threshold time-threshold of files: all files with modified
#' time prior to this threshold will be ignored; default is current time
#' @param max_jobs maximum concurrent imports, default is 1
#' @param as_job whether to run in 'RStudio' background job or to block the
#' session when monitoring; default is auto-detected
#' @param dry_run whether to dry-run the code (instead of executing the
#' scripts, return the watcher's instance and open the settings file);
#' default is false
#' @param config_open whether to open the pipeline configuration file; default
#' is equal to \code{dry_run}
#' @return When \code{dry_run} is true, then the watcher's instance will be
#' returned; otherwise nothing will be returned.
#' @export
auto_process_blackrock <- function(
    watch_path, project_name = "automated", task_name = "RAVEWatchDog",
    scan_interval = 10, time_threshold = Sys.time(), max_jobs = 1L,
    as_job = NA, dry_run = FALSE, config_open = dry_run
) {

  time_threshold <- as.POSIXlt(time_threshold)
  time_threshold <- time_threshold[!is.na(time_threshold)]
  if(!length(time_threshold)) {
    time_threshold <- as.POSIXlt(Sys.time())
  } else {
    time_threshold <- time_threshold[[1]]
  }

  if(!isFALSE(as_job)) {
    as_job <- dipsaus::rs_avail()
  }

  fun <- dipsaus::new_function2(body = bquote({

    raveio <- asNamespace('raveio')
    watcher <- raveio$RAVEWatchDog$new(
      watch_path = .(watch_path),
      job_name = .(task_name)
    )
    watcher$time_threshold <- .(time_threshold)
    watcher$max_jobs <- .(max_jobs)
    watcher$project_name <- .(project_name)

    settings_path <- file.path(watcher$log_path, "settings.yaml")
    if(!file.exists(settings_path)) {
      watcher$create_settings_file()
    }

    return(watcher)

  }), quote_type = "quote")

  watcher <- fun()
  if( config_open ) {

    settings_path <- file.path(watcher$log_path, "settings.yaml")
    if(!file.exists(settings_path)) {
      watcher$create_settings_file()
    }

    try({
      dipsaus::rs_edit_file(settings_path)
      catgl("Watcher's settings file has been opened. Please check the settings, and edit if necessary. All auto-discovered BlackRock files will be preprocessed using this settings file.", level = "INFO")
    }, silent = TRUE)

  }

  if( dry_run ) {
    return(watcher)
  }



  if(as_job) {
    dipsaus::rs_exec(
      bquote({
        fun <- dipsaus::new_function2(body = .(body(fun)))
        # return(fun)
        watcher <- fun()
        watcher$watch(interval = .(scan_interval))
      }),
      quoted = TRUE,
      rs = TRUE,
      name = task_name,
      focus_on_console = TRUE
    )
  } else {
    watcher$watch(interval = scan_interval)
  }

}
