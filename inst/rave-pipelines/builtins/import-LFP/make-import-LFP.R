library(targets)

source("common.R")
# tar_option_set(packages = c("raveio"))

pipeline_targets <- list()

# Monitor any changes in the settings path
pipeline_targets$monitor_settings_path <- tar_target(
  name = settings_path,
  command = "settings.yaml",
  format = "file"
)

# load settings
pipeline_targets$load_settings_file <- tar_target(
  name = settings,
  command = {
    settings <- raveio::load_yaml(settings_path)
  }
)

# Validate basic inputs (project, subject, format) and create preprocess instance
pipeline_targets$validate_basic_inputs <- tar_target(
  name = preprocess_settings,
  command = {

    subject_code <- settings$subject_code
    project_name <- settings$project_name
    source_format <- settings$file_format

    if(isFALSE(project_name != "")) {
      stop("Project name is blank")
    }
    if(isFALSE(subject_code != "")) {
      stop("Subject code is blank")
    }
    regexp <- "^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$"
    if(!stringr::str_detect(project_name, regexp)){
      stop("Invalid project name: must starts with letters and followed by letters, digits, dash (-), and/or underscore (_)")
    }
    if(!stringr::str_detect(subject_code, regexp)){
      stop("Invalid subject code: must starts with letters and followed by letters, digits, dash (-), and/or underscore (_)")
    }

    if(length(source_format) != 1 || !source_format %in% 1:6){
      stop("`file_format` is invalid, must be 1-6: \n  ",
           paste0(1:6, ". ", names(raveio::IMPORT_FORMATS), collapse = "\n  "))
    }

    if(source_format %in% 1:4){
      expected_raw_type <- "native"
      dirs <- raveio::rave_directories(
        subject_code = subject_code,
        project_name = project_name,
        .force_format = 'native'
      )
    } else {
      expected_raw_type <- "bids"
      dirs <- raveio::rave_directories(
        subject_code = subject_code,
        project_name = project_name,
        .force_format = 'BIDS'
      )
    }

    if(!dir.exists(dirs$raw_path)) {
      stop(raveio::glue("Cannot find subject directory (raw)"))
    } else if(dirs$.raw_path_type != expected_raw_type){
      if( expected_raw_type == "bids" ){
        stop(raveio::glue("A BIDS format is specified, but the raw subject directory ({dirs$raw_path}) is {dirs$.raw_path_type}?"))
      } else {
        stop(raveio::glue("A RAVE native format is specified, but the raw subject directory ({dirs$raw_path}) is {dirs$.raw_path_type}?"))
      }
    }

    preproc <- raveio::RAVEPreprocessSettings$new(
      sprintf("%s/%s", project_name, subject_code), read_only = FALSE)

    # if(file.exists(preproc$path) || any(preproc$data_imported)){
    #   return(list(
    #     pass = FALSE,
    #     message = raveio::glue("Subject is found.")
    #   ))
    # }

    if(!length(preproc$all_blocks)){
      stop(raveio::glue("Subject directory (raw) is found at {dirs$raw_path}, but there is no session/block in it."))
    }
    preproc
  }
)

# Create subject (no data imported yet)
pipeline_targets$ensure_subject <- tar_target(
  name = ensure_subject,
  command = {
    # preprocess_settings <- preproc
    if(!file.exists(preprocess_settings$path)){
      preprocess_settings$subject$initialize_paths(include_freesurfer = FALSE)
      preprocess_settings$save()
    }
    file.exists(preprocess_settings$path)
  },
  cue = targets::tar_cue("always")
)

# Set blocks, channel, sample rates
pipeline_targets$set_blocks <- tar_target(
  name = blocks,
  command = {
    if(!ensure_subject){ stop("Subject not created") }

    # # Do not use on old RAVEPreprocessSettings as it might be out-dated
    preproc <- raveio::RAVEPreprocessSettings$new(
      sprintf("%s/%s", settings$project_name, settings$subject_code), read_only = FALSE)

    sel <- settings$blocks %in% preproc$all_blocks
    if(!all(sel)){
      stop("Some block folders are missing: ", paste(settings$blocks[!sel], collapse = ", "))
    }
    sel <- preproc$electrodes %in% unlist(lapply(settings$channels, '[[', 'number'))
    if(length(sel) && any(preproc$data_imported[sel])){
      if(!setequal(settings$blocks, preproc$blocks)){
        stop("The subject has been imported before. The block was ",
             paste(preproc$blocks, collapse = ", "),
             ". However, block ",
             paste(settings$blocks, collapse = ", "), " is to be set.")
      }
    } else {
      preproc$set_blocks(settings$blocks, force = TRUE)
      preproc$save()
    }
    settings$blocks
  },
  cue = targets::tar_cue("always")
)

pipeline_targets$set_electrodes <- tar_target(
  name = electrodes,
  command = {
    force(blocks)

    # # Do not use on old RAVEPreprocessSettings as it might be out-dated
    preproc <- raveio::RAVEPreprocessSettings$new(
      sprintf("%s/%s", settings$project_name, settings$subject_code), read_only = FALSE)

    # set electrodes
    # TODO: set other types
    changed <- FALSE
    electrode_number <- sapply(settings$channels, function(channel){ as.integer(channel$number) })
    electrode_type <- sapply(settings$channels, function(channel){ c(channel$type, "LFP")[[1]] })
    electrode_srate <- sapply(settings$channels, function(channel){ channel$sample_rate })

    imported_electrodes <- preproc$electrodes
    imported_srates <- preproc$sample_rates
    imported_types <- preproc$electrode_types
    if(length(imported_electrodes)){
      imported_electrodes <- imported_electrodes[preproc$data_imported]
    }

    # for(type in unique(electrode_type)){
    #   elec <- electrode_number[electrode_type == type]
    #   srate <- electrode_srate[electrode_type == type][[1]]
    #   elec <- elec[!elec %in% imported_electrodes]
    #   if(length(elec)){
    #     preproc$set_electrodes(electrodes = elec, type = type, add = TRUE)
    #     changed <- TRUE
    #   }
    #   if(!isTRUE(any(imported_types %in% type)) || is.na(imported_srates[imported_types %in% type][[1]])){
    #     preproc$set_sample_rates(srate, type = type)
    #     changed <- TRUE
    #   }
    # }
    for(channel in settings$channels) {
      num <- as.integer(channel$number)
      if(!num %in% imported_electrodes) {
        preproc$set_electrodes(electrodes = num, type = channel$type, add = TRUE)
        preproc$set_sample_rates(channel$sample_rate, type = channel$type)
        new_electrodes <- TRUE
      }
    }

    # remove electrodes (try)
    not_imported <- preproc$electrodes
    not_imported <- not_imported[!not_imported %in% imported_electrodes]
    not_imported <- not_imported[!not_imported %in% electrode_number]
    if(length(not_imported)){
      not_imported <- as.character(as.integer(not_imported))
      preproc$data$`@remove`(not_imported)
      es <- preproc$data$electrodes
      preproc$data$electrodes <- es[!es %in% not_imported]
      changed <- TRUE
    }

    preproc$save()

    data.frame(
      Electrode = preproc$electrodes,
      Type = preproc$electrode_types,
      SampleRate = preproc$sample_rates
    )

  },
  cue = targets::tar_cue("always")
)


pipeline_targets$validate_configuration_before_importing <- tar_target(
  name = validate_preimport_results,
  command = {
    es <- electrodes$Electrode
    et <- electrodes$Type
    lfp_electrodes <- es[et == "LFP"]

    preproc <- raveio::RAVEPreprocessSettings$new(
      sprintf("%s/%s", settings$project_name, settings$subject_code), read_only = FALSE)

    # list(
    #   subject_id = preproc$subject$subject_id,
    #   blocks = preproc$blocks,
    #   lfp_electrodes = lfp_electrodes,
    #   lfp_sample_rate = preproc$`@lfp_ecog_sample_rate`
    # )
    # raveio::catgl(paste(
    #   "Setting subject [{preproc$subject$subject_id}]:\n",
    #   "    Blocks: ", paste(preproc$blocks, collapse = ", "), "\n",
    #   "    Electrodes: ", dipsaus::deparse_svec(es[et == "LFP"]), "\n",
    #   "    LFP sample rate: ", preproc$`@lfp_ecog_sample_rate`, "\n",
    #   sep = ""), level = "INFO")

    # Perform check
    check_results <- raveio::validate_raw_file(
      subject_code = preproc$subject$subject_code,
      blocks = preproc$blocks,
      electrodes = lfp_electrodes,
      project_name = preproc$subject$project_name,
      format = as.integer(settings$file_format)
    )
    if(isFALSE(check_results)){
      reason <- attr(check_results, "reason")
      names <- names(reason)
      msg <- lapply(seq_along(names), function(ii){
        raveio::glue("{ii}. {names[[ii]]} ({paste(reason[[ii]], collapse = ', ')})")
      })
      stop(paste(unlist(msg), collapse = "\n"))
    }
    check_results
  }
)

pipeline_targets$import_to_RAVE <- tar_target(
  name = import_to_rave,
  command = {
    force(validate_preimport_results)

    preproc <- raveio::RAVEPreprocessSettings$new(
      sprintf("%s/%s", settings$project_name, settings$subject_code), read_only = FALSE)

    etypes <- preproc$electrode_types
    etypes_unique <- unique(etypes)
    conversion <- settings$physical_unit
    if(!isTRUE(conversion %in% c("uV", "mV", "V"))){
      conversion <- NA
    }
    for(et in etypes_unique){
      elecs <- preproc$electrodes[etypes %in% et & !preproc$data_imported]
      raveio::rave_import(
        project_name = preproc$subject$project_name,
        subject_code = preproc$subject$subject_code,
        blocks = preproc$blocks,
        electrodes = elecs,
        format = settings$file_format,
        sample_rate = preproc$sample_rates[etypes %in% et][[1]],
        conversion = conversion,
        data_type = et,
        add = TRUE,
        skip_validation = TRUE
      )
    }

  }
)

pipeline_targets$generate_electrode_file <- tar_target(
  name = generate_electrode_file,
  command = {
    force(import_to_rave)

    # reload preprocess_instance (clean start)
    subject <-
      raveio::RAVESubject$new(project_name = settings$project_name,
                              subject_code = settings$subject_code)

    etable <- subject$meta_data("electrodes")

    if(is.null(etable)){
      etable <- data.frame(
        Electrode = subject$electrodes,
        Coord_x = 0,
        Coord_y = 0,
        Coord_z = 0,
        Label = "NoLabel"
      )
    } else {
      es <- subject$electrodes[!subject$electrodes %in% etable$Electrode]
      row <- etable[1,]
      row[] <- NA
      row <- do.call(rbind, lapply(es, function(e){
        row$Electrode <- e
        row$Coord_x <- 0
        row$Coord_y <- 0
        row$Coord_z <- 0
        row$Label <- "NoLabel"
        row
      }))
      etable <- rbind(etable, row)
    }
    etable <- etable[order(etable$Electrode),]
    etable <- etable[!duplicated(etable$Electrode), ]
    if(!dir.exists(subject$meta_path)){
      raveio::dir_create2(subject$meta_path)
    }
    csv <- file.path(subject$meta_path, "electrodes.csv")
    raveio::safe_write_csv(etable, csv)

    # also create reference
    raveio::safe_write_csv(
      data.frame(
        Electrode = etable$Electrode,
        Group = "default",
        Reference = "noref",
        Type = "No Reference"
      ),
      file.path(subject$meta_path, "reference_noref.csv")
    )

  }
)


pipeline_targets
#     save_pipeline_and_export_timestamp = tar_target_raw(
#       "save_pipeline_and_export_timestamp",
#       bquote({
#         if(.(save_scripts)){
#           src <- normalizePath(getwd())
#           dst <- file.path(subject$pipeline_path, .(target_name))
#           dst <- normalizePath(dst, mustWork = FALSE)
#           if(dst != src){
#             file.copy(src, subject$pipeline_path,
#                       recursive = TRUE, copy.date = TRUE)
#           }
#         }
#         timestamp <- Sys.time()
#         dir <- file.path(subject$pipeline_path, "_shared")
#         raveio::dir_create2(dir)
#         fname <- paste0("timestamp-", .(target_name), ".yaml")
#         raveio::save_yaml(list(
#           pipeline_name = .(target_name),
#           last_modified = timestamp
#         ), file.path(dir, fname))
#         timestamp
#       })
#     )
#   )
#
# })
#
#
#   generate_electrodes_csv = tar_target(
#     generate_electrodes_csv,
#     {
#       force(import_LFP)
#
#       # reload preprocess_instance (clean start)
#       subject <-
#         raveio::RAVESubject$new(project_name = settings$project_name,
#                                 subject_code = settings$subject_code)
#
#       etable <- subject$meta_data("electrodes")
#
#       if(is.null(etable)){
#         etable <- data.frame(
#           Electrode = subject$electrodes,
#           Coord_x = 0,
#           Coord_y = 0,
#           Coord_z = 0,
#           Label = "NoLabel"
#         )
#       } else {
#         es <- subject$electrodes[!subject$electrodes %in% etable$Electrode]
#         row <- etable[1,]
#         row[] <- NA
#         row <- do.call(rbind, lapply(es, function(e){
#           row$Electrode <- e
#           row$Coord_x <- 0
#           row$Coord_y <- 0
#           row$Coord_z <- 0
#           row$Label <- "NoLabel"
#           row
#         }))
#         etable <- rbind(etable, row)
#       }
#       etable <- etable[order(etable$Electrode),]
#       etable <- etable[!duplicated(etable$Electrode), ]
#       raveio::dir_create2(subject$meta_path)
#       csv <- file.path(subject$meta_path, "electrodes.csv")
#       raveio::safe_write_csv(etable, csv)
#
#       # also create reference
#       raveio::safe_write_csv(
#         data.frame(
#           Electrode = etable$Electrode,
#           Group = "default",
#           Reference = "noref",
#           Type = "No Reference"
#         ),
#         file.path(subject$meta_path, "reference_noref.csv")
#       )
#
#     }
#   )
# )
#
#
#
