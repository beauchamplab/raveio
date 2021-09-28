library(targets)

# tar_option_set(packages = c("raveio"))

...targets <- list(
  set_project_name = tar_target(
    project_name,
    "demo"
  ),
  set_subject_code = tar_target(
    subject_code,
    "DemoSubject"
  ),
  load_subject_basic_information = tar_target(
    basic_info,
    {
      re <- dipsaus::fastmap2()
      re$subject <- raveio::RAVESubject$new(
        project_name = project_name,
        subject_code = subject_code, strict = FALSE)
      re$project <- re$subject$project
      raveio::catgl("Initialized [{project_name}/{subject_code}]", level = "DEFAULT", .pal = list("DEFAULT" = "#1874CD"))
      re
    }
  ),
  load_initialize_settings = tar_target(
    settings_initialize,
    {
      s <- yaml::read_yaml("settings.yaml")
      s$electrodes <- dipsaus::parse_svec(s$electrodes)
      s[c("electrodes", "epoch", "reference", "intervals")]
    },
    cue = tar_cue("always")
  ),
  load_epoch = tar_target(
    epoch,
    raveio::RAVEEpoch$new(basic_info$subject, settings_initialize$epoch),
    storage = "main",
    cue = tar_cue(mode = "always") # maybe not?
  ),
  load_reference_table = tar_target(
    reference,
    basic_info$subject$meta_data(
      meta_type = "reference",
      meta_name = settings_initialize$reference),
    storage = "main",
    cue = tar_cue(mode = "always") # maybe not?
  ),
  initialize_electrode_instances = tar_target(
    electrodes,
    {
      inst <- lapply(settings_initialize$electrodes, function(ei){
        e <- raveio::LFP_electrode$new(basic_info$subject, number = ei)

        # set epoch
        e$set_epoch(epoch)

        # set epoch range (-1 to 2 seconds relative to onset)
        e$trial_intervals <- settings_initialize$intervals

        # set reference
        ref_name <- subset(reference, Electrode == ei)[["Reference"]]
        e$set_reference(ref_name)
        e
      })
      names(inst) <- settings_initialize$electrodes
      inst
    }
  )
)



