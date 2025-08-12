#' @importFrom ravecore RAVEAbstarctElectrode
#' @export
ravecore::RAVEAbstarctElectrode

#' @importFrom ravecore new_electrode
#' @export
ravecore::new_electrode

#' @importFrom ravecore new_reference
#' @export
ravecore::new_reference

#' @importFrom ravecore RAVEEpoch
#' @export
ravecore::RAVEEpoch

#' @importFrom ravecore LFP_electrode
#' @export
ravecore::LFP_electrode

#' @importFrom ravecore Spike_electrode
#' @export
ravecore::Spike_electrode

#' @importFrom ravecore Auxiliary_electrode
#' @export
ravecore::Auxiliary_electrode

#' @importFrom ravecore LFP_reference
#' @export
ravecore::LFP_reference

#' @importFrom ravecore RAVEPreprocessSettings
#' @export
ravecore::RAVEPreprocessSettings

#' @importFrom ravecore cmd_execute
#' @export
ravecore::cmd_execute

#' @importFrom ravecore as_rave_project
#' @export
ravecore::as_rave_project

#' @importFrom ravecore get_projects
#' @export
ravecore::get_projects

#' @importFrom ravecore RAVEProject
#' @export
ravecore::RAVEProject

#' @importFrom ravecore as_rave_subject
#' @export
ravecore::as_rave_subject

#' @importFrom ravecore RAVESubject
#' @export
ravecore::RAVESubject

#' @importFrom ravecore YAELProcess
#' @export
ravecore::YAELProcess

#' @importFrom ravecore as_yael_process
#' @export
ravecore::as_yael_process

#' @importFrom ravecore prepare_subject_bare0
#' @export
ravecore::prepare_subject_bare0

#' @importFrom ravecore prepare_subject_bare
#' @export
ravecore::prepare_subject_bare

#' @importFrom ravecore prepare_subject_with_epoch
#' @export
ravecore::prepare_subject_with_epoch

#' @importFrom ravecore prepare_subject_raw_voltage_with_epoch
#' @export
ravecore::prepare_subject_raw_voltage_with_epoch

#' @importFrom ravecore prepare_subject_voltage_with_epoch
#' @export
ravecore::prepare_subject_voltage_with_epoch

#' @importFrom ravecore prepare_subject_time_frequency_coefficients
#' @export
ravecore::prepare_subject_time_frequency_coefficients

#' @importFrom ravecore prepare_subject_power
#' @export
ravecore::prepare_subject_power

#' @importFrom ravecore prepare_subject_phase
#' @export
ravecore::prepare_subject_phase

#' @importFrom ravecore archive_subject
#' @export
ravecore::archive_subject

#' @importFrom ravecore install_subject
#' @export
ravecore::install_subject

#' @importFrom ravecore generate_reference
#' @export
ravecore::generate_reference

#' @importFrom ravecore cache_root
#' @export
ravecore::cache_root

#' @importFrom ravecore backup_file
#' @export
ravecore::backup_file

#' @importFrom ravecore clear_cached_files
#' @export
ravecore::clear_cached_files

#' @importFrom ravecore rave_brain
#' @export
ravecore::rave_brain

#' @importFrom ravecore transform_point_to_template
#' @export
ravecore::transform_point_to_template

#' @importFrom ravecore transform_thinfilm_to_mni152
#' @export
ravecore::transform_thinfilm_to_mni152

#' @importFrom ravecore compose_channel
#' @export
ravecore::compose_channel

#' @importFrom ravecore validate_time_window
#' @export
ravecore::validate_time_window

#' @importFrom ravecore load_meta2
#' @export
ravecore::load_meta2

#' @importFrom ravecore save_meta2
#' @export
ravecore::save_meta2

#' @importFrom ravecore validate_subject
#' @export
ravecore::validate_subject

#' @importFrom ravecore cmd_run_r
#' @export
ravecore::cmd_run_r

#' @importFrom ravecore SIGNAL_TYPES
#' @export
ravecore::SIGNAL_TYPES

#' @importFrom ravecore LOCATION_TYPES
#' @export
ravecore::LOCATION_TYPES

#' @importFrom ravecore MNI305_to_MNI152
#' @export
ravecore::MNI305_to_MNI152

#' @importFrom ravecore YAEL_IMAGE_TYPES
#' @export
ravecore::YAEL_IMAGE_TYPES

#' @importFrom ravecore IMPORT_FORMATS
#' @export
ravecore::IMPORT_FORMATS

#' @importFrom ravecore export_table
#' @export
ravecore::export_table

#' @importFrom ravecore import_table
#' @export
ravecore::import_table

rscript_path <- function(winslash = "\\") {
  call_pkg_fun("ravecore", "rscript_path", winslash = winslash)
}



get_os <- function() {
  call_pkg_fun("ravecore", "get_os")
}

normalize_path <- function(path, must_work = NA) {
  call_pkg_fun("ravecore", "path_abs", path = path, must_work = must_work)
}

initialize_imaging_paths <- function(subject) {
  # subject <- 'demo/DemoSubject'
  subject <- restore_subject_instance(subject, strict = FALSE)
  root_path <- file.path(subject$preprocess_settings$raw_path, "rave-imaging")
  dir_create2(file.path(root_path, "coregistration"))
  dir_create2(file.path(root_path, "log"))
  dir_create2(file.path(root_path, "scripts"))
  dir_create2(file.path(root_path, "inputs"))
  invisible()
}

run_wavelet <- function(
    subject, electrodes, freqs, cycles,
    target_sample_rate = 100,
    kernels_precision = "float", pre_downsample = 1,
    verbose = TRUE, ...
) {

  ravecore::run_wavelet(
    subject = subject,
    electrodes = electrodes,
    freqs = freqs,
    cycles = cycles,
    target_sample_rate = target_sample_rate,
    kernels_precision = kernels_precision,
    pre_downsample = pre_downsample,
    verbose = verbose,
    ...
  )
}
