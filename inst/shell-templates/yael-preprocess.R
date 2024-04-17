#!/usr/bin/env Rscript --no-save --no-restore

# check if rpyANTs is configured
if( !isTRUE(rpyANTs::ants_available()) ) {
  rpyANTs::install_ants()
}

# Prepare working directory: FreeSurfer does not like it when
# directories contain spaces
wdir_actual="{{ work_path_actual }}"

# Stage 1: import images
project_name <- "{{ project_name }}"
subject_code="{{ subject_code }}"
t1w_path="{{ t1w_path }}"
ct_path="{{ ct_path }}"
t2w_path="{{ t2w_path }}"
fgatir_path="{{ fgatir_path }}"
preopct_path="{{ preopct_path }}"
flair_path="{{ flair_path }}"
t1w_contrast_path="{{ t1w_contrast_path }}"
register_reversed={{ register_reversed }}
normalize_template={{ normalize_template_str }}
run_recon_all={{ run_recon_all }}

if( !run_recon_all ) {
  raveio::yael_preprocess(
    subject_code = subject_code,
    t1w_path = t1w_path,
    ct_path = ct_path,
    t2w_path = t2w_path,
    fgatir_path = fgatir_path,
    preopct_path = preopct_path,
    flair_path = flair_path,
    t1w_contrast_path = t1w_contrast_path,
    register_reversed = register_reversed,
    normalize_template = normalize_template,
    add_surfaces = TRUE
  )
  surf_path <- file.path(wdir_actual, "rave-imaging", "ants", "surf")
  if(dir.exists(surf_path)) {
    message("Finalizing 1 of 2...")
    pial_path = file.path(surf_path, "lh.pial")
    envelope_path = file.path(surf_path, "lh.pial-outer-smoothed")
    if(file.exists(pial_path) && !file.exists(envelope_path)) {
      threeBrain::generate_smooth_envelope(
        surface_path = pial_path,
        save_as = envelope_path,
        inflate = 3,
        verbose = TRUE,
        save_format = 'bin'
      )
    }

    message("Finalizing 2 of 2...")
    pial_path = file.path(surf_path, "rh.pial")
    envelope_path = file.path(surf_path, "rh.pial-outer-smoothed")
    if(file.exists(pial_path) && !file.exists(envelope_path)) {
      threeBrain::generate_smooth_envelope(
        surface_path = pial_path,
        save_as = envelope_path,
        inflate = 3,
        verbose = TRUE,
        save_format = 'bin'
      )
    }
  }
} else {
  # Run FreeSurfer
  subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code, strict = FALSE)
  raveio::yael_preprocess(
    subject_code = subject_code,
    t1w_path = t1w_path,
    ct_path = ct_path,
    t2w_path = t2w_path,
    fgatir_path = fgatir_path,
    preopct_path = preopct_path,
    flair_path = flair_path,
    t1w_contrast_path = t1w_contrast_path,
    register_reversed = register_reversed,
    normalize_template = normalize_template,
    add_surfaces = FALSE
  )
  # Prepare for the FreeSurfer
  raveio::cmd_run_recon_all(
    subject = subject,
    mri_path = file.path(wdir_actual, "rave-imaging", "inputs", "MRI", "MRI_RAW.nii.gz"),
    args = "-all",
    overwrite = TRUE,
    verbose = TRUE,
    dry_run = FALSE
  )
  # Copy so 3D viewer can recognize it (The YAEL pipeline does not conform, hence should have better quality)
  ants_vol <- file.path(wdir_actual, "rave-imaging", "ants", "mri", "brain.finalsurfs.nii.gz")
  dst_vol <- file.path(wdir_actual, "rave-imaging", "fs", "mri", "rave_slices.nii.gz")
  if(file.exists(ants_vol)) {
    file.copy(ants_vol, dst_vol, overwrite = TRUE)
  }

  surf_path <- file.path(wdir_actual, "rave-imaging", "fs", "surf")
  if(dir.exists(surf_path)) {
    message("Finalizing 1 of 2...")
    pial_path = file.path(surf_path, "lh.pial")
    envelope_path = file.path(surf_path, "lh.pial-outer-smoothed")
    if(file.exists(pial_path) && !file.exists(envelope_path)) {
      threeBrain::generate_smooth_envelope(
        surface_path = pial_path,
        save_as = envelope_path,
        inflate = 3,
        verbose = TRUE,
        save_format = 'bin'
      )
    }

    message("Finalizing 2 of 2...")
    pial_path = file.path(surf_path, "rh.pial")
    envelope_path = file.path(surf_path, "rh.pial-outer-smoothed")
    if(file.exists(pial_path) && !file.exists(envelope_path)) {
      threeBrain::generate_smooth_envelope(
        surface_path = pial_path,
        save_as = envelope_path,
        inflate = 3,
        verbose = TRUE,
        save_format = 'bin'
      )
    }
  }
}


message("Done.")

## END OF RAVE Script: YAEL Preprocessing + (optional) FreeSurfer recon-all ##

