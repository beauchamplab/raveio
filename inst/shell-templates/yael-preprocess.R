#!/usr/bin/env Rscript --no-save --no-restore

# check if rpyANTs is configured
if( !isTRUE(rpyANTs::ants_available()) ) {
  rpyANTs::install_ants()
}

# Prepare working directory: FreeSurfer does not like it when
# directories contain spaces
wdir_actual <- "{{ work_path_actual }}"

# Stage 1: import images
project_name <- "{{ project_name }}"
subject_code <- "{{ subject_code }}"
t1w_path <- "{{ t1w_path }}"
ct_path <- "{{ ct_path }}"
t2w_path <- "{{ t2w_path }}"
fgatir_path <- "{{ fgatir_path }}"
preopct_path <- "{{ preopct_path }}"
flair_path <- "{{ flair_path }}"
t1w_contrast_path <- "{{ t1w_contrast_path }}"
register_reversed <-{{ register_reversed }}
normalize_template= <-{{ normalize_template_str }}
run_recon_all <-{{ run_recon_all }}

max_mem_size <- raveio::raveio_getopt("max_mem", default = 8)
if(length(max_mem_size) == 1 && is.numeric(max_mem_size) && !is.na(max_mem_size) && isTRUE(max_mem_size < 12)) {
  # Cannot register to MNI152b as this requires at least 12GB RAM
  if( isTRUE("mni_icbm152_nlin_asym_09b" %in% normalize_template) ) {
    message(
      "It seems your computer memory is ", max_mem_size, "GB. You need at least 16GB to normalize to MNI152b template. ",
      "If you are sure that this is a false positive, please set via\n\traveio::raveio_setopt('max_mem', <your actual RAM in GB>)\n\n",
      "Using `MNI152a` instead..."
    )
    normalize_template <- c("mni_icbm152_nlin_asym_09a", normalize_template[normalize_template != "mni_icbm152_nlin_asym_09b"])
    normalize_template <- unique(normalize_template)
  }
}
if(!length(normalize_template)) {
  normalize_template <- NULL
}

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
    pial_path <- file.path(surf_path, "lh.pial")
    envelope_path <- file.path(surf_path, "lh.pial-outer-smoothed")
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
    pial_path <- file.path(surf_path, "rh.pial")
    envelope_path <- file.path(surf_path, "rh.pial-outer-smoothed")
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

  # Move the RAVE folder
  ants_ravedir <-  file.path(wdir_actual, "rave-imaging", "ants", "RAVE")
  fs_ravedir <-  file.path(wdir_actual, "rave-imaging", "fs", "RAVE")
  if(dir.exists(ants_ravedir)) {
    ants_ravefiles <- list.files(ants_ravedir, recursive = TRUE, full.names = FALSE, include.dirs = FALSE, all.files = FALSE)
    lapply(ants_ravefiles, function(file_relpath) {
      src_path <- file.path(ants_ravedir, file_relpath)
      dst_path <- file.path(fs_ravedir, file_relpath)
      if(!file.exists(dst_path)) {
        raveio::dir_create2(dirname(dst_path))
        file.copy(src_path, dst_path, overwrite = TRUE)
      }
    })
  }

  surf_path <- file.path(wdir_actual, "rave-imaging", "fs", "surf")
  if(dir.exists(surf_path)) {
    message("Finalizing 1 of 2...")
    pial_path <- file.path(surf_path, "lh.pial")
    envelope_path <- file.path(surf_path, "lh.pial-outer-smoothed")
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
    pial_path <- file.path(surf_path, "rh.pial")
    envelope_path <- file.path(surf_path, "rh.pial-outer-smoothed")
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

