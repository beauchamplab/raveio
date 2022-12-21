#!/usr/bin/env bash

set -e

timestamp=$(date -u)

# Prepare log file
log_file="{{ log_file }}"
if [[ ! -z "$log_file" ]]; then
  log_path="{{ log_path }}"
  log_file="{{ log_path }}/$log_file"
  mkdir -p "$log_path" && touch "$log_file" && echo "Started: $timestamp" >> "$log_file"
  echo --------------------------------------------------------
  echo Log file: "$log_file"
  echo --------------------------------------------------------
else
  log_file="/dev/null"
fi

# Prepare working directory: FreeSurfer does not like it when
# directories contain spaces
wdir_actual="{{ work_path_actual }}"
wdir_fs="{{ work_path_symlink }}"
mri_path="{{ mri_path }}"
input_name="{{ gsub('.*\\.(nii|nii\\.gz)$', 'MRI.\\1', mri_path) }}"
mri_ext="{{ ifelse(grepl('gz$', mri_path, ignore.case = TRUE), '.nii.gz', '.nii') }}"
derivative_path="$wdir_actual/rave-imaging/derivative"
mri_backup="$wdir_actual/rave-imaging/derivative/MRI_RAW$mri_ext"

# Make sure the actual working directory exists
fresh_start={{ ifelse(overwrite, 1, 0) }}
if [[ "$fresh_start" -eq 1 && -d "$wdir_actual/rave-imaging/fs" ]]; then
  rm -r "$wdir_actual/rave-imaging/fs"
fi
mkdir -p "$wdir_actual/rave-imaging/derivative"

# Back up original MRI file to derivative folder
cp -f "$mri_path" "$mri_backup"

touch "$derivative_path/conf-reconstruction.yaml"
echo "Heads up: Do NOT edit this file
profile: FreeSurfer Re-construction
work_path: \"$wdir_actual/rave-imaging\"
timestamp: \"$timestamp\"
command:
  execute: recon-all
  arguments: \"{{ paste(c(args, ""), collapse = " ") }}\"
input_image:
  type: nifti
  path: \"$mri_path\"
  backup: ./derivative/MRI_RAW$mri_ext
  comment: original MRI image file
outputs:
  fs_folder:
    type: directory
    path: ./fs/
    comment: FreeSurfer reconstruction
  normalized_image:
    type: nifti
    path: ./fs/mri/T1.nii
    backup: ./derivative/T1.nii
    comment: FreeSurfer normalized MRI image
  Norig:
    type: transform
    dimension: 4x4
    transform_from:
      volume: normalized_image
      coordinate_system: IJK
    transform_to:
      space: scanner
      coordinate_system: RAS
    path: ./derivative/transform-Norig.tsv
    comment: From IJK in normalized image to T1 MRI scanner RAS coordinate
  Torig:
    type: transform
    dimension: 4x4
    transform_from:
      volume: normalized_image
      coordinate_system: IJK
    transform_to:
      space: tkr
      coordinate_system: RAS
    path: ./derivative/transform-Torig.tsv
    comment: From IJK in normalized image to FreeSurfer surface RAS coordinate
" > "$derivative_path/conf-reconstruction.yaml"

# Make sure the symlink is done properly
if [[ -d "$wdir_fs" ]]; then
  rm -r "$wdir_fs"
fi
mkdir -p "$wdir_fs"
ln -s "$wdir_actual/rave-imaging" "$wdir_fs"

# Set FreeSurfer home directory & initialize
SUBJECTS_DIR="$wdir_fs/rave-imaging"
{{
  if(length(freesurfer_home) == 1 && !is.na(freesurfer_home) && file.exists(freesurfer_home)) {
    sprintf(
      'FREESURFER_HOME="%s"\nsource "$FREESURFER_HOME/SetUpFreeSurfer.sh"',
      freesurfer_home
    )
  } else { "" }
}}

if [ -d "$SUBJECTS_DIR/fs/mri" ]; then
  # Use existing FreeSurfer directory to continue analysis
  recon-all -sd "$SUBJECTS_DIR" -sid fs {{ paste(c(args, ""), collapse = " ") }}| tee -a "$log_file"
else
  # Copy Nifti input
  cp "$mri_path" "$wdir_fs/$input_name"
  if [ -d "$SUBJECTS_DIR/fs" ]; then
    rm -r "$SUBJECTS_DIR/fs"
  fi
  recon-all -sd "$SUBJECTS_DIR" -sid fs -i "$wdir_fs/$input_name" {{ paste(c(args, ""), collapse = " ") }}| tee -a "$log_file"
fi

# Convert T1.mgz to T1.nii so users can choose to use T1.nii to coregister
mri_convert --in_type mgz --out_type nii "$SUBJECTS_DIR/fs/mri/T1.mgz" "$SUBJECTS_DIR/fs/mri/T1.nii"

cp -f "$SUBJECTS_DIR/fs/mri/T1.nii" "$derivative_path/T1.nii"

# Generate transform matrices
mri_info --vox2ras "$derivative_path/T1.nii" > "$derivative_path/transform-Norig.tsv"
mri_info --vox2ras-tkr "$derivative_path/T1.nii" > "$derivative_path/transform-Torig.tsv"

# Removing temporary fs root
rm -r "$wdir_fs"

echo "Done." | tee -a "$log_file"

## END OF RAVE Script: FreeSurfer recon-all with flags: {{ paste(c(args, ""), collapse = " ") }} ##
