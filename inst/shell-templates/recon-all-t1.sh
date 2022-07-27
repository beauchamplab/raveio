#!/usr/bin/env bash

set -e

# Prepare log file
log_file="{{ log_file }}"
if [[ ! -z "$log_file" ]]; then
  log_path="{{ log_path }}"
  log_file="{{ log_path }}/$log_file"
  mkdir -p "$log_path" && touch "$log_file" && echo "Started: $(date -u)" >> "$log_file"
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

# Make sure the actual working directory exists
fresh_start={{ ifelse(overwrite, 1, 0) }}
if [[ "$fresh_start" -eq 1 && -d "$wdir_actual/rave-imaging/fs" ]]; then
  rm -r "$wdir_actual/rave-imaging/fs"
fi
mkdir -p "$wdir_actual/rave-imaging"

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

echo "Done." | tee -a "$log_file"

## END OF RAVE Script: FreeSurfer recon-all with flags: {{ paste(c(args, ""), collapse = " ") }} ##
