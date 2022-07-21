#!/usr/bin/env bash

set -e

# Set input T1 & CT files
mri_path="{{ mri_path }}"
ct_path="{{ ct_path }}"
dest_path="{{ dest_path }}"

if [[ -z "$mri_path" || -z "$ct_path" ]]; then
  echo "Cannot run FST-FLIRT: Invalid source path" >&2
  exit -1;
fi

# FSL setup
{{
  if(length(fsl_home) == 1 &&
      !is.na(fsl_home) &&
      isTRUE(file.exists(fsl_home))) {
    sprintf('export FSLDIR="%s"', fsl_home)
  } else {
    ''
  }
}}
source ${FSLDIR}/etc/fslconf/fsl.sh
PATH=${FSLDIR}/bin:${PATH}

# Prepare log file
log_file="{{ log_file }}"
if [[ ! -z "$log_file" ]]; then
  log_dir="{{ log_path }}"
  log_file="$log_dir/$log_file"
  mkdir -p "$log_dir" && touch "$log_file"
  echo '--------------------------------------------------------'
  echo "Log file: $log_file"
  echo '--------------------------------------------------------'
else
  log_file="/dev/null"
fi

mkdir -p "$dest_path"

# Run flirt
flirt -in "$ct_path" -ref "$mri_path" \
  -out "$dest_path/ct_in_t1.nii" -omat "$dest_path/ct2t1.mat" \
  -interp trilinear -cost mutualinfo -dof 6 -searchcost mutualinfo \
  -searchrx -180 180 -searchry -180 180 -searchrz -180 180 -v | tee -a "$log_file"

echo "Done." | tee -a "$log_file"

## END OF RAVE Script: FSL-flirt coregistration ##
