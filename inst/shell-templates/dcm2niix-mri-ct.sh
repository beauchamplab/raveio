#!/usr/bin/env bash

set -e

# This is your dcm2niix binary path
cmd_dcm2niix="{{ dcm2niix }}"

# MRI/CT settings
src_path="{{ paste(src_path, collapse='/') }}"
dest_path="{{ paste(dest_path, collapse='/') }}"

if [ -z "$src_path" ]; then
  echo "Cannot run dcm2niix: Invalid source path" >&2
  exit -1;
fi

# check if source imaging is already in NifTi format
is_nifti=1
if [[ -L "$src_path" && -d "$src_path" ]]; then
  echo "Source is a symlink to a directory"
  is_nifti=0
elif [[ -d "$src_path" ]]; then
  is_nifti=0
elif [[ ! "$src_path" =~ \.[nN][iI][iI]($|\.[gG][zZ]$) ]]; then
  echo "Cannot run dcm2niix: Invalid source path: not a directory nor a NifTi file" >&2
  exit -1;
fi

# Prepare log file
log_file="{{ log_file }}"
if [[ ! -z "$log_file" ]]; then
  log_dir="{{ log_path }}"
  mkdir -p "$log_dir" && touch "$log_dir/$log_file"
  echo '--------------------------------------------------------'
  echo "Log file: $log_dir/$log_file"
  echo '--------------------------------------------------------'
fi


# Convert/copy MRI
# dest_path must be a directory
[ -d "$dest_path" ] && rm -r "$dest_path"
mkdir -p "$dest_path"

if [[ $is_nifti -eq 0 ]]; then
  # Convert MRI (DICOM => Nifti)
  echo "Converting DICOM to NifTi:"
  if [[ ! -z "$log_file" ]]; then
    $cmd_dcm2niix{{ paste(c("", args), collapse = " ") }} -o "$dest_path" "$src_path" 2>&1 | tee -a "$log_dir/$log_file"
    echo "Done. Please check log file" | tee -a "$log_dir/$log_file"
  else
    $cmd_dcm2niix{{ paste(c("", args), collapse = " ") }} -o "$dest_path" "$src_path"
    echo "Done. Please check printed messages."
  fi
else
  # Direct copy NifTi files
  cp -r "$src_path" "$dest_path"
  echo "Done. Please check printed messages."
fi


## END OF RAVE Script: dcm2niix DICOM to NifTi, or copy existing NifTi file ##
