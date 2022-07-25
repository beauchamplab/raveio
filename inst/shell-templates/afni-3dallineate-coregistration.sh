#!/usr/bin/env bash

set -e

AFNI_HOME="{{ paste(afni_path, collapse='') }}"
DYLD_PATH="/opt/X11/lib/flat_namespace"

#~/rave_data/raw_dir/YCQ/rave-imaging/coregistration
# t1="../../inputs/MRI/YCQ_MRI.nii"
# ct="../../inputs/CT/YCQ_CT.nii"
workdir="{{ workdir }}"
t1="{{ mri_path }}"
ct="{{ ct_path }}"

# Load AFNI
if [[ ! -z "$AFNI_HOME" ]]; then
  export PATH="$PATH:$AFNI_HOME"
fi

if [[ -d "$DYLD_PATH" ]]; then
  export DYLD_LIBRARY_PATH="${DYLD_LIBRARY_PATH}:$DYLD_PATH"
fi

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

# Prepare working directory
echo "Set working directory: $workdir/afni_3dAllineate" >> "$log_file"
mkdir -p "$workdir/afni_3dAllineate" && cd "$workdir/afni_3dAllineate"
cp -f "$t1" T1.nii
cp -f "$ct" CT.nii

@Align_Centers -base T1.nii -dset CT.nii 2>&1 | tee -a "$log_file"
3dresample -input CT_shft.nii -prefix CT_RAI_res_shft.nii  -master T1.nii  -dxyz 1 1 1 -rmode NN 2>&1 | tee -a "$log_file"

# Check if python is available. On my OSX M1, python is missing and python3 is available
python_bin=$(which python | which python2 | which python3)
"$python_bin" "$(which align_epi_anat.py)" -dset1 T1.nii -dset2  CT_RAI_res_shft.nii -dset1_strip None -dset2_strip None -dset2to1 -suffix _al -feature_size 1 {{ ifelse(overwrite, "-overwrite ", "") }}-cost nmi -giant_move -rigid_body 2>&1 | tee -a "$log_file"

3dcopy  CT_RAI_res_shft_al+orig CT_RAI_res_shft_al.nii 2>&1 | tee -a "$log_file"

cp -f CT_RAI_res_shft_al.nii "$workdir/ct_in_t1.nii"

echo "Done." | tee -a "$log_file"

## END OF RAVE Script: AFNI-3dAllineate coregistration ##
