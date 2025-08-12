# DIPSAUS DEBUG START
# subject_code <- "testtest2"
# ct_path = '/Users/dipterix/rave_data/raw_dir/testtest/rave-imaging_old/sub-AnonSEEG_ses-postop_desc-preproc_CT.nii'
# mr_path = '/Users/dipterix/rave_data/raw_dir/testtest/rave-imaging_old/sub-AnonSEEG_ses-preop_desc-preproc_acq-ax_T1w.nii'
# pipelines = c("coregistration", "normalization")
# templates = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b", "mni_icbm152_nlin_asym_09c")
# template_path = "/Users/dipterix/rave_data/others/three_brain/templates/mni_icbm152_nlin_asym_09b/T1.nii.gz"
# self <- YAELProcess$new(subject_code)
# private <- self$.__enclos_env__$private
# verbose=T
# native_type = 'T1w'
# template_name = c("mni_icbm152_nlin_asym_09a"); template_name2 <- camel_template_name(template_name)
# native_ras = rnorm(90)
NULL

# self$set_input_image(mr_path, "T1w")
# self$set_input_image(ct_path, "CT")

# dipsaus::rs_exec({
#   self <- raveio:::YAELProcess$new(subject_code)
#   print(self)
#   self$register_to_T1w("CT", reverse = TRUE, verbose = TRUE)
# })
# dipsaus::rs_exec({
#   devtools::load_all("/Users/dipterix/Dropbox (Personal)/projects/raveio")
#   subject_code <- "testtest3"
#   self <- raveio:::YAELProcess$new(subject_code)
#   print(self)
#   self$map_to_template(template_name = "mni_icbm152_nlin_asym_09a")
# })

call_rpyants <- function(.name, ...) {
  rpyANTs <- asNamespace("rpyANTs")
  re <- rpyANTs[[.name]]
  if(is.function(re)) {
    re <- re(...)
  }
  return(re)
}

rpyants_builtin_templates <- function() {
  call_rpyants('BUILTIN_TEMPLATES')
}



