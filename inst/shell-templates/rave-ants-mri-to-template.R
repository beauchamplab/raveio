#!/usr/bin/env Rscript --no-save --no-restore

# check if rpyANTs is configured
if( !isTRUE(rpyANTs::ants_available()) ) {
  rpyANTs::install_ants()
}

subject <- raveio::as_rave_subject("{{ subject$subject_id }}", strict = FALSE)
template_subject <- "{{ template_subject }}"
verbose <- TRUE

raveio::ants_mri_to_template(subject = subject, template_subject = template_subject, preview = FALSE, verbose = TRUE)

## END OF RAVE Script: morph MRI to template via rpyANTs (essentially ANTs)
