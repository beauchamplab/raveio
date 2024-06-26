% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-YAEL.R
\name{as_yael_process}
\alias{as_yael_process}
\title{Create a 'YAEL' imaging processing instance}
\usage{
as_yael_process(subject)
}
\arguments{
\item{subject}{character (subject code, or project name with subject code),
or \code{\link{RAVESubject}} instance.}
}
\value{
A processing instance, see \code{\link{YAELProcess}}
}
\description{
Image registration across different modals. Normalize brain 'T1'-weighted
'MRI' to template brain and generate subject-level atlas files.
}
\examples{

library(raveio)
process <- as_yael_process("testtest2")

# This example requires extra demo data & settings.
\dontrun{

# Import and set original T1w MRI and CT
process$set_input_image("/path/to/T1w_MRI.nii", type = "T1w")
process$set_input_image("/path/to/CT.nii.gz", type = "CT")

# Co-register CT to MRI
process$register_to_T1w(image_type = "CT")

# Morph T1w MRI to 0.5 mm^3 MNI152 template
process$map_to_template(
  template_name = "mni_icbm152_nlin_asym_09b",
  native_type = "T1w"
)

}


}
