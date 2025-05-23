% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmd-recon-all.R, R/image-preprocess.R
\name{cmd_run_yael_preprocess}
\alias{cmd_run_yael_preprocess}
\alias{yael_preprocess}
\title{Process brain images for \code{'YAEL'}}
\usage{
cmd_run_yael_preprocess(
  subject_code,
  t1w_path = NULL,
  ct_path = NULL,
  t2w_path = NULL,
  fgatir_path = NULL,
  preopct_path = NULL,
  flair_path = NULL,
  t1w_contrast_path = NULL,
  register_reversed = FALSE,
  normalize_template = "mni_icbm152_nlin_asym_09b",
  run_recon_all = TRUE,
  dry_run = FALSE,
  verbose = TRUE
)

yael_preprocess(
  subject_code,
  t1w_path = NULL,
  ct_path = NULL,
  t2w_path = NULL,
  fgatir_path = NULL,
  preopct_path = NULL,
  flair_path = NULL,
  t1w_contrast_path = NULL,
  register_policy = c("auto", "all"),
  register_reversed = FALSE,
  normalize_template = "mni_icbm152_nlin_asym_09a",
  normalize_policy = c("auto", "all"),
  normalize_back = ifelse(length(normalize_template) >= 1, normalize_template[[1]], NA),
  atlases = list(),
  add_surfaces = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{subject_code}{'RAVE' subject code}

\item{t1w_path}{(required) 'T1' weighted 'MRI' path}

\item{ct_path}{(optional in general but mandatory for electrode localization)
post-surgery 'CT' path}

\item{t2w_path}{(optional) 'T2' weighted 'MRI' path}

\item{fgatir_path}{(optional) 'fGATIR' (fast gray-matter acquisition 'T1'
inversion recovery) image path}

\item{preopct_path}{(optional) pre-surgery 'CT' path}

\item{flair_path}{(optional) 'FLAIR' (fluid-attenuated inversion recovery)
image path}

\item{t1w_contrast_path}{(optional) 'T1' weighted 'MRI' with contrast
(usually used to show the blood vessels)}

\item{register_reversed}{direction of the registration; \code{FALSE}
(default) registers other images (such as post-surgery 'CT' to 'T1');
set to \code{FALSE} if you would like the \code{'T1'} to be registered into
other images. Since 'YAEL' does not re-sample the images, there is no
essential difference on the final registration results}

\item{normalize_template}{names of the templates which the native 'T1' images
will be normalized into}

\item{run_recon_all}{whether to run \code{'FreeSurfer'} reconstruction;
default is true}

\item{dry_run}{whether to dry-run the script and check if error exists before
actually execute the scripts.}

\item{verbose}{whether to print out the progress; default is \code{TRUE}}

\item{register_policy}{whether images should be registered with \code{'T1w'}
image; default is \code{"auto"}: automatically run registration algorithm
if missing; alternative is \code{"all"}: force the registration algorithm
even if mapping files exist}

\item{normalize_policy}{normalization policy; similar to
\code{register_policy} but is applied to normalization. Default is
\code{"auto"}: automatically run normalization when the mapping is missing,
and skip if exists; alternative is \code{"all"}: force to run the
normalization.}

\item{normalize_back}{length of one (select from \code{normalize_template}),
which template is to be used to generate native brain mask and transform
matrices}

\item{atlases}{a named list: the names must be template names from
\code{normalize_template} and the values must be directories of atlases of
the corresponding templates (see 'Examples').}

\item{add_surfaces}{Whether to add surfaces for the subject; default is
\code{FALSE}. The surfaces are created by reversing the normalization from
template brain, hence the results will not be accurate. Enable this option
only if cortical surface estimation is not critical.}
}
\value{
Nothing, a subject imaging folder will be created under 'RAVE'
raw folder
}
\description{
Aligns \code{'T1w'} with other image types; normalizes \code{'T1w'}
'MRI' to 'MNI152' templates via symmetric non-linear morphs. Create brain
custom atlases from templates.
}
\examples{

\dontrun{


# For T1 preprocessing only
yael_preprocess(
  subject_code = "patient01",
  t1w_path = "/path/to/T1.nii or T1.nii.gz",

  # normalize T1 to all 2009 MNI152-Asym brains (a,b,c)
  normalize_template = c(
    "mni_icbm152_nlin_asym_09a",
    "mni_icbm152_nlin_asym_09b",
    "mni_icbm152_nlin_asym_09c"
  ),

  # only normalize if not exists
  normalize_policy = "auto",

  # use MNI152b to create native processing folder
  normalize_back = "mni_icbm152_nlin_asym_09b",

  # Atlases generated from different templates have different
  # coordinates, hence both folder path and template names must be
  # provided
  atlases = list(
    mni_icbm152_nlin_asym_09b = "/path/to/atlas/folder1",
    mni_icbm152_nlin_asym_09c = "/path/to/atlas/folder2"
  )

)

# For T1 and postop CT coregistration only
yael_preprocess(
  subject_code = "patient01",
  t1w_path = "/path/to/T1.nii or T1.nii.gz",
  ct_path = "/path/to/CT.nii or CT.nii.gz",

  # No normalization
  normalize_template = NULL,
  normalize_back = NA

)

# For both T1 and postop CT coregistration and T1 normalization
yael_preprocess(
  subject_code = "patient01",
  t1w_path = "/path/to/T1.nii or T1.nii.gz",
  ct_path = "/path/to/CT.nii or CT.nii.gz",

  normalize_template = c(
    "mni_icbm152_nlin_asym_09a",
    "mni_icbm152_nlin_asym_09b",
    "mni_icbm152_nlin_asym_09c"
  ),

  normalize_policy = "auto",

  normalize_back = "mni_icbm152_nlin_asym_09b",

  atlases = list(
    mni_icbm152_nlin_asym_09b = "/path/to/atlas/folder1",
    mni_icbm152_nlin_asym_09c = "/path/to/atlas/folder2"
  )

)


}

}
