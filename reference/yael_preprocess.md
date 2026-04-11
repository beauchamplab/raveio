# Process brain images for `'YAEL'`

Aligns `'T1w'` with other image types; normalizes `'T1w'` 'MRI' to
'MNI152' templates via symmetric non-linear morphs. Create brain custom
atlases from templates.

## Usage

``` r
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
```

## Arguments

- subject_code:

  'RAVE' subject code

- t1w_path:

  (required) 'T1' weighted 'MRI' path

- ct_path:

  (optional in general but mandatory for electrode localization)
  post-surgery 'CT' path

- t2w_path:

  (optional) 'T2' weighted 'MRI' path

- fgatir_path:

  (optional) 'fGATIR' (fast gray-matter acquisition 'T1' inversion
  recovery) image path

- preopct_path:

  (optional) pre-surgery 'CT' path

- flair_path:

  (optional) 'FLAIR' (fluid-attenuated inversion recovery) image path

- t1w_contrast_path:

  (optional) 'T1' weighted 'MRI' with contrast (usually used to show the
  blood vessels)

- register_reversed:

  direction of the registration; `FALSE` (default) registers other
  images (such as post-surgery 'CT' to 'T1'); set to `FALSE` if you
  would like the `'T1'` to be registered into other images. Since 'YAEL'
  does not re-sample the images, there is no essential difference on the
  final registration results

- normalize_template:

  names of the templates which the native 'T1' images will be normalized
  into

- run_recon_all:

  whether to run `'FreeSurfer'` reconstruction; default is true

- dry_run:

  whether to dry-run the script and check if error exists before
  actually execute the scripts.

- verbose:

  whether to print out the progress; default is `TRUE`

- register_policy:

  whether images should be registered with `'T1w'` image; default is
  `"auto"`: automatically run registration algorithm if missing;
  alternative is `"all"`: force the registration algorithm even if
  mapping files exist

- normalize_policy:

  normalization policy; similar to `register_policy` but is applied to
  normalization. Default is `"auto"`: automatically run normalization
  when the mapping is missing, and skip if exists; alternative is
  `"all"`: force to run the normalization.

- normalize_back:

  length of one (select from `normalize_template`), which template is to
  be used to generate native brain mask and transform matrices

- atlases:

  a named list: the names must be template names from
  `normalize_template` and the values must be directories of atlases of
  the corresponding templates (see 'Examples').

- add_surfaces:

  Whether to add surfaces for the subject; default is `FALSE`. The
  surfaces are created by reversing the normalization from template
  brain, hence the results will not be accurate. Enable this option only
  if cortical surface estimation is not critical.

## Value

Nothing, a subject imaging folder will be created under 'RAVE' raw
folder

## Examples

``` r
if (FALSE) { # \dontrun{


# For T1 preprocessing only
yael_preprocess(
  subject_code = "patient01",
  t1w_path = "/path/to/T1.nii or T1.nii.gz",

  # normalize T1 to all 2009 MNI152-Asym brains (a,b,c)
  normalize_template = c(
    "mni_icbm152_nlin_asym_09b"
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


} # }
```
