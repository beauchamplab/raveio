# Register 'CT' or 'MR' images via `'ANTs'`

`ants_coreg` aligns 'CT' to 'MR' images; `ants_mri_to_template` aligns
native 'MR' images to group templates

## Usage

``` r
ants_coreg(
  ct_path,
  mri_path,
  coreg_path = NULL,
  reg_type = c("DenseRigid", "Rigid", "SyN", "Affine", "TRSAA", "SyNCC", "SyNOnly"),
  aff_metric = c("mattes", "meansquares", "GC"),
  syn_metric = c("mattes", "meansquares", "demons", "CC"),
  verbose = TRUE,
  ...
)

cmd_run_ants_coreg(
  subject,
  ct_path,
  mri_path,
  reg_type = c("DenseRigid", "Rigid", "SyN", "Affine", "TRSAA", "SyNCC", "SyNOnly"),
  aff_metric = c("mattes", "meansquares", "GC"),
  syn_metric = c("mattes", "meansquares", "demons", "CC"),
  verbose = TRUE,
  dry_run = FALSE
)

ants_mri_to_template(
  subject,
  template_subject = getOption("threeBrain.template_subject", "N27"),
  preview = FALSE,
  verbose = TRUE,
  ...
)

cmd_run_ants_mri_to_template(
  subject,
  template_subject = getOption("threeBrain.template_subject", "N27"),
  verbose = TRUE,
  dry_run = FALSE
)

ants_morph_electrode(subject, preview = FALSE, dry_run = FALSE)
```

## Arguments

- ct_path, mri_path:

  absolute paths to 'CT' and 'MR' image files

- coreg_path:

  registration path, where to save results; default is the parent folder
  of `ct_path`

- reg_type:

  registration type, choices are `'DenseRigid'`, `'Rigid'`, `'Affine'`,
  `'SyN'`, `'TRSAA'`, `'SyNCC'`, `'SyNOnly'`, or other types; see
  [`ants_registration`](http://dipterix.org/rpyANTs/reference/ants_registration.md)

- aff_metric:

  cost function to use for linear or 'affine' transform

- syn_metric:

  cost function to use for `'SyN'` transform

- verbose:

  whether to verbose command; default is true

- ...:

  other arguments passed to
  [`ants_registration`](http://dipterix.org/rpyANTs/reference/ants_registration.md)

- subject:

  'RAVE' subject

- dry_run:

  whether to dry-run the script and to print out the command instead of
  executing the code; default is false

- template_subject:

  template to map 'MR' images

- preview:

  whether to preview results; default is false

## Value

Aligned 'CT' will be generated at the `coreg_path` path:

- `'ct_in_t1.nii.gz'`:

  aligned 'CT' image; the image is also re-sampled into 'MRI' space

- `'transform.yaml'`:

  transform settings and outputs

- `'CT_IJK_to_MR_RAS.txt'`:

  transform matrix from volume 'IJK' space in the original 'CT' to the
  'RAS' anatomical coordinate in 'MR' scanner; 'affine' transforms only

- `'CT_RAS_to_MR_RAS.txt'`:

  transform matrix from scanner 'RAS' space in the original 'CT' to
  'RAS' in 'MR' scanner space; 'affine' transforms only
