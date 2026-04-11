# Register 'CT' to 'MR' images via `'NiftyReg'`

Supports 'Rigid', 'affine', or 'non-linear' transformation

## Usage

``` r
niftyreg_coreg(
  ct_path,
  mri_path,
  coreg_path = NULL,
  reg_type = c("rigid", "affine", "nonlinear"),
  interp = c("trilinear", "cubic", "nearest"),
  verbose = TRUE,
  ...
)

cmd_run_niftyreg_coreg(
  subject,
  ct_path,
  mri_path,
  reg_type = c("rigid", "affine", "nonlinear"),
  interp = c("trilinear", "cubic", "nearest"),
  verbose = TRUE,
  dry_run = FALSE,
  ...
)
```

## Arguments

- ct_path, mri_path:

  absolute paths to 'CT' and 'MR' image files

- coreg_path:

  registration path, where to save results; default is the parent folder
  of `ct_path`

- reg_type:

  registration type, choices are `'rigid'`, `'affine'`, or `'nonlinear'`

- interp:

  how to interpolate when sampling volumes, choices are `'trilinear'`,
  `'cubic'`, or `'nearest'`

- verbose:

  whether to verbose command; default is true

- ...:

  other arguments passed to
  [`register_volume`](https://dipterix.org/ravetools/reference/register_volume.html)

- subject:

  'RAVE' subject

- dry_run:

  whether to dry-run the script and to print out the command instead of
  executing the code; default is false

## Value

Nothing is returned from the function. However, several files will be
generated at the 'CT' path:

- `'ct_in_t1.nii'`:

  aligned 'CT' image; the image is also re-sampled into 'MRI' space

- `'CT_IJK_to_MR_RAS.txt'`:

  transform matrix from volume 'IJK' space in the original 'CT' to the
  'RAS' anatomical coordinate in 'MR' scanner

- `'CT_RAS_to_MR_RAS.txt'`:

  transform matrix from scanner 'RAS' space in the original 'CT' to
  'RAS' in 'MR' scanner space
