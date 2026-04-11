# Register 'CT' to 'MR' images via `'nipy'` script

Align 'CT' using `nipy.algorithms.registration.histogram_registration`.

## Usage

``` r
py_nipy_coreg(
  ct_path,
  mri_path,
  clean_source = TRUE,
  inverse_target = TRUE,
  precenter_source = TRUE,
  smooth = 0,
  reg_type = c("rigid", "affine"),
  interp = c("pv", "tri"),
  similarity = c("crl1", "cc", "cr", "mi", "nmi", "slr"),
  optimizer = c("powell", "steepest", "cg", "bfgs", "simplex"),
  tol = 1e-04,
  dry_run = FALSE
)

cmd_run_nipy_coreg(
  subject,
  ct_path,
  mri_path,
  clean_source = TRUE,
  inverse_target = TRUE,
  precenter_source = TRUE,
  reg_type = c("rigid", "affine"),
  interp = c("pv", "tri"),
  similarity = c("crl1", "cc", "cr", "mi", "nmi", "slr"),
  optimizer = c("powell", "steepest", "cg", "bfgs", "simplex"),
  dry_run = FALSE,
  verbose = FALSE
)
```

## Arguments

- ct_path, mri_path:

  absolute paths to 'CT' and 'MR' image files

- clean_source:

  whether to replace negative 'CT' values with zeros; default is true

- inverse_target:

  whether to inverse 'MRI' color intensity; default is true

- precenter_source:

  whether to adjust the 'CT' transform matrix before alignment, such
  that the origin of 'CT' is at the center of the volume; default is
  true. This option may avoid the case that 'CT' is too far-away from
  the 'MR' volume at the beginning of the optimization

- smooth, interp, optimizer, tol:

  optimization parameters, see `'nipy'` documentation for details.

- reg_type:

  registration type, choices are `'rigid'` or `'affine'`

- similarity:

  the cost function of the alignment; choices are `'crl1'` ('L1'
  regularized correlation), `'cc'` (correlation coefficient), `'cr'`
  (correlation), `'mi'` (mutual information), `'nmi'` (normalized mutual
  information), `'slr'` (likelihood ratio). In reality I personally find
  `'crl1'` works best in most cases, though many tutorials suggest
  `'nmi'`.

- dry_run:

  whether to dry-run the script and to print out the command instead of
  executing the code; default is false

- subject:

  'RAVE' subject

- verbose:

  whether to verbose command; default is false

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
