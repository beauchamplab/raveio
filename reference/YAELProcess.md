# Class definition of `'YAEL'` image pipeline

Rigid-registration across multiple types of images, non-linear
normalization from native brain to common templates, and map template
atlas or 'ROI' back to native brain. See examples at
[`as_yael_process`](as_yael_process.md)

## Value

whether the image has been set (or replaced)

Absolute path if the image

'RAVE' subject instance

Nothing

A list of moving and fixing images, with rigid transformations from
different formats.

See method `get_template_mapping`

A list of input, output images, with forward and inverse transform files
(usually two `'Affine'` with one displacement field)

transformed image in `'ANTs'` format

transformed image in `'ANTs'` format

Nothing

A matrix of 3 columns, each row is a transformed points ( invalid rows
will be filled with `NA`)

A matrix of 3 columns, each row is a transformed points ( invalid rows
will be filled with `NA`)

## Active bindings

- `subject_code`:

  'RAVE' subject code

- `image_types`:

  allowed image types

- `work_path`:

  Working directory ('RAVE' imaging path)

## Methods

### Public methods

- [`YAELProcess$new()`](#method-YAELProcess-new)

- [`YAELProcess$set_input_image()`](#method-YAELProcess-set_input_image)

- [`YAELProcess$get_input_image()`](#method-YAELProcess-get_input_image)

- [`YAELProcess$get_subject()`](#method-YAELProcess-get_subject)

- [`YAELProcess$register_to_T1w()`](#method-YAELProcess-register_to_T1w)

- [`YAELProcess$get_native_mapping()`](#method-YAELProcess-get_native_mapping)

- [`YAELProcess$map_to_template()`](#method-YAELProcess-map_to_template)

- [`YAELProcess$get_template_mapping()`](#method-YAELProcess-get_template_mapping)

- [`YAELProcess$transform_image_from_template()`](#method-YAELProcess-transform_image_from_template)

- [`YAELProcess$transform_image_to_template()`](#method-YAELProcess-transform_image_to_template)

- [`YAELProcess$generate_atlas_from_template()`](#method-YAELProcess-generate_atlas_from_template)

- [`YAELProcess$transform_points_to_template()`](#method-YAELProcess-transform_points_to_template)

- [`YAELProcess$transform_points_from_template()`](#method-YAELProcess-transform_points_from_template)

- [`YAELProcess$construct_ants_folder_from_template()`](#method-YAELProcess-construct_ants_folder_from_template)

- [`YAELProcess$get_brain()`](#method-YAELProcess-get_brain)

- [`YAELProcess$clone()`](#method-YAELProcess-clone)

------------------------------------------------------------------------

### Method `new()`

Constructor to instantiate the class

#### Usage

    YAELProcess$new(subject_code, image_types)

#### Arguments

- `subject_code`:

  character code representing the subject

- `image_types`:

  vector of image types, such as `'T1w'`, `'CT'`, `'fGATIR'`. All images
  except `'CT'` will be considered `'preop'` (before electrode
  implantation). Please use `'postop'` to indicate if an image is taken
  after the implantation (for example, `'postopT1w'`)

------------------------------------------------------------------------

### Method `set_input_image()`

Set the raw input for different image types

#### Usage

    YAELProcess$set_input_image(
      path,
      type = YAEL_IMAGE_TYPES,
      overwrite = FALSE,
      on_error = c("warning", "error", "ignore")
    )

#### Arguments

- `path`:

  path to the image files in `'NIfTI'` format

- `type`:

  type of the image

- `overwrite`:

  whether to overwrite existing images if the same type has been
  imported before; default is false

- `on_error`:

  when the file exists and `overwrite` is false, how should this error
  be reported; choices are `'warning'` (default), `'error'` (throw error
  and abort), or `'ignore'`.

------------------------------------------------------------------------

### Method `get_input_image()`

Get image path

#### Usage

    YAELProcess$get_input_image(type = YAEL_IMAGE_TYPES)

#### Arguments

- `type`:

  type of the image

------------------------------------------------------------------------

### Method `get_subject()`

Get 'RAVE' subject instance

#### Usage

    YAELProcess$get_subject(project_name = "YAEL", strict = FALSE)

#### Arguments

- `project_name`:

  project name; default is `'YAEL'`

- `strict`:

  passed to [`as_rave_subject`](as_rave_subject.md)

------------------------------------------------------------------------

### Method `register_to_T1w()`

Register other images to 'T1' weighted 'MRI'

#### Usage

    YAELProcess$register_to_T1w(image_type = "CT", reverse = FALSE, verbose = TRUE)

#### Arguments

- `image_type`:

  type of the image to register, must be set via
  `process$set_input_image` first.

- `reverse`:

  whether to reverse the registration; default is false, meaning the
  fixed (reference) image is the `'T1'`. When setting to true, then the
  `'T1'` 'MRI' will become the moving image

- `verbose`:

  whether to print out the process; default is true

------------------------------------------------------------------------

### Method `get_native_mapping()`

Get the mapping configurations used by `register_to_T1w`

#### Usage

    YAELProcess$get_native_mapping(image_type = YAEL_IMAGE_TYPES, relative = FALSE)

#### Arguments

- `image_type`:

  type of the image registered to 'T1' weighted 'MRI'

- `relative`:

  whether to use relative path (to the `work_path` field)

------------------------------------------------------------------------

### Method `map_to_template()`

Normalize native brain to `'MNI152'` template

#### Usage

    YAELProcess$map_to_template(
      template_name = rpyants_builtin_templates(),
      use_images = c("T1w", "T2w", "T1wContrast", "fGATIR", "preopCT"),
      native_type = "T1w",
      verbose = TRUE,
      ...
    )

#### Arguments

- `template_name`:

  which template to use, choices are `'mni_icbm152_nlin_asym_09a'`,
  `'mni_icbm152_nlin_asym_09b'`, `'mni_icbm152_nlin_asym_09c'`, and
  `'fsaverage'`.

- `use_images`:

  a vector of image types to use for normalization; default types are
  `'T1w'`, `'T2w'`, `'T1wContrast'`, `'fGATIR'`, and `'preopCT'`. To use
  all available images for normalization, use wildcard `"all"`

- `native_type`:

  which type of image should be used to map to template; default is
  `'T1w'`

- `verbose`:

  whether to print out the process; default is true

- `...`:

  additional tuning parameters passed to internal 'Python' code.

------------------------------------------------------------------------

### Method `get_template_mapping()`

Get configurations used for normalization

#### Usage

    YAELProcess$get_template_mapping(
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      relative = FALSE
    )

#### Arguments

- `template_name`:

  which template is used

- `native_type`:

  which native image is mapped to template

- `relative`:

  whether the paths should be relative or absolute; default is false
  (absolute paths)

------------------------------------------------------------------------

### Method `transform_image_from_template()`

Apply transform from images (usually an atlas or 'ROI') on template to
native space

#### Usage

    YAELProcess$transform_image_from_template(
      template_roi_path,
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      interpolator = c("auto", "nearestNeighbor", "linear", "gaussian", "bSpline",
        "cosineWindowedSinc", "welchWindowedSinc", "hammingWindowedSinc",
        "lanczosWindowedSinc", "genericLabel"),
      verbose = TRUE
    )

#### Arguments

- `template_roi_path`:

  path to the template image file which will be transformed into
  individuals' image

- `template_name`:

  templates to use

- `native_type`:

  which type of native image to use for calculating the coordinates
  (default `'T1w'`)

- `interpolator`:

  how to interpolate the `'voxels'`; default is `"auto"`: `'linear'` for
  probabilistic map and `'nearestNeighbor'` otherwise.

- `verbose`:

  whether the print out the progress

------------------------------------------------------------------------

### Method `transform_image_to_template()`

Apply transform to images (usually an atlas or 'ROI') from native space
to template

#### Usage

    YAELProcess$transform_image_to_template(
      native_roi_path,
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      interpolator = c("auto", "nearestNeighbor", "linear", "gaussian", "bSpline",
        "cosineWindowedSinc", "welchWindowedSinc", "hammingWindowedSinc",
        "lanczosWindowedSinc", "genericLabel"),
      verbose = TRUE
    )

#### Arguments

- `native_roi_path`:

  path to the native image file that will be transformed into template

- `template_name`:

  templates to use

- `native_type`:

  which type of native image to use for calculating the coordinates
  (default `'T1w'`)

- `interpolator`:

  how to interpolate the `'voxels'`; default is `"auto"`: `'linear'` for
  probabilistic map and `'nearestNeighbor'` otherwise.

- `verbose`:

  whether the print out the progress

------------------------------------------------------------------------

### Method `generate_atlas_from_template()`

Generate atlas maps from template and morph to native brain

#### Usage

    YAELProcess$generate_atlas_from_template(
      template_name = rpyants_builtin_templates(),
      atlas_folder = NULL,
      surfaces = NA,
      verbose = TRUE
    )

#### Arguments

- `template_name`:

  which template to use

- `atlas_folder`:

  path to the atlas folder (that contains the atlas files)

- `surfaces`:

  whether to generate surfaces (triangle mesh); default is `NA`
  (generate if not existed). Other choices are `TRUE` for always
  generating and overwriting surface files, or `FALSE` to disable this
  function. The generated surfaces will stay in native `'T1'` space.

- `verbose`:

  whether the print out the progress

------------------------------------------------------------------------

### Method `transform_points_to_template()`

Transform points from native images to template

#### Usage

    YAELProcess$transform_points_to_template(
      native_ras,
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      verbose = TRUE
    )

#### Arguments

- `native_ras`:

  matrix or data frame with 3 columns indicating points sitting on
  native images in right-anterior-superior (`'RAS'`) coordinate system.

- `template_name`:

  template to use for mapping

- `native_type`:

  native image type where the points sit on

- `verbose`:

  whether the print out the progress

------------------------------------------------------------------------

### Method `transform_points_from_template()`

Transform points from template images to native

#### Usage

    YAELProcess$transform_points_from_template(
      template_ras,
      template_name = rpyants_builtin_templates(),
      native_type = "T1w",
      verbose = TRUE
    )

#### Arguments

- `template_ras`:

  matrix or data frame with 3 columns indicating points sitting on
  template images in right-anterior-superior (`'RAS'`) coordinate
  system.

- `template_name`:

  template to use for mapping

- `native_type`:

  native image type where the points sit on

- `verbose`:

  whether the print out the progress

------------------------------------------------------------------------

### Method `construct_ants_folder_from_template()`

Create a reconstruction folder (as an alternative option) that is
generated from template brain to facilitate the '3D' viewer. Please make
sure method `map_to_template` is called before using this method (or the
program will fail)

#### Usage

    YAELProcess$construct_ants_folder_from_template(
      template_name = rpyants_builtin_templates(),
      add_surfaces = TRUE
    )

#### Arguments

- `template_name`:

  template to use for mapping

- `add_surfaces`:

  whether to create surfaces that is morphed from template to local;
  default is `TRUE`. Please enable this option only if the cortical
  surfaces are not critical (for example, you are studying the deep
  brain structures). Always use `'FreeSurfer'` if cortical information
  is used.

------------------------------------------------------------------------

### Method `get_brain()`

Get '3D' brain model

#### Usage

    YAELProcess$get_brain(
      electrodes = TRUE,
      project_name = "YAEL",
      coord_sys = c("scannerRAS", "tkrRAS", "MNI152", "MNI305"),
      ...
    )

#### Arguments

- `electrodes`:

  whether to add electrodes to the viewers; can be logical, data frame,
  or a character (path to electrode table). When the value is `TRUE`,
  the electrode file under `project_name` will be loaded; when
  `electrodes` is a
  [`data.frame`](https://rdrr.io/r/base/data.frame.html), or path to a
  `'csv'` file, then please specify `coord_sys` on what is the
  coordinate system used for columns `"x"`, `"y"`, and `"z"`.

- `project_name`:

  project name under which the electrode table should be queried, if
  `electrodes=TRUE`

- `coord_sys`:

  coordinate system if `electrodes` is a data frame with columns `"x"`,
  `"y"`, and `"z"`, available choices are `'scannerRAS'` (defined by
  'T1' weighted native 'MRI' image), `'tkrRAS'` (`'FreeSurfer'` defined
  native 'TK-registered'), `'MNI152'` (template 'MNI' coordinate system
  averaged over 152 subjects; this is the common "'MNI' coordinate
  space" we often refer to), and `'MNI305'` (template 'MNI' coordinate
  system averaged over 305 subjects; this coordinate system used by
  templates such as `'fsaverage'`)

- `...`:

  passed to
  [`threeBrain`](https://dipterix.org/threeBrain/reference/threeBrain.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    YAELProcess$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
