# Load 'FreeSurfer' or 'AFNI/SUMA' brain from 'RAVE'

Create 3D visualization of the brain and visualize with modern web
browsers

## Usage

``` r
rave_brain(
  subject,
  surfaces = "pial",
  overlays = "aparc.a2009s+aseg",
  annotations = "label/aparc.a2009s",
  ...,
  usetemplateifmissing = FALSE,
  include_electrodes = TRUE
)
```

## Arguments

- subject:

  character, list, or [`RAVESubject`](RAVESubject.md) instance; for list
  or other objects, make sure `subject$subject_id` is a valid 'RAVE'
  subject 'ID'

- surfaces:

  one or more brain surface types from `"pial"`, `"white"`,
  `"smoothwm"`, `"pial-outer-smoothed"`, etc.; check
  [`threeBrain`](https://dipterix.org/threeBrain/reference/threeBrain.html)

- overlays:

  volumes to overlay; default is `'aparc.a2009s+aseg'`

- annotations:

  surface annotation or curvature data to load; default is
  `'label/aparc.a2009s'`, referring to the `'*h.aparc.a2009s.annot'`
  under the label folder.

- ...:

  ignored, reserved for legacy code

- usetemplateifmissing:

  whether to use template brain when the subject brain files are
  missing. If set to true, then a template (usually 'N27') brain will be
  displayed as an alternative solution, and electrodes will be rendered
  according to their `'MNI305'` coordinates, or `'VertexNumber'` if
  given.

- include_electrodes:

  whether to include electrode in the model; default is true

## Value

A `'threeBrain'` instance if brain is found or `usetemplateifmissing` is
set to true; otherwise returns `NULL`

## Examples

``` r

# Please make sure DemoSubject is correctly installed
# The subject is ~1GB from Github

if(interactive()){
  brain <- rave_brain("demo/DemoSubject")

  if( !is.null(brain) ) { brain$plot() }

}

```
