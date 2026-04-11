# Definition for 'RAVE' project class

Definition for 'RAVE' project class

Definition for 'RAVE' project class

## Value

character vector

true or false whether subject is in the project

A data table of pipeline time-stamps and directories

## Active bindings

- `path`:

  project folder, absolute path

- `name`:

  project name, character

- `pipeline_path`:

  path to pipeline scripts under project's folder

## Methods

### Public methods

- [`RAVEProject$print()`](#method-RAVEProject-print)

- [`RAVEProject$new()`](#method-RAVEProject-new)

- [`RAVEProject$subjects()`](#method-RAVEProject-subjects)

- [`RAVEProject$has_subject()`](#method-RAVEProject-has_subject)

- [`RAVEProject$group_path()`](#method-RAVEProject-group_path)

- [`RAVEProject$subject_pipelines()`](#method-RAVEProject-subject_pipelines)

- [`RAVEProject$clone()`](#method-RAVEProject-clone)

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

override print method

#### Usage

    RAVEProject$print(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVEProject$new(project_name, strict = TRUE)

#### Arguments

- `project_name`:

  character

- `strict`:

  whether to check project path

------------------------------------------------------------------------

### Method `subjects()`

get all imported subjects within project

#### Usage

    RAVEProject$subjects()

------------------------------------------------------------------------

### Method `has_subject()`

whether a specific subject exists in this project

#### Usage

    RAVEProject$has_subject(subject_code)

#### Arguments

- `subject_code`:

  character, subject name

------------------------------------------------------------------------

### Method `group_path()`

get group data path for 'rave' module

#### Usage

    RAVEProject$group_path(module_id, must_work = FALSE)

#### Arguments

- `module_id`:

  character, 'rave' module ID

- `must_work`:

  whether the directory must exist; if not exists, should a new one be
  created?

------------------------------------------------------------------------

### Method `subject_pipelines()`

list saved pipelines

#### Usage

    RAVEProject$subject_pipelines(
      pipeline_name,
      cache = FALSE,
      check = TRUE,
      all = FALSE
    )

#### Arguments

- `pipeline_name`:

  name of the pipeline

- `cache`:

  whether to use cached registry

- `check`:

  whether to check if the pipelines exist as directories

- `all`:

  whether to list all pipelines; default is false; pipelines with the
  same label but older time-stamps will be hidden

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVEProject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
