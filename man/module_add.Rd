% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-create.R
\name{module_add}
\alias{module_add}
\title{Add new 'RAVE' (2.0) module to current project}
\usage{
module_add(
  module_id,
  module_label,
  path = ".",
  type = c("default", "bare", "scheduler", "python"),
  ...,
  pipeline_name = module_id,
  overwrite = FALSE
)
}
\arguments{
\item{module_id}{module ID to create, must be unique}

\item{module_label}{a friendly label to display in the dashboard}

\item{path}{project root path; default is current directory}

\item{type}{template to choose, options are \code{'default'} and
\code{'bare'}}

\item{...}{additional configurations to the module such as \code{'order'},
\code{'group'}, \code{'badge'}}

\item{pipeline_name}{the pipeline name to create along with the module;
default is identical to \code{module_id}}

\item{overwrite}{whether to overwrite existing module if module with
same ID exists; default is false}
}
\value{
Nothing.
}
\description{
Add new 'RAVE' (2.0) module to current project
}
