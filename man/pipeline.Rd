% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-pipeline_tools.R
\name{pipeline}
\alias{pipeline}
\alias{pipeline_from_path}
\title{Creates 'RAVE' pipeline instance}
\usage{
pipeline(
  pipeline_name,
  settings_file = "settings.yaml",
  paths = pipeline_root(),
  temporary = FALSE
)

pipeline_from_path(path, settings_file = "settings.yaml")
}
\arguments{
\item{pipeline_name}{the name of the pipeline, usually title field in the
\code{'DESCRIPTION'} file, or the pipeline folder name (if description
file is missing)}

\item{settings_file}{the name of the settings file, usually stores user
inputs}

\item{paths}{the paths to search for the pipeline, usually the parent
directory of the pipeline; default is \code{\link{pipeline_root}}, which
only search for pipelines that are installed or in current working directory.}

\item{temporary}{see \code{\link{pipeline_root}}}

\item{path}{the pipeline folder}
}
\value{
A \code{\link{PipelineTools}} instance
}
\description{
Set pipeline inputs, execute, and read pipeline outputs
}
\examples{

if(!is_on_cran()) {

library(raveio)

# ------------ Set up a bare minimal example pipeline ---------------
pipeline_path <- pipeline_create_template(
  root_path = tempdir(), pipeline_name = "raveio_demo",
  overwrite = TRUE, activate = FALSE, template_type = "rmd-bare")

save_yaml(list(
  n = 100, pch = 16, col = "steelblue"
), file = file.path(pipeline_path, "settings.yaml"))

pipeline_build(pipeline_path)

rmarkdown::render(input = file.path(pipeline_path, "main.Rmd"),
                  output_dir = pipeline_path,
                  knit_root_dir = pipeline_path,
                  intermediates_dir = pipeline_path, quiet = TRUE)

utils::browseURL(file.path(pipeline_path, "main.html"))

# --------------------- Example starts ------------------------

pipeline <- pipeline("raveio_demo", paths = tempdir())

pipeline$run("plot_data")

# Run again and you will see some targets are skipped
pipeline$set_settings(pch = 2)
pipeline$run("plot_data")

head(pipeline$read("input_data"))

# or use
pipeline[c("n", "pch", "col")]
pipeline[-c("input_data")]

pipeline$target_table

pipeline$result_table

pipeline$progress("details")

# --------------------- Clean up ------------------------
unlink(pipeline_path, recursive = TRUE)

}
}
