% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-archive.R
\name{archive_subject}
\alias{archive_subject}
\title{Archive and share a subject}
\usage{
archive_subject(
  subject,
  path,
  includes = c("orignal_signals", "processed_data", "rave_imaging", "pipelines", "notes",
    "user_generated"),
  config = list()
)
}
\arguments{
\item{subject}{'RAVE' subject to archive}

\item{path}{path to a zip file to store; if missing or empty,
then the path will be automatically created}

\item{includes}{data to include in the archive; default includes all (
original raw signals, processed signals, imaging files, stored pipelines,
notes, and user-generated exports)}

\item{config}{a list of configurations, including changing subject code,
project name, or to exclude cache data; see examples}
}
\description{
Archive and share a subject
}
\examples{

# This example requires you to install demo subject

\dontrun{


# Basic usage
path <- archive_subject('demo/DemoSubject')

# clean up
unlink(path)

# Advanced usage: include all the original signals
# and processed data, no cache data, re-name to
# demo/DemoSubjectLite
path <- archive_subject(
  'demo/DemoSubject',
  includes = c("orignal_signals", "processed_data"),
  config = list(
    rename = list(
      project_name = "demo",
      subject_code = "DemoSubjectLite"
    ),
    orignal_signals = list(
      # include all raw signals
      include_all = TRUE
    ),
    processed_data = list(
      include_cache = FALSE
    )
  )
)

# Clean up temporary zip file
unlink(path)

}


}
