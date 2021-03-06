% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edf.R
\name{read_edf_header}
\alias{read_edf_header}
\title{Read 'EDF(+)' or 'BDF(+)' file headers}
\usage{
read_edf_header(path)
}
\arguments{
\item{path}{file path, passed to \code{readEdfHeader}}
}
\value{
A list is header information of an 'EDF/BDF' file.
}
\description{
Wrapper of \code{\link[edfReader]{readEdfHeader}}, but added
some information
}
\details{
The added names are: \code{isAnnot2}, \code{sampleRate2}, and
\code{unit2}. To avoid conflict with other names, there is a "2" appended
to each names. \code{isAnnot2} indicates whether each channel is annotation
channel or recorded signals. \code{sampleRate2} is a vector of sample rates
for each channels. \code{unit2} is physical unit of recorded signals.
For 'iEEG' data, this is electric potential unit, and choices are \code{'V'}
for volt, \code{'mV'} for millivolt, and \code{'uV'} for micro-volt.
For more details, see \url{https://www.edfplus.info/specs/edftexts.html}
}
\seealso{
\code{\link[edfReader]{readEdfHeader}}
}
