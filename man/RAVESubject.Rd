% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-subject.R
\name{RAVESubject}
\alias{RAVESubject}
\title{Definition for 'RAVE' subject class}
\description{
Definition for 'RAVE' subject class

Definition for 'RAVE' subject class
}
\seealso{
\code{\link{load_meta2}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{project}}{project instance of current subject; see
\code{\link{RAVEProject}}}

\item{\code{project_name}}{character string of project name}

\item{\code{subject_code}}{character string of subject code}

\item{\code{subject_id}}{subject ID: \code{"project/subject"}}

\item{\code{path}}{subject root path}

\item{\code{rave_path}}{'rave' directory under subject root path}

\item{\code{meta_path}}{meta data directory for current subject}

\item{\code{freesurfer_path}}{'FreeSurfer' directory for current subject. If
no path exists, values will be \code{NA}}

\item{\code{preprocess_path}}{preprocess directory under subject 'rave' path}

\item{\code{data_path}}{data directory under subject 'rave' path}

\item{\code{cache_path}}{path to 'FST' copies under subject 'data' path}

\item{\code{epoch_names}}{possible epoch names}

\item{\code{reference_names}}{possible reference names}

\item{\code{reference_path}}{reference path under 'rave' folder}

\item{\code{preprocess_settings}}{preprocess instance; see
\code{\link{RAVEPreprocessSettings}}}

\item{\code{blocks}}{subject experiment blocks in current project}

\item{\code{electrodes}}{all electrodes, no matter excluded or not}

\item{\code{raw_sample_rates}}{voltage sample rate}

\item{\code{power_sample_rate}}{power spectrum sample rate}

\item{\code{has_wavelet}}{whether electrodes have wavelet transforms}

\item{\code{notch_filtered}}{whether electrodes are Notch-filtered}

\item{\code{electrode_types}}{electrode signal types}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-print}{\code{RAVESubject$print()}}
\item \href{#method-new}{\code{RAVESubject$new()}}
\item \href{#method-meta_data}{\code{RAVESubject$meta_data()}}
\item \href{#method-valid_electrodes}{\code{RAVESubject$valid_electrodes()}}
\item \href{#method-initialize_paths}{\code{RAVESubject$initialize_paths()}}
\item \href{#method-clone}{\code{RAVESubject$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
override print method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVESubject$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{ignored}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
constructor
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVESubject$new(project_name, subject_code = NULL, strict = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{project_name}}{character project name}

\item{\code{subject_code}}{character subject code}

\item{\code{strict}}{whether to check if subject folders exist}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-meta_data"></a>}}
\if{latex}{\out{\hypertarget{method-meta_data}{}}}
\subsection{Method \code{meta_data()}}{
get subject meta data located in \code{"meta/"} folder
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVESubject$meta_data(
  meta_type = c("electrodes", "frequencies", "time_points", "epoch", "references"),
  meta_name = "default"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{meta_type}}{choices are 'electrodes', 'frequencies', 'time_points',
'epoch', 'references'}

\item{\code{meta_name}}{if \code{meta_type='epoch'}, read in
\code{'epoch_<meta_name>.csv'}; if \code{meta_type='references'},
read in \code{'reference_<meta_name>.csv'}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data frame
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-valid_electrodes"></a>}}
\if{latex}{\out{\hypertarget{method-valid_electrodes}{}}}
\subsection{Method \code{valid_electrodes()}}{
get valid electrode numbers
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVESubject$valid_electrodes(reference_name, refresh = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{reference_name}}{character, reference name, see \code{meta_name}
in \code{self$meta_data} or \code{\link{load_meta2}} when
\code{meta_type} is 'reference'}

\item{\code{refresh}}{whether to reload reference table before obtaining data,
default is false}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
integer vector of valid electrodes
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-initialize_paths"></a>}}
\if{latex}{\out{\hypertarget{method-initialize_paths}{}}}
\subsection{Method \code{initialize_paths()}}{
create subject's directories on hard disk
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVESubject$initialize_paths(include_freesurfer = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{include_freesurfer}}{whether to create 'FreeSurfer' path}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVESubject$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
