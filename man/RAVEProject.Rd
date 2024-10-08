% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-project.R
\name{RAVEProject}
\alias{RAVEProject}
\title{Definition for 'RAVE' project class}
\value{
character vector

true or false whether subject is in the project

A data table of pipeline time-stamps and directories
}
\description{
Definition for 'RAVE' project class

Definition for 'RAVE' project class
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{path}}{project folder, absolute path}

\item{\code{name}}{project name, character}

\item{\code{pipeline_path}}{path to pipeline scripts under project's folder}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RAVEProject-print}{\code{RAVEProject$print()}}
\item \href{#method-RAVEProject-new}{\code{RAVEProject$new()}}
\item \href{#method-RAVEProject-subjects}{\code{RAVEProject$subjects()}}
\item \href{#method-RAVEProject-has_subject}{\code{RAVEProject$has_subject()}}
\item \href{#method-RAVEProject-group_path}{\code{RAVEProject$group_path()}}
\item \href{#method-RAVEProject-subject_pipelines}{\code{RAVEProject$subject_pipelines()}}
\item \href{#method-RAVEProject-clone}{\code{RAVEProject$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RAVEProject-print"></a>}}
\if{latex}{\out{\hypertarget{method-RAVEProject-print}{}}}
\subsection{Method \code{print()}}{
override print method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVEProject$print(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-RAVEProject-new"></a>}}
\if{latex}{\out{\hypertarget{method-RAVEProject-new}{}}}
\subsection{Method \code{new()}}{
constructor
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVEProject$new(project_name, strict = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{project_name}}{character}

\item{\code{strict}}{whether to check project path}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RAVEProject-subjects"></a>}}
\if{latex}{\out{\hypertarget{method-RAVEProject-subjects}{}}}
\subsection{Method \code{subjects()}}{
get all imported subjects within project
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVEProject$subjects()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RAVEProject-has_subject"></a>}}
\if{latex}{\out{\hypertarget{method-RAVEProject-has_subject}{}}}
\subsection{Method \code{has_subject()}}{
whether a specific subject exists in this project
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVEProject$has_subject(subject_code)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{subject_code}}{character, subject name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RAVEProject-group_path"></a>}}
\if{latex}{\out{\hypertarget{method-RAVEProject-group_path}{}}}
\subsection{Method \code{group_path()}}{
get group data path for 'rave' module
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVEProject$group_path(module_id, must_work = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{module_id}}{character, 'rave' module ID}

\item{\code{must_work}}{whether the directory must exist; if not exists,
should a new one be created?}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RAVEProject-subject_pipelines"></a>}}
\if{latex}{\out{\hypertarget{method-RAVEProject-subject_pipelines}{}}}
\subsection{Method \code{subject_pipelines()}}{
list saved pipelines
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVEProject$subject_pipelines(
  pipeline_name,
  cache = FALSE,
  check = TRUE,
  all = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pipeline_name}}{name of the pipeline}

\item{\code{cache}}{whether to use cached registry}

\item{\code{check}}{whether to check if the pipelines exist as directories}

\item{\code{all}}{whether to list all pipelines; default is false; pipelines
with the same label but older time-stamps will be hidden}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RAVEProject-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RAVEProject-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RAVEProject$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
