#' @title 'RAVE' pipeline functions
#' @name rave-pipeline
#' @description Utility functions for 'RAVE' pipelines, currently designed for
#' internal development use. The infrastructure will be deployed to 'RAVE' in
#' the future to facilitate the "self-expanding" aim.
#' @param pipe_dir where the pipeline directory is; can be set via system
#' environment \code{Sys.setenv("RAVE_PIPELINE"=...)}
#' @param quick whether to skip finished targets to save time
#' @param skip_names hint of target names to fast skip provided they are
#' up-to-date; only used when \code{quick=TRUE}. If missing, then
#' \code{skip_names} will be automatically determined
#' @param type how the pipeline should be executed; current choices are
#' \code{"basic"} to run in the main session; \code{"async"} to run in
#' a separate session without blocking the main session; \code{"vanilla"} to
#' run in a separate session and wait for the results; or \code{"custom"} to
#' run customized scheduler \code{callr_function}
#' @param env,envir environment to execute the pipeline
#' @param callr_function function to customized when \code{type="custom"}
#' @param method how the progress should be presented; choices are
#' \code{"summary"}, \code{"details"}, \code{"custom"}. If custom method is
#' chosen, then \code{func} will be called
#' @param func function to call when reading customized pipeline progress;
#' default is \code{\link[targets]{tar_progress_summary}}
#' @param src,dest pipeline folder to copy the pipeline script from and to
#' @param filter_pattern file name patterns used to filter the scripts to
#' avoid copying data files; default is \code{"\\.(R|yaml|txt|csv|fst|conf)$"}
#' @param activate whether to activate the new pipeline folder from \code{dest};
#' default is false
#' @param var_names variable name to fetch or to check
#' @param branches branch to read from; see \code{\link[targets]{tar_read}}
#' @param ifnotfound default values to return if variable is not found
#' @param targets_only whether to return the variable table for targets only;
#' default is true
#' @param complete_only whether only to show completed and up-to-date target
#' variables; default is false
#' @param subject character indicating valid 'RAVE' subject ID, or
#' \code{\link{RAVESubject}} instance
#' @param name,pipeline_name the pipeline name to create; usually also the folder
#' name within subject's pipeline path
#' @param template_type which template type to create; choices are \code{'r'}
#' or \code{'rmd'}
#' @param overwrite whether to overwrite existing pipeline; default is false
#' so users can double-check; if true, then existing pipeline, including the
#' data will be erased
#' @param file path to the 'DESCRIPTION' file under the pipeline folder, or
#' pipeline collection folder that contains the pipeline information,
#' structures, dependencies, etc.
#' @param root_path the root directory for pipeline templates
#' @param ... other parameters, targets, etc.
NULL
