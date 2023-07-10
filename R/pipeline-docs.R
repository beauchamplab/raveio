#' @title 'RAVE' pipeline functions
#' @name rave-pipeline
#' @description Utility functions for 'RAVE' pipelines, currently designed for
#' internal development use. The infrastructure will be deployed to 'RAVE' in
#' the future to facilitate the "self-expanding" aim. Please check the official
#' 'RAVE' website.
#' @param pipe_dir where the pipeline directory is; can be set via system
#' environment \code{Sys.setenv("RAVE_PIPELINE"=...)}
#' @param quick whether to skip finished targets to save time
#' @param skip_names hint of target names to fast skip provided they are
#' up-to-date; only used when \code{quick=TRUE}. If missing, then
#' \code{skip_names} will be automatically determined
#' @param scheduler how to schedule the target jobs: default is \code{'none'},
#' which is sequential. If you have multiple heavy-weighted jobs that can be
#' scheduled at the same time, you can choose \code{'future'} or
#' \code{'clustermq'}
#' @param type how the pipeline should be executed; current choices are
#' \code{"smart"} to enable 'future' package if possible, \code{'callr'}
#' to use \code{\link[callr]{r}}, or \code{'vanilla'} to run everything
#' sequentially in the main session.
#' @param env,envir environment to execute the pipeline
#' @param callr_function function that will be passed to
#' \code{\link[targets]{tar_make}}; will be forced to be \code{NULL} if
#' \code{type='vanilla'}, or \code{\link[callr]{r}} if
#' \code{type='callr'}
#' @param async whether to run pipeline without blocking the main session
#' @param names the names of pipeline targets that are to be executed; default
#' is \code{NULL}, which runs all targets; use \code{pipeline_target_names}
#' to check all your available target names.
#' @param return_values whether to return pipeline target values; default is
#' true; only works in \code{pipeline_run_bare} and will be ignored by
#' \code{pipeline_run}
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
#' @param shortcut whether to display shortcut targets
#' @param glimpse whether to hide network status when visualizing the pipelines
#' @param zoom_speed zoom speed when visualizing the pipeline dependence
#' @param complete_only whether only to show completed and up-to-date target
#' variables; default is false
#' @param subject character indicating valid 'RAVE' subject ID, or
#' \code{\link{RAVESubject}} instance
#' @param name,pipeline_name the pipeline name to create; usually also the folder
#' @param settings_path path to settings file
#' name within subject's pipeline path
#' @param template_type which template type to create; choices are \code{'r'}
#' or \code{'rmd'}
#' @param temporary whether not to save \code{paths} to current pipeline
#' root registry. Set this to \code{TRUE} when importing pipelines
#' from subject pipeline folders
#' @param overwrite whether to overwrite existing pipeline; default is false
#' so users can double-check; if true, then existing pipeline, including the
#' data will be erased
#' @param file path to the 'DESCRIPTION' file under the pipeline folder, or
#' pipeline collection folder that contains the pipeline information,
#' structures, dependencies, etc.
#' @param root_path the root directory for pipeline templates
#' @param check_interval when running in background (non-blocking mode),
#' how often to check the pipeline
#' @param progress_title,progress_max,progress_quiet control the progress,
#' see \code{\link[dipsaus]{progress2}}.
#' @param destroy what part of data repository needs to be cleaned
#' @param ask whether to ask
#' @param format format of the extended data, default is \code{'json'}, other
#' choices are \code{'yaml'}, \code{'fst'}, \code{'csv'}, \code{'rds'}
#' @param error_if_missing,default_if_missing what to do if the extended data
#' is not found
#' @param data extended data to be saved
#' @param ... other parameters, targets, etc.
#' @returns \describe{
#' \item{\code{pipeline_root}}{the root directories of the pipelines}
#' \item{\code{pipeline_list}}{the available pipeline names under \code{pipeline_root}}
#' \item{\code{pipeline_find}}{the path to the pipeline}
#' \item{\code{pipeline_run}}{a \code{\link{PipelineResult}} instance}
#' \item{\code{load_targets}}{a list of targets to build}
#' \item{\code{pipeline_target_names}}{a vector of characters indicating the pipeline target names}
#' \item{\code{pipeline_visualize}}{a widget visualizing the target dependence structure}
#' \item{\code{pipeline_progress}}{a table of building progress}
#' \item{\code{pipeline_fork}}{a normalized path of the forked pipeline directory}
#' \item{\code{pipeline_read}}{the value of corresponding \code{var_names}, or a named list if \code{var_names} has more than one element}
#' \item{\code{pipeline_vartable}}{a table of summaries of the variables; can raise errors if pipeline has never been executed}
#' \item{\code{pipeline_hasname}}{logical, whether the pipeline has variable built}
#' \item{\code{pipeline_watch}}{a basic shiny application to monitor the progress}
#' \item{\code{pipeline_description}}{the list of descriptions of the pipeline or pipeline collection}
#' }
NULL


generate_target <- function(
    expr, export, format, deps = NULL,
    cue = "thorough", pattern = NULL, quoted = TRUE) {
  if(!quoted) {
    expr <- substitute(expr)
  }
  bquote(
    targets::tar_target_raw(
      name = .(export),
      command = quote({
        .__target_expr__. <- quote(.(expr))
        tryCatch({
          eval(.__target_expr__.)
          return(.(str2lang(export)))
        }, error = function(e) {
          asNamespace("raveio")$resolve_pipeline_error(
            name = .(export), condition = e, expr = .__target_expr__.)
        })
      }),
      format = asNamespace("raveio")$target_format_dynamic(
        name = .(format),
        target_export = .(export),
        target_expr = quote({
          .(expr)
          .(str2lang(export))
        }),
        target_depends = .(deps)
      ),
      deps = .(deps),
      cue = targets::tar_cue(.(cue)),
      pattern = .(pattern),
      iteration = "list"
    )
  )
}
