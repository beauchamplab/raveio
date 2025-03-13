soft_deprecate <- local({

  warned_list <- list()

  function(name, alt_expr = "") {
    if(
      isTRUE(getOption("raveio.soft_deprecate.warn", TRUE)) &&
      !isTRUE(warned_list[[name]])
    ) {
      if(nzchar(alt_expr)) {
        warning("Function ", sQuote(name), " is soft deprecated. Please consider using `", alt_expr, "` instead. (This warning is only displayed once)")
      } else {
        warning("Function ", sQuote(name), " is soft deprecated. (This warning is only displayed once)")
      }
      warned_list[[name]] <<- TRUE
    }
    invisible()
  }
})

use_ravepipeline <- function(name, soft_deprecated = TRUE) {
  if( soft_deprecated ) {
    function(...) {
      soft_deprecate(sprintf("raveio:::%s", name), sprintf("ravepipeline:::%s", name))
      call_pkg_fun(package = "ravepipeline", f_name = name, ...)
    }
  } else {
    call_pkg_fun(package = "ravepipeline", f_name = name, .call_pkg_function = FALSE)
  }
}

activate_pipeline <- use_ravepipeline("activate_pipeline", soft_deprecated = FALSE)

PIPELINE_FORK_PATTERN <- call_pkg_fun(package = "ravepipeline", f_name = "PIPELINE_FORK_PATTERN", .call_pkg_function = FALSE)

# ---- internals.R -------------------------------------------------------------

guess_libpath <- use_ravepipeline("install_deps", soft_deprecated = FALSE)

install_deps <- use_ravepipeline("install_deps")

install_cran <- use_ravepipeline("install_cran")

session_uuid <- use_ravepipeline("session_uuid")

remove_empty_dir <- use_ravepipeline("remove_empty_dir", soft_deprecated = FALSE)

# ---- options.R ---------------------------------------------------------------
#' @importFrom ravepipeline raveio_setopt
#' @export
ravepipeline::raveio_setopt

#' @importFrom ravepipeline raveio_getopt
#' @export
ravepipeline::raveio_getopt

#' @importFrom ravepipeline raveio_resetopt
#' @export
ravepipeline::raveio_resetopt

#' @importFrom ravepipeline raveio_confpath
#' @export
ravepipeline::raveio_confpath

global_preferences <- use_ravepipeline("global_preferences", soft_deprecated = FALSE)

validate_settings <- use_ravepipeline("validate_settings")

load_setting <- use_ravepipeline("load_setting")

# ---- pipeline-docs.R ---------------------------------------------------------

sanitize_target_error <- use_ravepipeline("sanitize_target_error")

generate_target <- use_ravepipeline("generate_target")

# ---- pipeline-class.R --------------------------------------------------------

#' @importFrom ravepipeline PipelineTools
#' @export
ravepipeline::PipelineTools

#' @importFrom ravepipeline PipelineResult
#' @export
ravepipeline::PipelineResult


#' @importFrom ravepipeline pipeline
#' @export
ravepipeline::pipeline

#' @importFrom ravepipeline pipeline_from_path
#' @export
ravepipeline::pipeline_from_path

# ---- pipeline-install.R ------------------------------------------------------

pipeline_install_directory <- use_ravepipeline("pipeline_install_directory", soft_deprecated = FALSE)

#' @importFrom ravepipeline pipeline_install_local
#' @export
ravepipeline::pipeline_install_local

#' @importFrom ravepipeline pipeline_install_github
#' @export
ravepipeline::pipeline_install_github

#' @importFrom ravepipeline pipeline_root
#' @export
ravepipeline::pipeline_root

#' @importFrom ravepipeline pipeline_list
#' @export
ravepipeline::pipeline_list

#' @importFrom ravepipeline pipeline_find
#' @export
ravepipeline::pipeline_find

#' @importFrom ravepipeline pipeline_attach
#' @export
ravepipeline::pipeline_attach



# ---- pipeline-serializer.R ---------------------------------------------------

target_format <- use_ravepipeline("target_format")

target_format_dynamic <- use_ravepipeline("target_format_dynamic")

target_format_register <- use_ravepipeline("target_format_register")

target_format_unregister <- use_ravepipeline("target_format_unregister")

# ---- pipeline-run.R ----------------------------------------------------------

#' @importFrom ravepipeline pipeline_run
#' @export
ravepipeline::pipeline_run


#' @importFrom ravepipeline pipeline_clean
#' @export
ravepipeline::pipeline_clean


#' @importFrom ravepipeline pipeline_run_bare
#' @export
ravepipeline::pipeline_run_bare

#' @importFrom ravepipeline pipeline_eval
#' @export
ravepipeline::pipeline_eval


pipeline_run_interactive <- use_ravepipeline("pipeline_run_interactive")

# ---- pipeline-knitr.R --------------------------------------------------------

check_knit_packages <- use_ravepipeline("check_knit_packages")

resolve_pipeline_error <- use_ravepipeline("resolve_pipeline_error", soft_deprecated = FALSE)

rave_knit_r <- use_ravepipeline("rave_knit_r")

rave_knit_python <- use_ravepipeline("rave_knit_python")

rave_knitr_engine <- use_ravepipeline("rave_knitr_engine")

guess_py_indent <- use_ravepipeline("guess_py_indent")

rave_knitr_build <- use_ravepipeline("rave_knitr_build")

#' @importFrom ravepipeline pipeline_render
#' @export
ravepipeline::pipeline_render

#' @importFrom ravepipeline configure_knitr
#' @export
ravepipeline::configure_knitr

#' @importFrom ravepipeline pipeline_setup_rmd
#' @export
ravepipeline::pipeline_setup_rmd

# ---- pipeline-tools.R --------------------------------------------------------

#' @importFrom ravepipeline load_targets
#' @export
ravepipeline::load_targets

#' @importFrom ravepipeline pipeline_target_names
#' @export
ravepipeline::pipeline_target_names


#' @importFrom ravepipeline pipeline_debug
#' @export
ravepipeline::pipeline_debug

#' @importFrom ravepipeline pipeline_dep_targets
#' @export
ravepipeline::pipeline_dep_targets

#' @importFrom ravepipeline pipeline_visualize
#' @export
ravepipeline::pipeline_visualize

#' @importFrom ravepipeline pipeline_progress
#' @export
ravepipeline::pipeline_progress

#' @importFrom ravepipeline pipeline_fork
#' @export
ravepipeline::pipeline_fork

#' @importFrom ravepipeline pipeline_build
#' @export
ravepipeline::pipeline_build

#' @importFrom ravepipeline pipeline_read
#' @export
ravepipeline::pipeline_read

#' @importFrom ravepipeline pipeline_vartable
#' @export
ravepipeline::pipeline_vartable

#' @importFrom ravepipeline pipeline_hasname
#' @export
ravepipeline::pipeline_hasname

#' @importFrom ravepipeline pipeline_watch
#' @export
ravepipeline::pipeline_watch

#' @importFrom ravepipeline pipeline_create_template
#' @export
ravepipeline::pipeline_create_template

#' @importFrom ravepipeline pipeline_create_subject_pipeline
#' @export
ravepipeline::pipeline_create_subject_pipeline

#' @importFrom ravepipeline pipeline_description
#' @export
ravepipeline::pipeline_description

#' @importFrom ravepipeline pipeline_settings_set
#' @export
ravepipeline::pipeline_settings_set

#' @importFrom ravepipeline pipeline_settings_get
#' @export
ravepipeline::pipeline_settings_get

#' @importFrom ravepipeline pipeline_load_extdata
#' @export
ravepipeline::pipeline_load_extdata

#' @importFrom ravepipeline pipeline_save_extdata
#' @export
ravepipeline::pipeline_save_extdata

#' @importFrom ravepipeline pipeline_shared
#' @export
ravepipeline::pipeline_shared

#' @importFrom ravepipeline pipeline_set_preferences
#' @export
ravepipeline::pipeline_set_preferences

#' @importFrom ravepipeline pipeline_get_preferences
#' @export
ravepipeline::pipeline_get_preferences

#' @importFrom ravepipeline pipeline_has_preferences
#' @export
ravepipeline::pipeline_has_preferences

target_user_path <- use_ravepipeline("target_user_path")

load_target <- use_ravepipeline("load_target")

pipeline_dependency_graph <- use_ravepipeline("pipeline_dependency_graph", soft_deprecated = FALSE)

resolve_pipeline_settings_opt <- use_ravepipeline("resolve_pipeline_settings_opt")

resolve_pipeline_settings_value <- use_ravepipeline("resolve_pipeline_settings_value")

pipeline_py_info <- use_ravepipeline("pipeline_py_info")

pipeline_py_module <- use_ravepipeline("pipeline_py_module")

# ---- pipeline-collection.R ---------------------------------------------------

#' @importFrom ravepipeline pipeline_collection
#' @export
ravepipeline::pipeline_collection

#' @importFrom ravepipeline PipelineCollections
#' @export
ravepipeline::PipelineCollections

run_collection_pipeline <- use_ravepipeline("pipeline_py_module", soft_deprecated = FALSE)

# ---- snippet.R ---------------------------------------------------------------

#' @importFrom ravepipeline load_snippet
#' @export
ravepipeline::load_snippet

#' @importFrom ravepipeline update_local_snippet
#' @export
ravepipeline::update_local_snippet

#' @importFrom ravepipeline install_snippet
#' @export
ravepipeline::install_snippet

#' @importFrom ravepipeline list_snippets
#' @export
ravepipeline::list_snippets



# ---- module.R ----------------------------------------------------------------

#' @importFrom ravepipeline module_add
#' @export
ravepipeline::module_add

#' @importFrom ravepipeline install_modules
#' @export
ravepipeline::install_modules

#' @importFrom ravepipeline module_registry
#' @export
ravepipeline::module_registry

#' @importFrom ravepipeline module_registry2
#' @export
ravepipeline::module_registry2

#' @importFrom ravepipeline get_modules_registries
#' @export
ravepipeline::get_modules_registries

#' @importFrom ravepipeline get_module_description
#' @export
ravepipeline::get_module_description

#' @importFrom ravepipeline add_module_registry
#' @export
ravepipeline::add_module_registry

module_dev_create <- use_ravepipeline("module_dev_create", soft_deprecated = FALSE)

validate_modules_registries <- use_ravepipeline("validate_modules_registries")
