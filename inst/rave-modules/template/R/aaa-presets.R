#' This file runs right after `aa.R`. The goal of this script
#' is to generate a bunch of preset input components
#' and create a component container that collate all the components
NULL

# Create a component container
component_container <- ravedash::new_rave_shiny_component_container(
  module_id = module_id, pipeline_name = pipeline$pipeline_name,
  settings_file = "settings.yaml"
)


# Define components
loader_project <- ravedash::presets_loader_project()
loader_subject <- ravedash::presets_loader_subject()
loader_epoch <- ravedash::presets_loader_epoch()
loader_electrodes <- ravedash::presets_loader_electrodes()
loader_reference <- ravedash::presets_loader_reference()
loader_viewer <- ravedash::presets_loader_3dviewer(height = "100%")

import_export_pipeline <- ravedash::presets_import_export_subject_pipeline()
electrode_selector <- ravedash::presets_analysis_electrode_selector2()
baseline_choices <- ravedash::presets_baseline_choices()
comp_condition_groups <- ravedash::presets_condition_groups()
comp_analysis_ranges <- ravedash::presets_analysis_ranges()

# Register the components
component_container$add_components(
  loader_project, loader_subject, loader_epoch,
  loader_electrodes, loader_reference, loader_viewer,
  electrode_selector, import_export_pipeline, baseline_choices,
  comp_condition_groups, comp_analysis_ranges
)


