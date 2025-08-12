
#' Returns a list of 'RAVE' directories
#' @description This function is internally used and should not be called
#' directly.
#' @param subject_code 'RAVE' subject code
#' @param project_name 'RAVE' project name
#' @param blocks session or block names, optional
#' @param .force_format format of the data, default is automatically detected.
#' @returns A list of directories
#' @noRd
rave_directories <- function(subject_code, project_name, blocks = NULL, .force_format = c('', 'native', 'BIDS')){
  .force_format <- match.arg(.force_format)
  re <- dipsaus::fastmap2()

  if(startsWith(project_name, "@meta_analysis")) {
    subject_code <- strsplit(subject_code, "/")[[1]]
    project_name <- subject_code[[1]]
    subject_code <- subject_code[[2]]
  }

  subject_code <- gsub("^sub-", "", subject_code)

  re$root_data <- normalizePath(raveio_getopt('data_dir'), mustWork = FALSE)

  # check file structure mode
  fstruct <- raveio_getopt('file_structure')

  bids_raw <- normalizePath(raveio_getopt('bids_data_dir'), mustWork = FALSE)
  # raw path
  re$root_raw <- normalizePath(raveio_getopt('raw_data_dir'), mustWork = FALSE)
  re$raw_path <- file.path(re$root_raw, subject_code)
  re$.raw_path_type <- "native"
  if(!dir.exists(re$raw_path)){
    raw_path <- file.path(bids_raw, project_name, sprintf('sub-%s', subject_code))
    if(dir.exists(raw_path)){
      re$root_raw <- bids_raw
      re$raw_path <- raw_path
      re$.raw_path_type <- "bids"
    }
  }


  # TODO: in RAVE 2.0, BIDS should be supported and native path should be
  # supported.
  if(fstruct == 'BIDS' || .force_format == 'BIDS'){
    re$bids_project_path <- file.path(bids_raw, project_name)
    re$bids_subject_path <- file.path(re$bids_project_path, sprintf('sub-%s', subject_code))
    re$project_path <- file.path(re$root_data, project_name, 'derivatives', 'rave', project_name)
  } else {
    re$project_path <- file.path(re$root_data, project_name)
  }
  re$group_data_path <- file.path(re$project_path, '_project_data')
  re$subject_path <- file.path(re$project_path, subject_code)
  re$rave_path <- file.path(re$subject_path, 'rave')
  re$note_path <- file.path(re$subject_path, 'notes', 'rave_notes')
  re$proprocess_path <- file.path(re$rave_path, 'preprocess')
  re$meta_path <- file.path(re$rave_path, 'meta')
  re$data_path <- file.path(re$rave_path, 'data')
  re$reference_path <- file.path(re$data_path, 'reference')

  re$pipeline_path <- file.path(re$rave_path, 'pipeline')



  return(re)
}
