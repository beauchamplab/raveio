#' Convert character to \code{\link{RAVEProject}} instance
#' @param project character project name
#' @param ... passed to other methods
#' @returns A \code{\link{RAVEProject}} instance
#' @seealso \code{\link{RAVEProject}}
#' @export
as_rave_project <- function(project, ...){
  if(inherits(project, 'RAVEProject')){
    return(project)
  } else {
    RAVEProject$new(project, ...)
  }
}


#' Get all possible projects in 'RAVE' directory
#' @param refresh whether to refresh the cache; default is true
#' @returns characters of project names
#' @export
get_projects <- local({
  re <- NULL
  function(refresh = TRUE){
    if(refresh || !length(re)){
      projects <- list.dirs(raveio_getopt('data_dir'), full.names = FALSE, recursive = FALSE)
      projects <- projects[stringr::str_detect(projects, '^[a-zA-Z0-9]+')]
      re <<- projects
    }
    re
  }
})

#' Definition for 'RAVE' project class
#' @export
RAVEProject <- R6::R6Class(
  classname = 'RAVEProject',
  class = TRUE,
  portable = FALSE,
  private = list(
    .name = character(0),
    .path = character(0)
  ),
  public = list(

    #' @description override print method
    #' @param ... ignored
    print = function(...){
      cat('RAVE project <', self$name, '>\n', sep = '')
      cat('  Directory:', self$path, '\n')
      cat('  Subjects :', paste(self$subjects(), collapse = ', '), '\n')
      nms <- names(self)
      nms <- nms[!nms %in% r6_reserved_fields]
      cat('Field/Method:', paste(nms, collapse = ', '), "\n")
    },

    #' @description constructor
    #' @param project_name character
    #' @param strict whether to check project path
    initialize = function(project_name, strict = TRUE){
      project_name <- stringr::str_trim(project_name)
      stopifnot2(length(project_name) == 1 && project_name != '',
                 msg = 'RAVEProject: project_name must not be blank character.')
      stopifnot2(!stringr::str_detect(project_name, '/|\\\\'),
                 msg = 'RAVEProject: project_name must contains no {sQuote("/")} nor {sQuote("\\\\")}')
      private$.name <- project_name

      dirs <- rave_directories('', project_name)
      private$.path <- normalizePath(dirs$project_path, mustWork = FALSE)
      if(strict && !dir.exists(private$.path)){
        warning(catgl("RAVE project does not exist:\n  {private$.path}", .capture = TRUE))
      }
    },

    #' @description get all imported subjects within project
    #' @returns character vector
    subjects = function(){
      re <- list.dirs(private$.path, full.names = FALSE, recursive = FALSE)
      # Must start with a-zA-Z
      re <- re[stringr::str_detect(re, '^[a-zA-Z]+')]
      re <- stringr::str_remove(re, '^sub-')
      re
    },

    #' @description whether a specific subject exists in this project
    #' @param subject_code character, subject name
    #' @returns true or false whether subject is in the project
    has_subject = function(subject_code){
      dirs <- rave_directories(subject_code, project_name = private$.name)
      dir.exists(dirs$subject_path)
    },

    #' @description get group data path for 'rave' module
    #' @param module_id character, 'rave' module ID
    #' @param must_work whether the directory must exist; if not exists,
    #' should a new one be created?
    group_path = function(module_id, must_work = FALSE){
      # fake subject code
      dirs <- rave_directories('', project_name = private$.name)
      p <- file.path(dirs$group_data_path, module_id)
      if(must_work){
        dir_create2(p, check = FALSE)
      }
      normalizePath(p, mustWork = FALSE)
    },

    #' @description list saved pipelines
    #' @param pipeline_name name of the pipeline
    #' @param cache whether to use cached registry
    #' @param check whether to check if the pipelines exist as directories
    #' @param all whether to list all pipelines; default is false; pipelines
    #' with the same label but older time-stamps will be hidden
    #' @returns A data table of pipeline time-stamps and directories
    subject_pipelines = function(pipeline_name, cache = FALSE, check = TRUE, all = FALSE) {
      # pipeline_name <- "power_explorer"
      # self <- raveio::as_rave_project("demo")
      # check = FALSE
      subjects <- self$subjects()
      re <- lapply(subjects, function(subject_code) {
        subject <- RAVESubject$new(project_name = self$name, subject_code = subject_code, strict = FALSE)
        subject$list_pipelines(pipeline_name = pipeline_name, check = check, all = all)
      })
      data.table::rbindlist(re)
    }

  ),
  active = list(

    #' @field path project folder, absolute path
    path = function(){
      private$.path
    },

    #' @field name project name, character
    name = function(){
      private$.name
    },

    #' @field pipeline_path path to pipeline scripts under project's folder
    pipeline_path = function(){
      file.path(private$.path, '_project_pipeline')
    }
  )
)
