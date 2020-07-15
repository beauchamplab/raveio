#' @title Verify 'RStudio' Version
#' @param version_needed minimum version required
#' @return whether 'RStudio' is running and its version is above the
#' required
#' @export
verify_rstudio_version <- function(version_needed = '1.2'){
  if(requireNamespace('shiny', quietly = TRUE)){
    # check shiny is installed, if not, shiny is not running
    if(!is.null(shiny::getDefaultReactiveDomain())){
      return(FALSE)
    }
  }
  if(!requireNamespace('rstudioapi')){
    return(FALSE)
  }
  tryCatch({
    rstudioapi::verifyAvailable(version_needed = version_needed)
    TRUE
  }, error = function(e){
    FALSE
  })
}

#' Get 'RStudio' active project
#' @return If 'RStudio' is running and current project is not none, return
#' project name, otherwise return \code{NULL}
#' @export
rstudio_active_project <- function(){
  if( verify_rstudio_version() ){
    return(rstudioapi::getActiveProject())
  }
  NULL
}

#' @title Get 'RStudio' Viewer, or Return Default
#' @param ... passed to \code{\link[rstudioapi]{viewer}}
#' @param default if \code{\link{verify_rstudio_version}} fails, the
#' value to return. Default is \code{TRUE}
#' @return If \code{\link[rstudioapi]{viewer}} can be called and
#' 'RStudio' is running, then launch 'RStudio' internal viewer.
#' Otherwise if \code{default} is a function such as
#' \code{\link[utils]{browseURL}}, then call the function with given
#' arguments. If \code{default} is not a function, return \code{default}
#' @export
rstudio_viewer <- function(..., default = TRUE){
  if(verify_rstudio_version()){
    rstudioapi::viewer(...)
  }else{
    if(is.function(default)){
      default(...)
    }else{
      return(default)
    }
  }
}


#' @title Use 'RStudio' to Select a Path on the Server
#' @param is_directory whether the path should be a directory
#' @return Raise error if \code{\link{verify_rstudio_version}} fails,
#' otherwise returns the selected path
#' @export
select_path <- function(is_directory = TRUE){
  if(verify_rstudio_version()){
    if(is_directory){
      path = rstudioapi::selectDirectory()
    }else{
      path = rstudioapi::selectFile()
    }
    # warning("Please fix the path in your script!!!\n\t{path}")
    return(path)
  }else{
    stop("Cannot find file path. Please contact package owner to fix it.")
  }
}

#' @title Ask questions via 'RStudio' or console
#' @param title,message title and message of the question
#' @param ok,cancel the button label to be displayed
#' @param use_console whether to force use console to ask questions
#' @description Use 'RStudio' dialogue to ask question and collect
#' answers interactively. If 'RStudio' is not running, a console version
#' will be raised.
#' @return A logical value.
#' @export
ask_question <- function(title, message, ok = 'Yes', cancel = 'No',
                         use_console = FALSE){
  if(!verify_rstudio_version()){
    use_console = TRUE
  }
  if(use_console){
    res = dipsaus::ask_yesno(title, '\n  - ', message, use_rs = FALSE)
    if(is.null(res) || is.na(res)){
      stop('Well... Maybe next time enter "yes" or "no" :)')
    }
    return( res )
  }else{
    rstudioapi::showQuestion(
      title = title,
      message = message,
      ok = ok,
      cancel = cancel
    )
  }

}


#' @title Save all documents in 'RStudio'
#' @description Perform "safe" save-all action with backward
#' compatibility: check whether 'RStudio' is running and whether
#' \code{rstudioapi} has function \code{documentSaveAll}.
#' @export
save_all <- function(){
  if(verify_rstudio_version()){
    if (rstudioapi::hasFun("documentSaveAll")) {
      rstudioapi::documentSaveAll()
    }
  }
}


