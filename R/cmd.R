# Find command line paths


#' @name rave_command_line_path
#' @title Find and execute external command-line tools
#' @param path path to normalize
#' @param type type of command
#' @param unset default to return if the command is not found
#' @param error_on_missing whether to raise errors if command is missing
#' @return Normalized path to the command, or \code{unset} if command is
#' missing.
#' @export
normalize_commandline_path <- function(
  path, type = c("dcm2niix", "freesurfer", "others"),
  unset = NA) {

  type <- match.arg(type)

  if(length(path) != 1 || trimws(path) == "") {
    return(unset)
  }
  if(!file.exists(path)) {
    return(unset)
  }
  path <- normalizePath(path, winslash = "\\")
  if(is_dry_run()) {
    return(path)
  }

  switch (
    type,
    `dcm2niix` = {
      res <- ""
      suppressWarnings({
        res <- system2(path, args = "--version",
                       wait = TRUE, stdout = TRUE, stderr = TRUE)
        if(any(grepl("dcm2niix", res, ignore.case = TRUE))) {
          return(path)
        }
      })
    },
    `freesurfer` = {
      recon_all <- file.path(path, "bin", "recon-all")
      if(file.exists(recon_all)){
        res <- system2(
          command = recon_all,
          args = "--version",
          env = c(sprintf(
            "FREESURFER_HOME=%s", shQuote(path, type = "cmd")
          )),
          wait = TRUE, stdout = TRUE, stderr = TRUE
        )
        if(grepl("freesurfer", res, ignore.case = TRUE)) {
          return(path)
        }
      }
    },
    {
      return(path)
    }
  )

  return(unset)

}

#' @rdname rave_command_line_path
#' @export
cmd_dcm2niix <- function(error_on_missing = TRUE, unset = NA) {

  path <- normalize_commandline_path(
    raveio::raveio_getopt("dcm2niix_path", default = Sys.which("dcm2niix")),
    type = "dcm2niix",
    unset = unset
  )
  if(error_on_missing && !isTRUE(file.exists(path))) {
    stop("Cannot find binary command `dcm2niix`. ",
         "Please go to the following website to install it:\n\n",
         "  https://github.com/rordenlab/dcm2niix#install\n\n",
         "If you have installed `dcm2niix`, please use\n\n",
         '  raveio::raveio_setopt("dcm2niix_path", <path to dcm2niix>)\n\n',
         "to set the path. Remember to replace and quote <path to dcm2niix>")
  }
  return(path)

}

#' @rdname rave_command_line_path
#' @export
cmd_freesurfer_home <- function(error_on_missing = TRUE, unset = NA) {
  path <- raveio::raveio_getopt("freesurfer_path", default = {
    Sys.getenv("FREESURFER_HOME", unset = local({
      fs <- c(
        "/Applications/freesurfer",
        "/usr/local/freesurfer"
      )
      fs <- fs[dir.exists(fs)]
      if(length(fs)) { fs[[1]] } else { "" }
    }))
  })
  path <- normalize_commandline_path(path, type = "freesurfer", unset = unset)
  if(error_on_missing && !isTRUE(file.exists(path))) {
    stop("Cannot find FreeSurfer home directory. ",
         "Please go to the following website to install it:\n\n",
         "  https://surfer.nmr.mgh.harvard.edu/fswiki/DownloadAndInstall\n\n",
         "If you have installed FreeSurfer, please use\n\n",
         '  raveio::raveio_setopt("freesurfer_path", <path to FREESURFER_HOME>)\n\n',
         "to set the FreeSurfer home directory. ",
         "Remember to replace and quote <path to FREESURFER_HOME>")
  }
  return(path)
}


#' @rdname rave_command_line_path
#' @export
cmd_fsl_flirt <- function(error_on_missing = TRUE, unset = NA) {

  path <- normalize_commandline_path(
    raveio::raveio_getopt("fsl_flirt_path", default = Sys.which("flirt")),
    type = "others",
    unset = unset
  )
  if(error_on_missing && !isTRUE(file.exists(path))) {
    stop("Cannot find binary command `flirt`. ",
         "Please go to the following website to install it:\n\n",
         "  https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FslInstallation\n\n",
         "If you have installed FSL-FLIRT, please use\n\n",
         '  raveio::raveio_setopt("fsl_flirt_path", <path to flirt>)\n\n',
         "to set the path. Remember to replace and quote <path to flirt>")
  }
  return(path)

}

#' @rdname rave_command_line_path
#' @export
is_dry_run <- function(){
  isTRUE(raveio::raveio_getopt("cmd_dry_run", default = FALSE))
}
