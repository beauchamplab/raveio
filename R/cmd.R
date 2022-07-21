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
  path, type = c("dcm2niix", "freesurfer", "fsl", "afni", "others"),
  unset = NA) {

  type <- match.arg(type)

  if(length(path) != 1 || is.na(path) || trimws(path) %in% c("", "/")) {
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
    `fsl` = {
      flirt <- file.path(path, "bin", "flirt")
      if(file.exists(flirt)){
        return(path)
      }
    },
    `afni` = {
      allineate <- file.path(path, "3dallineate")
      if(file.exists(allineate)){
        return(path)
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
    raveio_getopt("dcm2niix_path", default = Sys.which("dcm2niix")),
    type = "dcm2niix",
    unset = unset
  )
  if( length(path) != 1 || is.na(path) || !isTRUE(file.exists(path)) ) {
    if( error_on_missing ) {
      stop("Cannot find binary command `dcm2niix`. ",
           "Please go to the following website to install it:\n\n",
           "  https://github.com/rordenlab/dcm2niix#install\n\n",
           "If you have installed `dcm2niix`, please use\n\n",
           '  raveio::raveio_setopt("dcm2niix_path", <path to dcm2niix>)\n\n',
           "to set the path. Remember to replace and quote <path to dcm2niix>")
    }
  } else {
    path <- normalizePath(path, winslash = "/")
  }
  return(path)

}

#' @rdname rave_command_line_path
#' @export
cmd_freesurfer_home <- function(error_on_missing = TRUE, unset = NA) {
  path <- raveio_getopt("freesurfer_path", default = {
    Sys.getenv("FREESURFER_HOME", unset = NA)
  })
  if(isTRUE(is.na(path))) {
    path <- local({
      if(file.exists("/Applications/freesurfer")) {
        additional_fs <- c(
          "/Applications/freesurfer",
          list.dirs("/Applications/freesurfer", recursive = FALSE, full.names = TRUE)
        )
      } else {
        additional_fs <- c("/Applications/freesurfer")
      }
      fs <- c(
        additional_fs,
        "/usr/local/freesurfer"
      )
      fs <- fs[dir.exists(fs)]
      if(length(fs)) { fs } else { "" }
    })
  }
  path <- sapply(path, normalize_commandline_path, type = "freesurfer", unset = NA)
  path <- path[!is.na(path)]
  if(length(path)) {
    path <- path[[1]]
  } else {
    path <- unset
  }
  if( length(path) != 1 || is.na(path) || !isTRUE(dir.exists(path)) ) {
    if( error_on_missing ) {
      stop("Cannot find FreeSurfer home directory. ",
           "Please go to the following website to install it:\n\n",
           "  https://surfer.nmr.mgh.harvard.edu/fswiki/DownloadAndInstall\n\n",
           "If you have installed FreeSurfer, please use\n\n",
           '  raveio::raveio_setopt("freesurfer_path", <path to FREESURFER_HOME>)\n\n',
           "to set the FreeSurfer home directory. ",
           "Remember to replace and quote <path to FREESURFER_HOME>")
    }
  } else {
    path <- normalizePath(path, winslash = "/")
  }
  return(path)
}


#' @rdname rave_command_line_path
#' @export
cmd_fsl_home <- function(error_on_missing = TRUE, unset = NA) {

  path <- normalize_commandline_path(
    raveio_getopt("fsl_path", default = Sys.which("FSLDIR")),
    type = "others",
    unset = local({
      fs <- c(
        "/usr/local/fsl"
      )
      fs <- fs[dir.exists(fs)]
      if(length(fs)) { fs[[1]] } else { unset }
    })
  )
  if( length(path) != 1 || is.na(path) || !isTRUE(dir.exists(path)) ) {
    if( error_on_missing ) {
      stop("Cannot find binary command `flirt`. ",
           "Please go to the following website to install it:\n\n",
           "  https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FslInstallation\n\n",
           "If you have installed FSL-FLIRT, please use\n\n",
           '  raveio::raveio_setopt("fsl_path", <path to FSL>)\n\n',
           "to set the path. Remember to replace and quote <path to FSL>")
    }
  } else {
    path <- normalizePath(path, winslash = "/")
  }
  return(path)
}

#' @rdname rave_command_line_path
#' @export
cmd_afni_home <- function(error_on_missing = TRUE, unset = NA) {

  path <- normalize_commandline_path(
    raveio_getopt("afni_path", default = Sys.which("AFNI_HOME")),
    type = "afni",
    unset = local({
      fs <- c(
        "~/abin/"
      )
      fs <- fs[dir.exists(fs)]
      if(length(fs)) { fs[[1]] } else { unset }
    })
  )
  if( length(path) != 1 || is.na(path) || !isTRUE(dir.exists(path)) ) {
    if(error_on_missing) {
      stop("Cannot find AFNI command `3dallineate`. ",
           "Please go to the following website to install it:\n\n",
           "  https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/background_install/install_instructs/index.html\n\n",
           "If you have installed AFNI, please use\n\n",
           '  raveio::raveio_setopt("afni_path", <path to AFNI>)\n\n',
           "to set the path. Remember to replace and quote <path to AFNI>")
    }
  } else {
    path <- normalizePath(path, winslash = "/")
  }
  return(path)

}

#' @rdname rave_command_line_path
#' @export
cmd_homebrew <- function(error_on_missing = TRUE, unset = NA) {

  path <- normalize_commandline_path(
    raveio_getopt("homebrew_path", default = Sys.which("brew")),
    type = "others",
    unset = unset
  )
  if(length(path) != 1 || is.na(path) || !isTRUE(file.exists(path))) {

    if(identical(Sys.info()[['machine']], "arm64")) {
      path <- "/opt/homebrew/bin/brew"
    } else {
      path <- "/usr/local/bin/brew"
    }

  }
  if( length(path) != 1 || is.na(path) || !isTRUE(file.exists(path)) ) {
    if( error_on_missing ) {
      stop("Cannot find binary command `brew`. ",
           "Please open terminal and run the following shell command:\n\n",
           '  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"\n\n',
           "If you have installed brew, please use\n\n",
           '  raveio::raveio_setopt("homebrew_path", <path to brew>)\n\n',
           "to set the path. Remember to replace and quote <path to brew>")
    }
  } else {
    path <- normalizePath(path, winslash = "/")
  }
  return(path)
}


#' @rdname rave_command_line_path
#' @export
is_dry_run <- function(){
  isTRUE(raveio_getopt("cmd_dry_run", default = FALSE))
}

