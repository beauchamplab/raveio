#' @name cmd-external
#' @title External shell commands for 'RAVE'
#' @description These shell commands are for importing 'DICOM' images to
#' 'Nifti' format, reconstructing cortical surfaces, and align' the CT' to
#' 'MRI'. The commands are only tested on 'MacOS' and 'Linux'. On 'Windows'
#' machines, please use the 'WSL2' system.
#' @param subject characters or a \code{\link{RAVESubject}} instance
#' @param src_path source of the 'DICOM' or 'Nifti' image (absolute path)
#' @param type type of the 'DICOM' or 'Nifti' image; choices are \code{'MRI'}
#' and \code{'CT'}
#' @param merge,float,crop \code{'dcm2niix'} conversion arguments; ignored when
#' the source is in 'Nifti' format
#' @param overwrite whether to overwrite existing files; default is false
#' @param command_path command line path if 'RAVE' cannot find the command
#' binary files
#' @param dry_run whether to run in dry-run mode; under such mode, the shell
#' command will not execute. This is useful for debugging scripts; default is
#' false
#' @param verbose whether to print out the command script; default is true under
#' dry-run mode, and false otherwise
#' @param expr expression to run as command
#' @param quoted whether \code{expr} is quoted; default is false
#' @param mri_path the absolute to 'MRI' volume; must in 'Nifti' format
#' @param ct_path the absolute to 'CT' volume; must in 'Nifti' format
#' @param args further arguments in the shell command, especially the
#' 'FreeSurfer' reconstruction command
#' @param work_path work path for 'FreeSurfer' command;
#' @param script the shell script
#' @param script_path path to run the script
#' @param log_file where should log file be stored
#' @param command which command to invoke; default is \code{'bash'}
#' @param dof,cost,search,searchcost parameters used by 'FSL' \code{'flirt'}
#' command; see their documentation for details
#' @param backup whether to back up the script file immediately; default is true
#' @param ... passed to \code{\link{system2}}, or additional arguments
#' @returns A list of data containing the script details:
#' \describe{
#' \item{\code{script}}{script details}
#' \item{\code{script_path}}{where the script should/will be saved}
#' \item{\code{dry_run}}{whether dry-run mode is turned on}
#' \item{\code{log_file}}{path to the log file}
#' \item{\code{execute}}{a function to execute the script}
#' }
NULL

#' @rdname cmd-external
#' @export
cmd_execute <- function(script, script_path, command = "bash", dry_run = FALSE, backup = TRUE, args = NULL, ...) {

  dir_create2(dirname(script_path))
  writeLines(script, con = script_path)

  # Back up the script
  if(backup) {
    backup_dir <- file.path(dirname(script_path), "backups")
    backup_path <- backup_file(script_path, remove = FALSE, quiet = TRUE)
    if(!isFALSE(backup_path) && isTRUE(file.exists(backup_path))) {
      backup_dir <- dir_create2(backup_dir)
      to_path <- file.path(backup_dir, basename(backup_path))
      if(file.exists(to_path)) {
        unlink(backup_path)
      } else {
        file.rename(backup_path, to_path)
      }
    }
  }

  script_path <- normalizePath(script_path)

  if( dry_run ) {
    args <- paste(args, collapse = " ")
    if(nzchar(args)) {
      args <- sprintf("%s ", args)
    }
    if( .Platform$OS.type == "windows" ) {
      command <- gsub("/", "\\", command)
    }
    cmd <- sprintf("%s %s%s", shQuote(command), args, shQuote(script_path))
    return(cmd)
  } else {
    system2(command = command, args = c(args, shQuote(script_path)), ...)
  }

}


validate_nii <- function(path) {
  if(missing(path) || length(path) != 1 || is.na(path) || !file.exists(path) ||
     dir.exists(path)) {
    stop("`validate_nii`: `path` is not a valid file path.")
  }
  path <- normalizePath(path, winslash = "/")
  if(!grepl("\\.nii($|\\.gz$)", path, ignore.case = TRUE)) {
    stop("`validate_nii`: `path` is not a valid NifTi file (.nii or .nii.gz)")
  }
  path
}


rscript_path <- function(winslash = "\\") {
  binary_path <- R.home("bin")
  rscript_path <- list.files(binary_path, pattern = "^rscript",
                             full.names = TRUE, ignore.case = TRUE,
                             all.files = FALSE, recursive = FALSE,
                             include.dirs = FALSE)
  if(length(rscript_path)) {
    return(normalizePath(rscript_path[[1]], winslash = winslash))
  }

  rscript_path <- list.files(
    R.home(), pattern = "^rscript($|\\.exe$)",
    full.names = TRUE, ignore.case = TRUE,
    all.files = FALSE, recursive = TRUE,
    include.dirs = FALSE)

  if(length(rscript_path)) {
    # x64
    i386 <- grepl("i386", rscript_path)
    if(any(!i386)) {
      rscript_path <- rscript_path[!i386]
    }
    return(normalizePath(rscript_path[[1]], winslash = winslash))
  }

  # usually we won't reach to this step
  rscript_path <- Sys.which("Rscript")
  if(rscript_path != "") { return(normalizePath(rscript_path, winslash = winslash)) }

  rscript_path <- Sys.which("Rscript.exe")
  if(rscript_path != "") { return(normalizePath(rscript_path, winslash = winslash)) }

  return("Rscript")
}



#' @rdname cmd-external
#' @export
cmd_run_r <- function(
    expr, quoted = FALSE,
    verbose = TRUE, dry_run = FALSE,
    log_file = tempfile(), script_path = tempfile(),
    ...) {

  force(dry_run)
  if(!quoted) {
    expr <- substitute(expr)
  }

  # work_path <- normalizePath(
  #   file.path(subject$preprocess_settings$raw_path, "rave-imaging"),
  #   winslash = "/", mustWork = FALSE
  # )
  log_path <- normalizePath(dirname(log_file), mustWork = FALSE, winslash = "/")
  log_file <- basename(log_file)

  script_path <- normalizePath(
    script_path,
    mustWork = FALSE, winslash = "/"
  )

  cmd <- paste(collapse = "\n", c(
    "#!/usr/bin/env Rscript --no-save --no-restore",
    deparse(expr),
    "",
    "# END OF SCRIPT"
  ))

  execute <- function(...) {
    dir_create2(log_path)
    log_abspath <- normalizePath(file.path(log_path, log_file), winslash = "/", mustWork = FALSE)
    cmd_execute(script = cmd, script_path = script_path,
                args = c("--no-save", "--no-restore"),
                command = rscript_path(),
                stdout = log_abspath, stderr = log_abspath, ...)
  }
  re <- list(
    script = cmd,
    script_path = script_path,
    dry_run = dry_run,
    log_file = file.path(log_path, log_file, fsep = "/"),
    execute = execute,
    command = rscript_path()
  )
  if( verbose ) {
    message(cmd)
  }
  if(dry_run) {
    return(invisible(re))
  }

  execute()

  return(invisible(re))

}
