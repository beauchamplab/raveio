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


