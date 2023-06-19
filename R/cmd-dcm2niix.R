# Scripts to generate shell commands - dcm to nii

#' @rdname cmd-external
#' @export
cmd_run_dcm2niix <- function(subject, src_path, type = c("MRI", "CT"),
                             merge = c("Auto", "No", "Yes"),
                             float = c("Yes", "No"),
                             crop = c("No", "Yes", "Ignore"),
                             overwrite = FALSE, command_path = NULL,
                             dry_run = FALSE, verbose = dry_run) {

  # DIPSAUS DEBUG START
  # # subject <- as_rave_subject("demo/DemoSubject")
  # src_path <- "/Users/dipterix/rave_data/raw_dir/YDS/IMG/CT-MRI/MRI/"
  # subject <- "devel/PAV006"
  # src_path <- "/Users/dipterix/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV006/pre-MRI/DICOM_SAG_MPRAGE_20220728090416_13.nii"
  # merge <- "Auto"
  # float <- "Yes"
  # crop <- "No"
  # type <- "MRI"
  # command_path <- NULL
  # dry_run <- TRUE
  # overwrite <- FALSE

  merge <- match.arg(merge)
  float <- match.arg(float)
  crop <- match.arg(crop)
  type <- match.arg(type)

  subject <- restore_subject_instance(subject, strict = FALSE)

  # find MRI path
  dest_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "inputs", type),
    mustWork = FALSE, winslash = "/"
  )
  derivative_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "derivative"),
    mustWork = FALSE, winslash = "/"
  )

  if(!overwrite && dir.exists(dest_path) &&
     length(list.files(dest_path, pattern = "\\.nii($|\\.gz$)", ignore.case = TRUE))) {
    stop("`cmd_run_dcm2niix`: destination folder already exists. Please specify `overwrite=TRUE` to remove the previous results.")
  }
  if(missing(src_path) || length(src_path) != 1 || is.na(src_path) || !file.exists(src_path)) {
    stop("`cmd_run_dcm2niix`: Blank or invalid `src_path` specified.")
  } else {
    src_path <- normalizePath(src_path, mustWork = TRUE, winslash = "/")
    if(startsWith(src_path, dest_path)) {
      # no need to import
      stop(sprintf("`cmd_run_dcm2niix`: `src_path` cannot be from within the following subject path [raw]/rave-imageing/inputs/%s", type))
    }
    # set default for modules
    catgl("Setting default {type} path: [{src_path}]", level = "trace")
    subject$set_default(sprintf("raw_%s_path", tolower(type)), src_path, namespace = "surface_reconstruction")
  }

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-dcm2niix-%y%m%d-%H%M%S.log")

  # Check if source file is a nifti file
  if( grepl("\\.(nii|nii\\.gz)$", src_path, ignore.case = TRUE) ) {
    # nii file, do not use dcm2niix!

    script_path <- normalizePath(
      file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts",
                sprintf("cmd-import-%s.R", tolower(type))),
      mustWork = FALSE, winslash = "/"
    )

    dst_fname <- sprintf(
      "%s_RAW.nii%s", type,
      ifelse( grepl("gz$", src_path, ignore.case = TRUE), ".gz", "")
    )


    cmd <- sprintf(r'(#!/usr/bin/env Rscript --no-save --no-restore
cat(
  sep = "",
  "Task: Import NIFTI file (%s)\n",
  strftime(Sys.time(), "Date: %%Y-%%m-%%d %%H:%%M:%%S %%Z\n")
)
src <- "%s"
dst_directory <- "%s"
derivative_path <- "%s"
dst_filename <- "%s"
if( !dir.exists( dst_directory )) {
  dir.create(path = dst_directory, showWarnings = FALSE, recursive = TRUE, mode = "0777")
}
if( !dir.exists( derivative_path ) ) {
  dir.create(path = derivative_path, showWarnings = FALSE, recursive = TRUE, mode = "0777")
}
file.copy(from = src, to = file.path(dst_directory, dst_filename),
          overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = TRUE)
cat(sep = "",
  "File copied from: ", src, "\nTo: ",
  file.path(dst_directory, dst_filename), "\n")
file.copy(from = src, to = file.path(derivative_path, dst_filename),
          overwrite = TRUE, recursive = FALSE, copy.mode = TRUE, copy.date = TRUE)
cat(sep = "",
  "File backed-up in: ",
  file.path(derivative_path, dst_filename), "\n")
)', toupper(type), src_path, dest_path, derivative_path, dst_fname)

    log_abspath <- file.path(log_path, log_file)

    command <- rscript_path()

    re <- list(
      script = cmd,
      script_path = script_path,
      dry_run = dry_run,
      log_file = file.path(log_path, log_file, fsep = "/"),
      src_path = src_path,
      dest_path = dest_path,
      command = command
    )

  } else {
    default_dcm2niix_path <- cmd_dcm2niix(error_on_missing = FALSE)
    dcm2niix <- normalize_commandline_path(
      path = command_path,
      unset = default_dcm2niix_path,
      type = "dcm2niix"
    )
    if(length(dcm2niix) != 1 || is.na(dcm2niix) || !isTRUE(file.exists(dcm2niix))) {
      dcm2niix <- NULL
    } else if (!identical(default_dcm2niix_path, dcm2niix)) {
      raveio_setopt("dcm2niix_path", dcm2niix)
    }
    has_dcm2niix <- !is.null(dcm2niix)

    # Generate script
    if(!has_dcm2niix) {
      dcm2niix <- "dcm2niix"
    }

    merge <- c("-m n ", "-m y ", "")[[which(c("No", "Yes", "Auto") == merge)]]
    float <- c('-p y ', '-p n ')[[which(c("Yes", "No") == float)]]
    crop <- c("-x n", "-x y", "-x i")[[which(c("No", "Yes", "Ignore") == crop)]]

    args <- paste0(merge, float, crop)

    template <- c(
      readLines(system.file('shell-templates/dcm2niix-mri-ct.sh', package = "raveio")),
      ""
    )

    script_path <- normalizePath(
      file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts",
                sprintf("cmd-import-%s.sh", tolower(type))),
      mustWork = FALSE, winslash = "/"
    )

    cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE)

    command <- "bash"

    re <- list(
      script = cmd,
      script_path = script_path,
      dry_run = dry_run,
      dcm2niix = if(has_dcm2niix) { dcm2niix } else { NULL },
      log_file = file.path(log_path, log_file, fsep = "/"),
      src_path = src_path,
      dest_path = dest_path,
      command = command
    )

  }

  execute <- function(..., args = NULL, stdout = "", stderr = "",
                         command = re$command) {
    initialize_imaging_paths(subject)
    if(!identical(stdout, "") || !identical(stderr, "")) {
      dir_create2(log_path)
    }
    if( grepl("rscript", command, ignore.case = TRUE) ) {
      args <- c("--no-save", "--no-restore", args)
    }
    cmd_execute(script = cmd, script_path = script_path, command = command, args = args, stdout = stdout, stderr = stderr, ...)
  }
  re$execute <- execute

  if( verbose ) {
    message(cmd)
  }
  if(dry_run) {
    return(invisible(re))
  }

  execute()


  return(invisible(re))
}


