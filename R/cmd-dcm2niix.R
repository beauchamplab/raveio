# Scripts to generate shell commands - dcm to nii

#' @rdname cmd-external
#' @export
cmd_run_dcm2niix <- function(subject, src_path, type = c("MRI", "CT"),
                             merge = c("Auto", "No", "Yes"),
                             float = c("Yes", "No"),
                             crop = c("No", "Yes", "Ignore"),
                             overwrite = FALSE, command_path = NULL,
                             dry_run = FALSE, verbose = dry_run) {

  # # Debug:
  # subject <- as_rave_subject("demo/DemoSubject")
  # src_path <- "/Users/dipterix/rave_data/raw_dir/YDS/IMG/CT-MRI/MRI/"
  # merge <- "Auto"
  # float <- "Yes"
  # crop <- "No"
  # type <- "MRI"
  # command_path <- NULL
  # raveio:::cmd_run_dcm2niix(subject = "devel/YDS", src_path = "/Users/dipterix/rave_data/raw_dir/YDS/IMG/CT-MRI/MRI/")

  merge <- match.arg(merge)
  float <- match.arg(float)
  crop <- match.arg(crop)
  type <- match.arg(type)

  subject <- as_rave_subject(subject, strict = FALSE)

  # find MRI path
  dest_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "inputs", type),
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
  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-dcm2niix-%y%m%d-%H%M%S.log")

  template <- c(
    readLines(system.file('shell-templates/dcm2niix-mri-ct.sh', package = "raveio")),
    ""
  )
  cmd <- glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE)

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts",
              sprintf("cmd-import-%s.sh", tolower(type))),
    mustWork = FALSE, winslash = "/"
  )

  execute <- function(...) {
    initialize_imaging_paths(subject)
    cmd_execute(script = cmd, script_path = script_path, command = "bash", ...)
  }

  re <- list(
    script = cmd,
    script_path = script_path,
    dry_run = dry_run,
    dcm2niix = if(has_dcm2niix) { dcm2niix } else { NULL },
    log_file = file.path(log_path, log_file, fsep = "/"),
    src_path = src_path,
    dest_path = dest_path,
    execute = execute
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


