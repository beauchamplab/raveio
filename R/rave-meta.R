# This section is under development. They work for RAVE v1.0, but need some modification for RAVE 2.0

#' Function to save meta data to 'RAVE' subject
#' @param data data table
#' @param meta_type see load meta
#' @param project_name project name
#' @param subject_code subject code
#' @returns Either none if no meta matched or the absolute path of file saved.
#' @export
save_meta2 <- function(data, meta_type, project_name, subject_code){
  subject_code <- stringr::str_remove(subject_code, '^sub-')
  dirs <- rave_directories(subject_code = subject_code, project_name = project_name)
  meta_dir <- dirs$meta_path

  if(!dir.exists(meta_dir)){
    dir_create2(meta_dir)
  }

  if(meta_type == 'electrodes'){
    names(data)[1] <- c('Electrode')
    if(!'Coord_x' %in% names(data)){
      # try not to overwrite original data
      data$Coord_x <- 0
      data$Coord_y <- 0
      data$Coord_z <- 0
      data$Label <- ''
    }
    if(!"LocationType" %in% names(data)){
      data$LocationType <- "iEEG"
    }

    safe_write_csv(data, file = file.path(meta_dir, 'electrodes.csv'), row.names = FALSE)
  }else if(meta_type == 'time_points'){
    names(data) <- c('Block', 'Time')
    safe_write_csv(data, file = file.path(meta_dir, 'time_points.csv'), row.names = FALSE)
  }else if(meta_type == 'frequencies'){
    names(data) <- c('Frequency')
    safe_write_csv(data, file = file.path(meta_dir, 'frequencies.csv'), row.names = FALSE)
  }else if(meta_type == 'time_excluded'){
    # deprecated
    if(!is.data.frame(data)){
      data <- as.data.frame(data, stringsAsFactors = FALSE)
    }
    if(nrow(data)){
      names(data) <- c('Block', 'Start', 'End')
      safe_write_csv(data, file = file.path(meta_dir, 'time_excluded.csv'), row.names = FALSE)
    }
  }


}

load_electrodes_csv <- function(file) {
  tbl <- safe_read_csv(file)
  if(!'Label' %in% names(tbl)){
    tbl$Label <- NA
  }
  na_labels <- is.na(tbl$Label)
  if(any(na_labels)){
    tbl$Label[na_labels] <- paste0('Unlabeled', seq_len(sum(na_labels)))
  }

  if(!'LocationType' %in% names(tbl)){
    tbl$LocationType <- "iEEG"
  }
  if(any(!tbl$LocationType %in% c(LOCATION_TYPES, ""))){
    usp <- unique(tbl$LocationType[!tbl$LocationType %in% LOCATION_TYPES])
    warning("Unsupported electrode location type(s) found: ", paste(usp, collapse = ", "),
            ". Alter these electrode types to `iEEG`. If you see this warning, it is most likely the `LocationType` column in `electrodes.csv` (subject meta folder) contains invalid elements. I have corrected for you, however, please double-check the file as my correction might be wrong.")
    tbl$LocationType[!tbl$LocationType %in% LOCATION_TYPES] <- "iEEG"
  }
  tbl$LocationType[is.na(tbl$LocationType) | tbl$LocationType == ""] <- "iEEG"

  return(tbl)
}

#' Load 'RAVE' subject meta data
#' @param meta_type electrodes, epochs, time_points, frequencies, references ...
#' @param project_name project name
#' @param subject_code subject code
#' @param subject_id "project_name/subject_code"
#' @param meta_name only used if meta_type is epochs or references
#' @returns A data frame of the specified meta type or \code{NULL} is no meta
#' data is found.
#' @export
load_meta2 <- function(meta_type, project_name, subject_code, subject_id, meta_name){
  if(!missing(subject_id)){
    tmp <- stringr::str_split_fixed(subject_id, '/|\\\\', 2)
    project_name <- tmp[[1]]
    subject_code <- tmp[[2]]
  }
  subject_code <- stringr::str_remove(subject_code, '^sub-')

  if( meta_type == 'electrodes' ) {

    if(!missing(project_name)) {
      dirs <- rave_directories(subject_code = subject_code, project_name = project_name)
      meta_dir <- dirs$meta_path

      if(dir.exists(meta_dir)){
        file <- file.path(meta_dir, 'electrodes.csv')
        if(file.exists(file)){
          return(load_electrodes_csv(file))
        }
      }
    }

    # [project/subject] has no electrodes.csv
    # check raw_dir
    root_raw <- normalizePath(raveio_getopt('raw_data_dir'), mustWork = FALSE)
    file <- file.path(root_raw, subject_code, "rave-imaging", "derivative", "electrodes.csv")
    if(file.exists(file)){
      return(load_electrodes_csv(file))
    }
    return()
  }


  dirs <- rave_directories(subject_code = subject_code, project_name = project_name)
  meta_dir <- dirs$meta_path

  if(dir.exists(meta_dir)){
    if(meta_type == 'time_points'){
      file <- file.path(meta_dir, 'time_points.csv')
      if(file.exists(file)){
        tbl <- safe_read_csv(file, colClasses = c(Block = 'character'))
        return(tbl)
      }
    }
    else if(meta_type == 'time_excluded'){
      # Read time_excluded.csv if exists
      time_excluded_path <- file.path(meta_dir, 'time_excluded.csv')
      if(file.exists(time_excluded_path)){
        return(safe_read_csv(time_excluded_path, colClasses = c(Block = 'character')))
      }else{
        return(data.frame(
          Block = NULL,
          Electrode = NULL,
          Start = NULL,
          End = NULL
        ))
      }
    }
    else if(meta_type == 'frequencies'){
      file <- file.path(meta_dir, 'frequencies.csv')
      if(file.exists(file)){
        return(safe_read_csv(file, colClasses = c(Frequency = 'numeric')))
      }
    }
    else if(meta_type == 'epoch'){
      epoch_file <- file.path(meta_dir, sprintf('epoch_%s.csv', meta_name))
      if(!length(epoch_file) || !file.exists(epoch_file)){
        return(NULL)
      }
      default_cols <- c('Block', 'Time', 'Trial', 'Condition', 'Duration', 'ExcludedElectrodes')

      epochs <- utils::read.csv(epoch_file, header = TRUE, stringsAsFactors = FALSE,
                               colClasses = 'character')
      # check blocks in case block leading 0s are removed by excel
      preprocess_yaml <- file.path(dirs$proprocess_path, 'rave.yaml')
      if(file.exists(preprocess_yaml)){
        preproc_info <- load_yaml(preprocess_yaml)
        if(length(preproc_info$blocks)){
          pass_test <- TRUE
          # let's check block!
          invalid_blocks <- !epochs$Block %in% preproc_info$blocks
          if(any(invalid_blocks)){
            t1 <- data.frame(idx = seq_along(epochs$Block), block = epochs$Block, stringsAsFactors = FALSE)
            numeric_blocks <- suppressWarnings({ as.numeric(preproc_info$blocks) })
            t2 <- data.frame(block = preproc_info$blocks, numblock = numeric_blocks, value = preproc_info$blocks, stringsAsFactors = FALSE)
            t1 <- merge(t1, t2, all.x = TRUE, by.x = 'block', by.y = 'block')
            t1 <- merge(t1[, c('block', 'idx', 'value')], t2, all.x = TRUE, by.x = 'block', by.y = 'numblock', suffixes = c('1', '2'))
            sel <- is.na(t1$value1)
            t1$value1[sel] <- t1$value2[sel]

            if(any(is.na(t1$value1))){
              # block cannot find
              # TODO
            }

            epochs$Block <- t1$value1[order(t1$idx)]
          }
        }
      }

      epochs$Time <- as.numeric(epochs$Time)
      epochs$Trial <- as.numeric(epochs$Trial)
      epochs$Duration %?<-% NA
      epochs$Duration <- as.numeric(epochs$Duration)

      epochs$Condition %?<-% 'NoCondition'
      epochs$Condition[is.na(epochs$Condition)] <- 'NoCondition'
      epochs$Condition <- as.character(epochs$Condition)

      epochs$ExcludedElectrodes %?<-% ''
      # sort column orders
      nms <- names(epochs)
      nms <- c(default_cols, nms[!nms %in% default_cols])
      epochs <- epochs[, nms]
      # get column names with leading "Event_xxx"
      events <- nms[stringr::str_detect(nms, '^Event_.+')]
      for(evt in events){
        epochs[[evt]] <- as.numeric(epochs[[evt]])
      }
      return(epochs)
    }
    else if(meta_type == 'info'){
      info_file <- file.path(meta_dir, 'info.yaml')
      if(file.exists(info_file)){
        info <- load_yaml(info_file)
        return(as.list(info))
      }
    }
    else if(meta_type == 'time_excluded'){
      file <- file.path(meta_dir, 'time_excluded.csv')
      if(!file.exists(file)){
        return(NULL)
      }
      time_excluded <- utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE,
                                      colClasses = c('character', 'numeric', 'numeric'))
      return(time_excluded)
    }
    else if(meta_type == 'references'){
      file <- file.path(meta_dir, sprintf('reference_%s.csv', meta_name))
      if(!length(file) || !file.exists(file)){
        return(NULL)
      }
      ref_tbl <- utils::read.csv(file, header = TRUE, stringsAsFactors = FALSE)
      nms <- names(ref_tbl)
      dft <- c("Electrode", "Group", "Reference", "Type")
      ref_tbl <- ref_tbl[, c(dft[dft %in% nms], nms[!nms %in% dft])]
      return(ref_tbl)
    }
  }

  return(NULL)
}

#' Import electrode table into subject meta folder
#' @param path path of table file, must be a \code{'csv'} file
#' @param subject 'RAVE' subject ID or instance
#' @param use_fs whether to use 'FreeSurfer' files to calculate other
#' coordinates
#' @param dry_run whether to dry-run the process; if true, then the table
#' will be generated but not saved to subject's meta folder
#' @param ... passed to \code{\link[utils]{read.csv}}
#' @returns Nothing, the electrode information will be written directly to the
#' subject's meta directory
#' @export
import_electrode_table <- function (path, subject, use_fs = NA,
                                    dry_run = FALSE, ...) {

  subject <- restore_subject_instance(subject, strict = FALSE)
  electrodes <- subject$electrodes
  new_tbl <- utils::read.csv(path, stringsAsFactors = FALSE, ...)
  nms <- names(new_tbl)
  if (!"Electrode" %in% nms) {
    stop("`import_electrode_table` cannot find column `Electrode` (case-sensitive).")
  }
  if(length(electrodes)) {
    # subject has electrode channels set
    new_tbl <- new_tbl[new_tbl$Electrode %in% electrodes, ]
  }
  electrodes2 <- electrodes[!electrodes %in% new_tbl$Electrode]
  if (length(electrodes2)) {
    catgl("`import_electrode_table` has some issues with importing {path}",
         "\nThe number of electrodes in file does not match with what's in subject [{subject$subject_id}].\n  In table file: ", dipsaus::deparse_svec(new_tbl$Electrode),
         "\n  In subject: ", dipsaus::deparse_svec(electrodes),
         "\nAppending electrodes with blank rows", level = "WARNING")

    new_items <- data.frame(
      Electrode = electrodes2, Label = "NoLabel",
      Coord_x = 0, Coord_y = 0, Coord_z = 0,
      T1R = 0, T1A = 0, T1S = 0,
      MNI305_x = 0, MNI305_y = 0, MNI305_z = 0,
      MNI152_x = 0, MNI152_y = 0, MNI152_z = 0,
      Radius = 2, VertexNumber = -1,
      SignalType = subject$electrode_types[electrodes %in% electrodes2],
      SurfaceElectrode = FALSE, SurfaceType = "pial"
    )
    nms1 <- names(new_items)
    nms2 <- names(new_tbl)
    nms1 <- nms1[nms1 %in% nms2]
    nms2 <- nms2[!nms2 %in% nms1]
    for(nm in nms2) {
      if(is.character(new_tbl[[nm]])) {
        new_items[[nm]] <- ""
      } else if(is.numeric(new_tbl[[nm]])) {
        new_items[[nm]] <- 0
      } else {
        new_items[[nm]] <- NA
      }
    }
    new_items <- new_items[,names(new_tbl)]
    new_tbl <- rbind(new_tbl, new_items)
  }
  new_tbl <- new_tbl[order(new_tbl$Electrode), ]
  nms <- names(new_tbl)
  has_tkrRAS <- all(c("Coord_x", "Coord_y", "Coord_z") %in% nms)
  has_T1RAS <- all(c("T1R", "T1A", "T1S") %in% nms)
  has_mni305 <- all(c("MNI305_x", "MNI305_y", "MNI305_z") %in% nms)
  has_mni152 <- all(c("MNI152_x", "MNI152_y", "MNI152_z") %in% nms)
  if (!any(has_tkrRAS, has_T1RAS, has_mni305, has_mni152)) {
    catgl("`import_electrode_table`: No coordinates found. The coordinates are set to the origin. If you want to import coordinates. Please make sure to have at least one of the following coordinates in your file:\n  T1R, T1A, T1S (scanner T1 RAS)\n  Coord_x, Coord_y, Coord_z (FreeSurfer tkrRAS)\n  MNI305_x, MNI305_y, MNI305_z (MNI305 RAS)\n  MNI152_x, MNI152_y, MNI152_z (MNI152 RAS)\nImporting anyway...",
                  level = "WARNING")
    save_meta2(data = new_tbl, meta_type = "electrodes",
              project_name = subject$project_name,
              subject_code = subject$subject_code)
    catgl("`import_electrode_table`: Done importing {subject$subject_id} - meta/electrodes.csv. However, the coordinates are blank.", level = "INFO")
    return(invisible(NULL))
  }
  brain <- NULL
  if (!isFALSE(use_fs)) {
    brain <- rave_brain(subject = subject)
  }
  has_brain <- !is.null(brain)
  if (!has_brain) {
    if (use_fs) {
      stop("`import_electrode_table`: `use_fs=TRUE` but FreeSurfer is absent.")
    }
    catgl("`import_electrodes`: FreeSurfer files are missing. Save the electrodes with minimal editing", level = "WARNING")
    save_meta2(data = new_tbl, meta_type = "electrodes",
               project_name = subject$project_name,
               subject_code = subject$subject_code)
    return(invisible(NULL))
  }
  if (!has_tkrRAS) {
    catgl("FreeSurfer tkrRAS are not detected. Trying to calculate using T1, MNI305, or MNI152. \nOrder: T1 > MNI305 > MNI152.\n",
          level = "INFO")
    if (has_T1RAS) {
      catgl("T1 RAS detected! T1 -> tkrRAS")
      tmp <- new_tbl[, c("T1R", "T1A", "T1S")]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*%
                   tmp)[, c(1, 2, 3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c("x", "y", "z"))] <- tkRAS
    } else if (has_mni305) {
      catgl("MNI-305 detected! MNI-305 -> tkrRAS")
      tmp <- new_tbl[, paste0("MNI305_", c("x", "y", "z"))]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*%
                   solve(brain$xfm) %*% tmp)[, c(1, 2, 3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c("x", "y", "z"))] <- tkRAS
    } else if (has_mni152) {
      catgl("MNI-152 detected! MNI-152 -> tkrRAS")
      tmp <- new_tbl[, paste0("MNI152_", c("x", "y", "z"))]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*%
                   solve(brain$xfm) %*% solve(MNI305_to_MNI152) %*%
                   tmp)[, c(1, 2, 3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c("x", "y", "z"))] <- tkRAS
    } else {
      stop("Unexpected error: raveio-0001. Please report to https://github.com/beauchamplab/rave/issues")
    }
  }
  tkRAS <- t(cbind(data.matrix(new_tbl[, paste0("Coord_", c("x", "y", "z"))]), 1))
  invalids <- colSums(tkRAS == 0) == 3
  if (!has_T1RAS) {
    T1 <- t(brain$Norig %*% solve(brain$Torig) %*% tkRAS)[, c(1, 2, 3)]
    T1[invalids, ] <- 0
    new_tbl[, c("T1R", "T1A", "T1S")] <- T1
    catgl("T1 has been generated from tkrRAS", level = "INFO")
  }
  if (!has_mni305) {
    mni305 <- t(brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*%
                  tkRAS)[, c(1, 2, 3)]
    mni305[invalids, ] <- 0
    new_tbl[, c("MNI305_x", "MNI305_y", "MNI305_z")] <- mni305
    catgl("MNI-305 has been generated from tkrRAS", level = "INFO")
  }
  if (!has_mni152) {
    mni305 <- t(cbind(data.matrix(new_tbl[, c("MNI305_x",
                                              "MNI305_y", "MNI305_z")]), 1))
    mni152 <- t(MNI305_to_MNI152 %*% mni305)[, c(1, 2, 3)]
    mni152[invalids, ] <- 0
    new_tbl[, c("MNI152_x", "MNI152_y", "MNI152_z")] <- mni152
    catgl("MNI-152 has been generated from MNI-305", level = "INFO")
  }
  if (is.null(new_tbl$Label)) {
    new_tbl$Label <- "NoLabel"
  }
  if (is.null(new_tbl$SignalType)) {
    new_tbl$SignalType <- subject$electrode_types
  }
  brain$set_electrodes(new_tbl)
  new_tbl2 <- tryCatch({
    brain$calculate_template_coordinates()
  }, error = function(e){
    new_tbl
  })
  # Reorder
  nms1 <- c("Electrode", "Coord_x", "Coord_y", "Coord_z", "Label", "SignalType")
  nms <- names(new_tbl2)
  nms1 <- nms1[nms1 %in% nms]
  nms2 <- nms[!nms %in% nms1]
  new_tbl2 <- new_tbl2[, c(nms1, nms2)]
  if( dry_run ) {
    return(new_tbl2)
  }
  save_meta2(new_tbl2, meta_type = "electrodes",
             project_name = subject$project_name,
             subject_code = subject$subject_code)
  invisible(new_tbl2)
}
