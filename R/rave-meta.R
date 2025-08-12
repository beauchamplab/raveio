# This section is under development. They work for RAVE v1.0, but need some modification for RAVE 2.0



#' Convert electrode table
#' @param subject 'RAVE' subject
#' @param space suggested coordinate space, notice this argument might not be
#' supported when \code{'FreeSurfer'} reconstruction is missing.
#' @returns A list of table in data frame and a list of meta information
#' @export
convert_electrode_table_to_bids <- function(
    subject,
    space = c("ScanRAS", "MNI305", "fsnative")) {
  subject <- restore_subject_instance(subject)

  space <- match.arg(space)
  electrode_table <- subject$get_electrode_table()

  if( !is.data.frame(electrode_table) ) {
    stop(sprintf("Subject [%s] does not have valid electrode table.", subject$subject_id))
  }

  # https://bids-specification.readthedocs.io/en/stable/appendices/coordinate-systems.html#ieeg-specific-coordinate-systems

  re <- data.frame(
    name = electrode_table$Label
  )
  brain <- rave_brain(subject)
  switch(
    space,
    "ScanRAS" = {
      if(all(c("T1R", "T1A", "T1S") %in% names(electrode_table))) {
        re$x <- electrode_table$T1R
        re$y <- electrode_table$T1A
        re$z <- electrode_table$T1S
      } else if( is.null(brain) ) {
        re$x <- electrode_table$Coord_x
        re$y <- electrode_table$Coord_y
        re$z <- electrode_table$Coord_z
        space <- "fsnative"
      } else {
        tkr_ras <- cbind(data.matrix(electrode_table[, paste0("Coord_", c("x", "y", "z"))]), 1)
        invalid <- rowSums((tkr_ras[, c(1,2,3)])^2) == 0
        ras <- t(brain$Norig %*% solve(brain$Torig) %*% t(tkr_ras))[, c(1,2,3)]
        ras[invalid, ] <- 0
        re$x <- ras[,1]
        re$y <- ras[,2]
        re$z <- ras[,3]
      }
    },
    "MNI305" = {
      if(all(c("MNI305_x", "MNI305_y", "MNI305_z") %in% names(electrode_table))) {
        re$x <- electrode_table$MNI305_x
        re$y <- electrode_table$MNI305_y
        re$z <- electrode_table$MNI305_z
      } else if( is.null(brain) ) {
        re$x <- electrode_table$Coord_x
        re$y <- electrode_table$Coord_y
        re$z <- electrode_table$Coord_z
        space <- "fsnative"
      } else {
        tkr_ras <- cbind(data.matrix(electrode_table[, paste0("Coord_", c("x", "y", "z"))]), 1)
        invalid <- rowSums((tkr_ras[, c(1,2,3)])^2) == 0
        ras <- t(brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*% t(tkr_ras))[, c(1,2,3)]
        ras[invalid, ] <- 0
        re$x <- ras[,1]
        re$y <- ras[,2]
        re$z <- ras[,3]
      }
    },
    {
      re$x <- electrode_table$Coord_x
      re$y <- electrode_table$Coord_y
      re$z <- electrode_table$Coord_z
      space <- "fsnative"
    }
  )

  return(list(
    table = re,
    meta = list(
      iEEGCoordinateSystem = ifelse(space == "fsnative", "Other", space),
      iEEGCoordinateUnits = "mm",
      iEEGCoordinateSystemDescription = list(
        "fsnative" = "FreeSurfer tk-registered RAS",
        "MNI305" = "Linear registered MNI-305 RAS",
        "ScanRAS" = "T1-weighted RAS"
      )[[space]],
      iEEGCoordinateProcessingReference = "Wang, Z., Magnotti, J., Zhang, X. and Beauchamp, M.S., 2023. YAEL: Your Advanced Electrode Localizer. bioRxiv, pp.2023-08. https://doi.org/10.1101/2023.08.04.552023"
    )
  ))
}
