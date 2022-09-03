#' Get 'Neurosynth' website address using 'MNI152' coordinates
#' @param x,y,z numerical values: the right-anterior-superior 'RAS'
#' coordinates in \code{'MNI152'} space
#' @returns 'Neurosynth' website address
#' @export
url_neurosynth <- function(x, y, z) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  z <- as.numeric(z)

  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  z[is.na(z)] <- 0
  sprintf("https://neurosynth.org/locations/?x=%.0f&y=%.0f&z=%.0f", x, y, z)
}
