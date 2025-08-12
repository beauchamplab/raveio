
m44_to_quaternion <- function(m) {
  m00 <- m[1, 1]
  m01 <- m[1, 2]
  m02 <- m[1, 3]
  m10 <- m[2, 1]
  m11 <- m[2, 2]
  m12 <- m[2, 3]
  m20 <- m[3, 1]
  m21 <- m[3, 2]
  m22 <- m[3, 3]

  tr <- m00 + m11 + m22 + 1.0

  # https://github.com/NIFTI-Imaging/nifti_clib/blob/d05a71ecdc0b811a509b77c3e82fd0674e901807/niftilib/nifti1_io.c
  if( tr > 0.5 ) {
    S <- sqrt( tr ) * 2.0
    qw <- 0.25 * S
    qx <- (m21 - m12) / S
    qy <- (m02 - m20) / S
    qz <- (m10 - m01) / S
  } else {
    Sx <- sqrt(1.0 + m00 - m11 - m22) * 2.0 # S = 4 * qx
    Sy <- sqrt(1.0 + m11 - m00 - m22) * 2.0 # S = 4 * qy
    Sz <- sqrt(1.0 + m22 - m00 - m11) * 2.0 # S = 4 * qz

    if( Sx > 2.0 ) {
      qw <- (m21 - m12) / Sx
      qx <- 0.25 * Sx
      qy <- (m01 + m10) / Sx
      qz <- (m02 + m20) / Sx
    } else if ( Sy > 2.0 ) {
      qw <- (m02 - m20) / Sy
      qx <- (m01 + m10) / Sy
      qy <- 0.25 * Sy
      qz <- (m12 + m21) / Sy
    } else {
      qw <- (m10 - m01) / Sz
      qx <- (m02 + m20) / Sz
      qy <- (m12 + m21) / Sz
      qz <- 0.25 * Sz
    }

    if( tr < 0.0 ) {
      qx <- -qx
      qy <- -qy
      qz <- -qz
    }

  }
  return(c(qw, qx, qy, qz))
}

#' Convert 'FreeSurfer' \code{'mgh'} to 'Nifti'
#' @param from path to 'FreeSurfer' \code{'mgh'} or \code{'mgz'} file
#' @param to path to 'Nifti' file, must ends with \code{'nii'} or \code{'nii.gz'}
#' @returns Nothing; the file will be created to path specified by \code{to}
#' @export
mgh_to_nii <- function(from, to) {

  v <- ieegio::as_ieegio_volume(from)
  ieegio::write_volume(v, con = to, format = "nifti")

}
