
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

  # from <- "/Users/dipterix/Dropbox (PennNeurosurgery)/RAVE/Samples/raw/PAV006/rave-imaging/fs/mri/aparc+aseg.mgz"
  # to <- tempfile(fileext = ".nii")

  mgh <- freesurferformats::read.fs.mgh(from, with_header = TRUE, drop_empty_dims = FALSE)

  # generate nii1 header
  freesurferformats <- asNamespace("freesurferformats")
  header <- freesurferformats$ni1header.for.data(mgh$data)
  header$dim <- c(3L, dim(mgh$data)[1:3], 1L, 1L, 1L, 1L)
  header$intent_p1 <- 0
  header$intent_p2 <- 0
  header$intent_p3 <- 0
  header$intent_code <- 0L
  # header$datatype <- 8L
  # header$bitpix <- 32L
  header$slice_start <- 0L
  header$pix_dim <- c(-1, 1, 1, 1, 0, 1, 1, 1)
  # header$vox_offset <- 352L
  # header$scl_slope
  # header$scl_inter
  # header$slice_end
  # header$slice_code
  header$xyzt_units <- 10L
  # header$cal_max
  # header$cal_min
  # header$slice_duration <- 0L
  # header$toffset
  # header$glmax
  header$qform_code <- 1L
  mat <- mgh$header$vox2ras_matrix
  # a = 0.5  * sqrt(1+R11+R22+R33)    (not stored)
  # b = 0.25 * (R32-R23) / a       => quatern_b
  # c = 0.25 * (R13-R31) / a       => quatern_c
  # d = 0.25 * (R21-R12) / a       => quatern_d

  # extract rotation
  rot <- mat
  rot[, 4] <- c(0,0,0,1)
  rot <- t(t(rot) / sqrt(colSums(rot^2)))
  if( det(rot) < 0 ) {
    # r13 = -r13 ; r23 = -r23 ; r33 = -r33 ;
    rot[,3] <- -rot[,3]
  }
  quatern <- m44_to_quaternion(rot)
  header$quatern_b <- quatern[2]
  header$quatern_c <- quatern[3]
  header$quatern_d <- quatern[4]

  header$sform_code <- 1L
  header$qoffset_x <- mat[1, 4]
  header$qoffset_y <- mat[2, 4]
  header$qoffset_z <- mat[3, 4]
  header$srow_x <- mat[1, ]
  header$srow_y <- mat[2, ]
  header$srow_z <- mat[3, ]

  freesurferformats::write.nifti1(
    filepath = to,
    niidata = mgh$data,
    niiheader = header
  )

}
