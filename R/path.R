# path-related functions



#' Simple hard disk speed test
#' @param path an existing directory where to test speed, default is temporary
#' local directory.
#' @param file_size in bytes, default is 1 MB.
#' @param quiet should verbose messages be suppressed?
#' @param abort_if_slow abort test if hard drive is too slow. This usually
#' happens when the hard drive is connected via slow internet: if the write
#' speed is less than 0.1 MB per second.
#' @param use_cache if hard drive speed was tested before, abort testing
#' and return cached results or not; default is false.
#' @returns A vector of two: writing and reading speed in MB per seconds.
#' @export
test_hdspeed <- function(path = tempdir(), file_size = 1e6, quiet = FALSE,
                         abort_if_slow = TRUE, use_cache = FALSE){

  last_speed <- raveio_getopt('drive_speed')

  if(!all(last_speed == c(50, 20)) && use_cache){
    return(last_speed)
  }

  if(!dir.exists(path)){
    warning(path, ' does not exist.')
    return(last_speed)
  }

  # create tempdir for testing
  root_dir <- file.path(path, '.rave_hd_test')
  has_root <- dir.exists(root_dir)
  test_dir <- file.path(path, '.rave_hd_test', rand_string(8))
  on.exit({
    if(!has_root){
      unlink(root_dir, recursive = TRUE)
    }else{
      unlink(test_dir, recursive = TRUE)
    }
  })
  dir_create2(test_dir)

  if(abort_if_slow && file_size > 1e5){
    # test write speed. If the speed is too low, should ignore test
    file <- tempfile(tmpdir = test_dir)
    dat <- rand_string(1e5 - 1)
    upload <- system.time(writeLines(dat, file, useBytes = TRUE))
    wsp <- 0.1 / (upload[3])
    if( wsp < 0.1 ) {
      if(!quiet){
        catgl('Hard disk speed might be too slow. Abort speed test')
      }
      sp <- c(wsp, wsp)
      raveio_setopt('drive_speed', sp)
      return(sp)
    }
  }

  progress <- ravepipeline::rave_progress(title = 'Testing read/write speed', max = 2,
                                quiet = quiet, shiny_auto_close = TRUE)

  progress$inc('Write to disk...')

  # generate 10M file, tested
  file <- tempfile(tmpdir = test_dir)
  dat <- rand_string(file_size - 1)
  upload <- system.time(writeLines(dat, file, useBytes = TRUE), gcFirst = TRUE)

  progress$inc('Read from disk...')
  download <- system.time({dat_c <- readLines(file, n = 1)}, gcFirst = TRUE)

  if(exists('dat_c') && dat_c != dat){
    warning('Uploaded data is broken...')
  }

  ratio <- file.info(file)$size / 1000000

  speed <- ratio / c(upload[3], download[3])
  names(speed) <- NULL

  raveio_setopt('drive_speed', speed)

  class(speed) <- 'rave-units'
  attr(speed, 'unit') <- 'MB/s'
  attr(speed, 'labels') <- c('Write - ', 'Read - ')
  return(speed)
}



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

