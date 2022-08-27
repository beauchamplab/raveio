# validate

#' @title Check If Input Has Blank String
#' @param x input data: a vector or an array
#' @return \code{x == ""}
#' @export
is.blank <- function(x){
  x == ''
}

#' @title Check If Input Has Zero Length
#' @param x input data: a vector, list, or array
#' @return whether \code{x} has zero length
#' @export
is.zerolenth <- function(x){
  length(x) == 0
}

rand_string <- function(length = 50){
  pid <- as.integer(Sys.getpid())
  now <- as.numeric(Sys.time() - as.POSIXlt(Sys.Date()), units = "secs")
  now <- sprintf("%.24f", now)
  now <- strsplit(now, "\\.")[[1]]
  now2 <- strsplit(now[[2]], "")[[1]]
  now <- as.integer(c(
    paste(now2[c(1,5,9,13,17,21) + 3], collapse = ""),
    paste(now2[c(1,5,9,13,17,21) + 2], collapse = ""),
    paste(now2[c(1,5,9,13,17,21) + 1], collapse = ""),
    paste(now2[c(1,5,9,13,17,21)], collapse = ""),
    now[[1]]
  ))
  now <- rev(as.integer(now))

  dict0 <- dipsaus::digest(paste(pid, now), algo = "xxhash32", seed = pid)
  dict1 <- dipsaus::digest(paste(pid, now, dict0), algo = "xxhash32", seed = now[[1]])
  dict2 <- dipsaus::digest(paste(pid, now, dict1), algo = "murmur32", seed = sum(now))
  dict3 <- dipsaus::digest(paste(pid, now, dict1, dict2), algo = "xxhash64",
                           seed = strtoi(sprintf("0x%s", substr(dict2, 1, 7))))

  dict <- strsplit(paste0(dict3, dict2, dict1), "")[[1]]
  # dict <- c(dict, letters, LETTERS, 0:9)

  paste(sample(dict, size = length, replace = TRUE), collapse = "")
  # c(dict1, dict2, dict3)
}

#' Check if data is close to ``valid"
#' @param x data to check
#' @param min_len,max_len minimal and maximum length
#' @param mode which storage mode (see \code{\link{mode}})
#' should \code{x} be considered valid. Default is \code{NA}: disabled.
#' @param na whether \code{NA} values considered invalid?
#' @param blank whether blank string considered invalid?
#' @param all if \code{na} or \code{blank} is true, whether all element
#' of \code{x} being invalid will result in failure?
#' @return logicals whether input \code{x} is valid
#'
#' @examples
#'
#' # length checks
#' is_valid_ish(NULL)                     # FALSE
#' is_valid_ish(integer(0))               # FALSE
#' is_valid_ish(integer(0), min_len = 0)  # TRUE
#' is_valid_ish(1:10, max_len = 9)        # FALSE
#'
#' # mode check
#' is_valid_ish(1:10)                     # TRUE
#' is_valid_ish(1:10, mode = 'numeric')   # TRUE
#' is_valid_ish(1:10, mode = 'character') # FALSE
#'
#' # NA or blank checks
#' is_valid_ish(NA)                     # FALSE
#' is_valid_ish(c(1,2,NA), all = FALSE) # FALSE
#' is_valid_ish(c(1,2,NA), all = TRUE)  # TRUE as not all elements are NA
#'
#' is_valid_ish(c('1',''), all = FALSE)  # TRUE
#' is_valid_ish(1:3, all = FALSE)        # TRUE as 1:3 are not characters
#'
#'
#' @export
is_valid_ish <- function(x, min_len = 1, max_len = Inf, mode = NA,
                         na = TRUE, blank = FALSE, all = FALSE){
  if(!is.na(mode) && mode(x) != mode){
    return(FALSE)
  }
  len <- length(x)
  if(len < min_len || len > max_len){
    return(FALSE)
  }
  if(len == 0){
    return(TRUE)
  }
  if(na){
    if(!all && any(is.na(x))){
      return(FALSE)
    }
    if(all && all(!is.na(x))){
      return(FALSE)
    }
  }
  if(blank && mode(x) == 'character'){
    if(!all && any(is.blank(x), na.rm = TRUE)){
      return(FALSE)
    }
    if(all && all(!is.blank(x), na.rm = TRUE)){
      return(FALSE)
    }
  }
  return(TRUE)
}



#' @title Get value or return default if invalid
#' @param x a list, or environment, or just any R object
#' @param key the name to obtain from \code{x}. If \code{NA}, then
#' return x. Default is \code{NA}
#' @param default default value if
#' @param na,min_len,... passed to \code{\link{is_valid_ish}}
#' @return values of the keys or default is invalid
#' @examples
#'
#' x <- list(a=1, b = NA, c = character(0))
#'
#' # ------------------------ Basic usage ------------------------
#'
#' # no key, returns x if x is valid
#' get_val2(x)
#'
#' get_val2(x, 'a', default = 'invalid')
#'
#'
#'
#' # get 'b', NA is not filtered out
#' get_val2(x, 'b', default = 'invalid')
#'
#' # get 'b', NA is considered invalid
#' get_val2(x, 'b', default = 'invalid', na = TRUE)
#'
#'
#'
#' # get 'c', length 0 is allowed
#' get_val2(x, 'c', default = 'invalid', min_len = 0)
#'
#' # length 0 is forbidden
#' get_val2(x, 'c', default = 'invalid', min_len = 1)
#'
#'
#' @export
get_val2 <- function(x, key = NA, default = NULL, na=FALSE, min_len=1L, ...){

  if(is.null(key) || is.na(key)){
    val <- x
  }else{
    val <- x[[key]]
  }
  if(!is_valid_ish(val, na = na, min_len = min_len, ...)){
    return(default)
  }
  return(val)
}


#' @title Validate time windows to be used
#' @description Make sure the time windows are valid intervals and returns
#' a reshaped window list
#' @param time_windows vectors or a list of time intervals
#' @return A list of time intervals (ordered, length of 2)
#' @examples
#'
#'
#' # Simple time window
#' validate_time_window(c(-1, 2))
#'
#' # Multiple windows
#' validate_time_window(c(-1, 2, 3, 5))
#'
#' # alternatively
#' validate_time_window(list(c(-1, 2), c(3, 5)))
#' validate_time_window(list(list(-1, 2), list(3, 5)))
#'
#'
#' \dontrun{
#'
#' # Incorrect usage (will raise errors)
#'
#'   # Invalid interval (length must be two for each intervals)
#'   validate_time_window(list(c(-1, 2, 3, 5)))
#'
#'   # Time intervals must be in ascending order
#'   validate_time_window(c(2, 1))
#'
#' }
#'
#'
#' @export
validate_time_window <- function(time_windows){
  if(!is.list(time_windows)){
    time_windows <- unlist(time_windows)
    if(length(time_windows) %% 2 != 0){
      stop("`time_windows` must be a list of time intervals (length 2)")
    }
    time_windows <- matrix(time_windows, nrow = 2, byrow = FALSE)
    time_windows <- as.list(as.data.frame(time_windows))
    time_windows <- unname(time_windows)
  }
  time_windows <- lapply(time_windows, function(x){
    if(is.list(x)){
      x <- unlist(x)
    }
    if(length(x) != 2){
      stop("`time_windows` must be a list of time intervals (length 2)")
    }
    if(!is.numeric(x)){
      stop("`time_windows` must be a list of 'numerical' time intervals")
    }
    if(anyNA(x)){
      stop("`time_windows` cannot contain NAs")
    }
    if(x[[1]] > x[[2]]){
      stop("`time_windows` time intervals must be in ascending order")
    }
    x
  })
  time_windows
}

