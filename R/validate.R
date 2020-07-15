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
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = '')
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



#' @title Get Value or Default if Value is Invalid
#' @param x a list, or environment, or just any R object
#' @param key the name to obtain from \code{x}. If \code{NA}, then
#' return x. Default is \code{NA}
#' @param default default value if
#' @param na,min_len,... passed to \code{\link{is_valid_ish}}
#' @export
get_val2 <- function(x, key = NA, default = NULL, na=FALSE, min_len=1L, ...){

  if(is.null(key) || is.na(key)){
    val = x
  }else{
    val = x[[key]]
  }
  if(!is_valid_ish(val, na = na, min_len = min_len, ...)){
    return(default)
  }
  return(val)
}


