#' @importFrom dipsaus %?<-%
#' @importFrom glue glue
NULL

#' @export
glue::glue


#' @title Print colored messages
#' @param ...,.envir passed to \code{\link[glue]{glue}}
#' @param level passed to \code{\link[dipsaus]{cat2}}
#' @param .pal see \code{pal} in \code{\link[dipsaus]{cat2}}
#' @export
catgl <- function(..., .envir = parent.frame(), level = 'DEBUG', .pal){
  if(missing(.pal)){
    dipsaus::cat2(glue::glue(..., .envir = .envir), level = level)
  }else{
    dipsaus::cat2(glue::glue(..., .envir = .envir), level = level, pal = .pal)
  }
}


deparse1 <- function(expr, collapse = ' '){
  paste(deparse(expr), collapse = collapse)
}


stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    stop(msg)
  }
}


