#' @title A port to \code{\link[yaml]{read_yaml}}
#' @description For more examples, see \code{\link{save_yaml}}.
#' @param file,... passed to \code{\link[yaml]{read_yaml}}
#' @param map \code{\link[dipsaus]{fastmap2}} instance or \code{NULL}
#' @return A \code{\link[dipsaus]{fastmap2}}. If \code{map} is provided
#' then return map, otherwise return newly created one
#' @seealso \code{\link[dipsaus]{fastmap2}}, \code{\link{save_yaml}},
#' \code{\link[yaml]{read_yaml}}, \code{\link[yaml]{write_yaml}}
#'
#' @export
load_yaml <- function(file, ..., map = NULL){
  re <- yaml::read_yaml(file = file, ...)
  if(!inherits(map, 'fastmap2')){
    map <- dipsaus::fastmap2()
  }
  for(nm in names(re)){
    if(nm != ''){
      map[[nm]] <- re[[nm]]
    }
  }
  map
}


#' @title Write named list to file
#' @param x a named list, \code{\link[dipsaus]{fastmap2}}, or anything
#' that can be transformed into named list via \code{as.list}
#' @param file,... passed to \code{\link[yaml]{write_yaml}}
#' @return Normalized file path
#' @param sorted whether to sort the results by name; default is false
#' @seealso \code{\link[dipsaus]{fastmap2}}, \code{\link{load_yaml}},
#' \code{\link[yaml]{read_yaml}}, \code{\link[yaml]{write_yaml}}
#'
#' @examples
#'
#'
#' x <- list(a = 1, b = 2)
#' f <- tempfile()
#'
#' save_yaml(x, f)
#'
#' load_yaml(f)
#'
#' map <- dipsaus::fastmap2(missing_default = NA)
#' map$c <- 'lol'
#' load_yaml(f, map = map)
#'
#' map$a
#' map$d
#'
#'
#' @export
save_yaml <- function(x, file, ..., sorted = FALSE){
  if(inherits(x, 'fastmap')) {
    x <- x$as_list(sort = sorted)
  } else if(inherits(x, 'fastmap2')) {
    x <- x[["@as_list"]](sort = sorted)
  } else if(inherits(x, c('fastqueue', 'fastqueue2'))) {
    x <- x$as_list()
  } else if(sorted){
    x <- as.list(x, sorted = sorted, ...)
  } else {
    # as.list generics only requires `x`, therefore `sorted` may cause errors
    x <- as.list(x, ...)
  }
  yaml::write_yaml(x, file = file, ...)
  if(!inherits(file, "connection")) {
    file <- normalizePath(file)
  }
  invisible(file)
}



