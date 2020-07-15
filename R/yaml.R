#' @title A port to \code{\link[yaml]{read_yaml}}
#' @param file,... passed to \code{\link[yaml]{read_yaml}}
#' @param map \code{\link[dipsaus]{fastmap2}} instance or \code{NULL}
#' @return A \code{\link[dipsaus]{fastmap2}}. If \code{map} is provided
#' then return map, otherwise return newly created one
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
#' @export
save_yaml <- function(x, file, ...){
  yaml::write_yaml(as.list(x), file = file, ...)
  invisible(normalizePath(file))
}



