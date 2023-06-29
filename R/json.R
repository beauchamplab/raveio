#' @title Save or load R object in 'JSON' format
#' @param x R object to save
#' @param con file or connection
#' @param digits number of digits to save
#' @param pretty whether the output should be pretty
#' @param serialize whether to save a serialized version of \code{x};
#' see 'Examples'.
#' @param map a map to save the results
#' @param ... other parameters to pass into \code{\link[jsonlite]{toJSON}} or
#' \code{\link[jsonlite]{fromJSON}}
#' @returns \code{save_json} returns nothing; \code{load_json} returns an
#' R object.
#'
#' @examples
#'
#'
#' # Serialize
#' save_json(list(a = 1, b = function(){}))
#'
#' # use toJSON
#' save_json(list(a = 1, b = function(){}), serialize = FALSE)
#'
#'
#' # Demo of using serializer
#' f1 <- tempfile(fileext = ".json")
#' save_json(x ~ y + 1, f1)
#'
#' load_json(f1)
#'
#' unlink(f1)
#'
#'
#' @export
save_json <- function(x, con = stdout(), ...,
                      digits = ceiling(-log10(.Machine$double.eps)),
                      pretty = TRUE, serialize = TRUE) {

  if(serialize){
    s <- jsonlite::serializeJSON(x, digits = digits, pretty = pretty)
  } else {
    s <- jsonlite::toJSON(x, digits = digits, pretty = pretty, ...)
  }

  writeLines(s, con)
  invisible()
}

#' @rdname save_json
#' @export
load_json <- function(con, ..., map = NULL){

  s <- readLines(con)
  args <- list(...)

  re <- tryCatch({
    jsonlite::unserializeJSON(s)
  }, error = function(e){
    args$txt <- s
    do.call(jsonlite::fromJSON, args)
  })
  if(is.list(re) && !is.null(map)){
    if(is.environment(map)){
      list2env(re, map)
    } else if (inherits(map, 'fastmap2')){
      dipsaus::list_to_fastmap2(re, map)
    } else if (inherits(map, "fastmap")){
      map$mset(.list = re)
    }
  }
  re
}

