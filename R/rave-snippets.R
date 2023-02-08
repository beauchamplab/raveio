#' @name rave-snippet
#' @title 'RAVE' code snippets
#' @description Run snippet code
#' @param topic snippet topic
#' @param local whether to use local snippets first before requesting online
#' repository
#' @param force whether to force updating the snippets; default is true
#' @returns `load_snippet` returns snippet as a function, others return nothing
#' @examples
#'
#' if(interactive()) {
#'
#'   update_local_snippet()
#'   snippet <- load_snippet("dummy-snippet")
#'   snippet
#' }
#'
NULL

#' @rdname rave-snippet
#' @export
update_local_snippet <- function(force = TRUE) {
  root_path <- R_user_dir(package = "raveio", which = "cache")
  snippet_path <- file.path(root_path, "rave-gists-main")
  if(dir.exists(snippet_path)) {
    if(!force) { return() }
    unlink(snippet_path, recursive = TRUE, force = TRUE)
  }
  tmpfile <- tempfile(fileext = ".zip")

  utils::download.file(
    "https://github.com/dipterix/rave-gists/archive/refs/heads/main.zip",
    destfile = tmpfile)
  utils::unzip(tmpfile, exdir = root_path)
}

#' @rdname rave-snippet
#' @export
load_snippet <- function(topic, local = TRUE) {

  fname <- sprintf("%s.R", topic)
  if(!isFALSE(local)) {
    if(isTRUE(local)) {
      update_local_snippet(force = FALSE)
      path <- file.path(R_user_dir(package = "raveio", which = "cache"), "rave-gists-main", fname)
    } else {
      path <- file.path(local, fname)
    }
    if(!file.exists(path)) {
      warning("Cannot find local snippet [", topic, "]. Please make sure the repository is up-to-date and the topic name is correct. Trying snippets")
      local <- FALSE
    }
  }

  if(isFALSE(local)) {
    path <- sprintf("https://raw.githubusercontent.com/dipterix/rave-gists/main/%s", fname)
  }

  # load scripts
  s <- trimws(readLines(path))

  # find documentation
  end_of_doc <- c(which(s == "#' END OF DOC"), length(s))[[1]]
  end_of_doc <- max(end_of_doc - 1L, 0L)

  docs <- s[seq_len(end_of_doc)]
  docs <- docs[startsWith(docs, "#'")]

  # get inputs
  params <- trimws(gsub("^#'", "", docs))
  params <- params[grepl("^@param [^\\ ]+ ", params)]
  params <- unlist(lapply(strsplit(params, " "), function(x){ x[[2]] }))

  params <- unique(c(params, "..."))

  args <- NULL
  missing_arg <- alist(params = )
  for(nm in params) {
    if(nm != "") {
      names(missing_arg) <- nm
      args <- c(args, missing_arg)
    }
  }

  fbody <- parse(text = c("{", s, "}"))[[1]]
  f <- dipsaus::new_function2(
    args = args,
    body = fbody,
    quote_type = "quote",
    env = new.env(parent = globalenv())
  )

  attr(f, "docs") <- docs
  attr(f, "args") <- params
  attr(f, "path") <- path
  attr(f, "topic") <- topic

  class(f) <- c("rave_snippet", class(f))

  f
}

#' @export
print.rave_snippet <- function(x, ...) {
  topic <- sprintf("<RAVE code snippet: [%s]>", attr(x, "topic"))
  path <- sprintf("Snippet path: %s", attr(x, "path"))

  docs <- gsub("^#'[ ]{0,1}", "", attr(x, "docs"))

  docs <- paste0(ifelse(startsWith(docs, "@"), "  ", "    "), docs)

  docs <- docs[docs != ""]

  usage <- sprintf("  @usage: snippet(%s)",
                   paste(attr(x, "args"), collapse = ", "))

  cat(paste(c(topic , path, docs, "", usage, ""), collapse = "\n"))
  return(invisible(x))
}
