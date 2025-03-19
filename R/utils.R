get_os <- function() {
  os <- R.version$os
  if (grepl("^darwin", os, ignore.case = TRUE)) {
    return("darwin")
  }
  if (grepl("^linux", os, ignore.case = TRUE)) {
    return("linux")
  }
  if (grepl("^solaris", os, ignore.case = TRUE)) {
    return("solaris")
  }
  if (grepl("^win", os, ignore.case = TRUE)) {
    return("windows")
  }
  if (grepl("^(emscr|wasm)", os, ignore.case = TRUE)) {
    return("emscripten")
  }
  return("unknown")
}

normalize_path <- function(path, must_work = NA) {
  path <- unlist(lapply(path, function(p) {
    if(!file.exists(p)) {
      dname <- dirname(p)
      dname <- normalizePath(dname, winslash = "/", mustWork = must_work)
      p <- file.path(dname, basename(p), fsep = "/")
    } else {
      p <- normalizePath(p, winslash = "/", mustWork = must_work)
    }
    p
  }))

  gsub("[/|\\\\]+", "/", path)
}

file_move <- function(from, to) {
  if(package_installed("fs")) {
    fs <- asNamespace("fs")
    impl <- fs$file_move
    if(is.function(impl)) {
      impl(path = from, new_path = to)
      return(invisible(to))
    }
  }
  file.rename(from = from, to = to)
  return(invisible(to))
}



safe_system <- function(cmd, ..., intern = TRUE, ignore.stderr = TRUE,
                        minimized = TRUE, invisible = TRUE, show.output.on.console = TRUE){
  suppressWarnings({
    if(get_os() == 'windows'){
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr,
                    minimized = minimized, invisible = invisible,
                    show.output.on.console = show.output.on.console, ...)
    } else {
      ret <- system(cmd, intern = intern, ignore.stderr = ignore.stderr, ...)
    }
  })
  ret
}

safe_system2 <- function(cmd, args, ..., stdout = TRUE, stderr = FALSE, onFound = NULL, onNotFound = NA){

  if(Sys.which(cmd) == ""){
    return(onNotFound)
  }

  suppressWarnings({
    ret <- system2(cmd, args, ..., stdout = stdout, stderr = stderr)
  })
  if(is.function(onFound)){
    ret <- onFound(ret)
  }
  ret
}

