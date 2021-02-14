
filenames <- function(file, sep = "/|\\\\"){
  sapply(stringr::str_split(file, sep), function(x){
    x[[length(x)]]
  })
}

fileexts <- function(file){
  x <- filenames(file)
  sapply(stringr::str_split(file, '\\.'), function(x){
    l <- length(x)
    ifelse(l > 1, x[[l]], '')
  })
}


load_table <- function(file, ..., use.names = TRUE) {

  if(!length(file)){
    return()
  }

  if(length(file) == 1 && dir.exists(file)) {
    return(load_table.directory(file, ..., use.names = use.names))
  }

  if(length(file) > 1){
    return(data.table::rbindlist(lapply(file, load_table, ...), use.names = use.names))
  }


  if(!file.exists(file)){

    fname <- filenames(file)
    fdir <- dirname(file)
    fs <- list.files(fdir, full.names = FALSE, all.files = TRUE, recursive = FALSE,
               include.dirs = FALSE, no.. = TRUE)
    fl <- stringr::str_length(fname)
    fl <- stringr::str_sub(fs, end = fl + 1)
    sel <- fl == paste0(fname, '.')

    if(any(sel)){
      fnames <- fs[sel]
      exts <- fileexts(fnames)
      sel <- fnames == sprintf('%s.%s', fname, exts)
      if(any(sel)){
        fname <- fnames[sel][[1]]
        return(Recall(file.path(fdir, fname), ...))
      }
    }
    return()

  }

  # single file with extension
  fext <- stringr::str_match(file, '\\.([a-zA-Z0-9]+)$')[,2]

  funname <- sprintf('load_table.%s', stringr::str_to_lower(fext))
  return(do.call(funname, list(
    file = file,
    ...
  )))
}

load_table.directory <- function(file, recursive = FALSE, use.names = TRUE, ...){
  fs <- list.files(file, recursive = recursive, include.dirs = FALSE, full.names = TRUE)
  fs <- fs[!dir.exists(fs)]
  fs <- normalizePath(fs)
  data.table::rbindlist(lapply(fs, load_table, as.data.table = TRUE), ..., use.names = use.names)
}


load_table.csv <- function(file, sep = ',', stringsAsFactors = FALSE, ...){
  data.table::fread(file = file, sep = sep, stringsAsFactors = stringsAsFactors, ...)
}

load_table.tsv <- function(file, sep = '\t', stringsAsFactors = FALSE, ...){
  data.table::fread(file = file, sep = sep, stringsAsFactors = stringsAsFactors, ...)
}

load_table.fst <- function(file, ...){
  load_fst(file, ...)
}

