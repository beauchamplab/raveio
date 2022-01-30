filearray_checkload_or_remove <- function(filebase, mode = "readonly", symlink_ok = TRUE, ...){
  if(file.exists(filebase)){
    tryCatch({
      return(filearray::filearray_checkload(
        filebase = filebase, mode = mode,
        symlink_ok = symlink_ok, ...
      ))
    }, error = function(e){
      unlink(filebase, recursive = TRUE, force = TRUE)
    })
  }
  return(NULL)
}

filearray_create2 <- function(filebase, ..., dimnames = NULL){
  dname <- dirname(filebase)
  if(!dir.exists(dname)){
    dir_create2(dname)
  }

  re <- filearray::filearray_create(filebase = filebase, ...)
  if(length(dimnames)){
    mode <- re$.mode
    on.exit({ re$.mode <- mode }, add = FALSE)
    dimnames(re) <- dimnames
    re$.mode <- mode
    on.exit({}, add = FALSE)
  }
  re
}
