#' Save Data to csv Files but Rename Old File Instead of Overwrite
#' @param x,file see also \code{\link[utils]{write.csv}}
#' @param quiet whether to suppress overwrite message
#' @param ... pass to \code{\link[utils]{write.csv}}
#' @return Normalized path of \code{file}
#' @export
safe_write_csv <- function(x, file, ..., quiet = FALSE){
  if(file.exists(file)){
    oldfile = stringr::str_replace(file, '\\.[cC][sS][vV]$', strftime(Sys.time(), '_[%Y%m%d_%H%M%S].csv'))
    if(!quiet){
      catgl('Renaming file {file} >> {oldfile}')
    }
    file.rename(file, oldfile)
  }
  args = list(...)
  rn <- args$row.names

  tryCatch({
    utils::write.csv(x, file, ...)
  }, error = function(e){

    call_args <- list(x = quote(x), file = file)
    call_args$col.names <- if (is.logical(rn) && !rn) TRUE else NA
    call_args$sep <- ","
    call_args$dec <- "."
    call_args$qmethod <- "double"

    do.call(utils::write.table, call_args)
  })
  normalizePath(file)
}



#' Read csv Files with Given Column Classes
#' @param file,header,sep,colClasses,skip,quote,stringsAsFactors,...
#' passed to \code{\link[utils]{read.csv}}
#' @return A data frame
#' @details
#' Reading a csv file using builtin function
#' \code{\link[utils]{read.csv}} might result in some unexpected
#' behavior. \code{safe_read_csv} does some preprocessing on the
#' format so that it take cares of the following cases.
#'
#' 1. If \code{skip} exceeds the maximum rows of the data, return
#' a blank data frame instead of raising error.
#'
#' 2. If row names are included in the file, \code{colClasses}
#' automatically skip that column and starts from the second column
#'
#' 3. If length of \code{colClasses} does not equal to the number of
#' columns, instead of cycling the class types, we set those columns
#' to be \code{NA} type and let \code{\link[utils]{read.csv}} decide
#' the default types.
#'
#' 4. \code{stringsAsFactors} is by default \code{FALSE} to be
#' consistent with R 4.0, if the function is called in R 3.x.
#' @export
safe_read_csv <- function(file, header = TRUE, sep = ',',
                          colClasses = NA, skip = 0, quote = "\"",
                          ..., stringsAsFactors = FALSE){

  s = readLines(file, n = skip+1, ok = TRUE)

  # Reach EOF
  if(length(s) != skip+1){
    return(data.frame())
  }

  # Parse headers
  s = s[skip+1]
  s = strsplit(s, sep)[[1]]
  s = stringr::str_remove_all(s, quote)

  # If length(s) == 1
  if(length(s) == 1){
    colClasses = colClasses[1]
    return(utils::read.csv(file = file, header = header, sep = sep,
                           colClasses = colClasses, skip = skip,
                           quote = quote, ..., stringsAsFactors = stringsAsFactors))
  }


  if(!header || s[1] != ''){
    col_class =  rep(NA, length(s))
    col_class[seq_along(colClasses)] = colClasses
    return(utils::read.csv(file = file, header = header, sep = sep,
                           colClasses = col_class, skip = skip,
                           quote = quote, ..., stringsAsFactors = stringsAsFactors))
  }else{
    # first blank header will be rownames
    col_class =  rep(NA, length(s))
    col_class[seq_along(colClasses) + 1] = colClasses
    dat = utils::read.csv(file = file, header = header, sep = sep,
                          colClasses = col_class, skip = skip,
                          quote = quote, ..., stringsAsFactors = stringsAsFactors)
    row.names(dat) = dat[,1]
    dat = dat[,-1]
    return(dat)
  }

  utils::read.csv(file = file, header = header, sep = sep,
                  colClasses = colClasses, skip = skip,
                  quote = quote, ..., stringsAsFactors = stringsAsFactors)
}


#' Read csv file and ignore headers
#' @description Resolved some iEEG format import situation when
#' the header could be missing.
#' @param file csv file to read from. The file must contains all
#' numerical values
#' @param nrows number of rows to read
#' @param drop passed to \code{\link[data.table]{fread}}
#' @details The function checks the first two rows of csv file.
#' If the first row has different \code{\link{storage.mode}} than
#' the second row, then the first row is considered header, otherwise
#' header is treated missing. Note \code{file} must have at least two
#' rows.
#' @export
read_csv_no_header <- function(file, nrows = Inf, drop = NULL){
  header1 = utils::read.csv(file = file, nrows = 1, header = FALSE)
  header2 = utils::read.csv(file = file, nrows = 1, header = FALSE, skip = 1)

  ncols <- ncol(header1)

  same_mode <- sapply(seq_len(ncols), function(ii){
    storage.mode(header1[[ii]]) == storage.mode(header2[[ii]])
  })

  if(all(same_mode)){
    header = FALSE
  }else{
    header = TRUE
  }
  data.table::fread(file = file, header = header, nrows = nrows, drop = drop, fill = TRUE)
}


