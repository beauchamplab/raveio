#' Save data to comma separated value files with backups
#' @description Save comma separated value files, if file exists,
#' backup original file.
#' @param x,file,... pass to \code{write.csv}
#' @param quiet whether to suppress overwrite message
#' @returns Normalized path of \code{file}
#'
#' @examples
#'
#' f <- tempfile()
#' x <- data.frame(a = 1:10)
#'
#' # File not exists, same as write file, returns normalized `f`
#' safe_write_csv(x, f)
#'
#' # Check whether file exists
#' file.exists(f)
#'
#' # write again, and the old file will be copied
#' safe_write_csv(x, f)
#'
#' @export
safe_write_csv <- function(x, file, ..., quiet = FALSE){
  if(file.exists(file)){
    regexp <- stringr::regex('(\\.csv|)$', ignore_case = TRUE)
    oldfile <- stringr::str_replace(file, regexp, strftime(Sys.time(), '_[%Y%m%d_%H%M%S].csv'))
    if(!quiet){
      catgl('Renaming file {file}\n  >> {oldfile}')
    }
    file.rename(file, oldfile)
  }
  args <- list(...)
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



#' Read comma separated value files with given column classes
#' @param file,header,sep,colClasses,skip,quote,stringsAsFactors,...
#' passed to \code{read.csv}
#' @returns A data frame
#' @details
#' Reading a comma separated value file using builtin function
#' \code{read.csv} might result in some unexpected
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
#' to be \code{NA} type and let \code{read.csv} decide
#' the default types.
#'
#' 4. \code{stringsAsFactors} is by default \code{FALSE} to be
#' consistent with R 4.0, if the function is called in R 3.x.
#'
#' @examples
#'
#' f <- tempfile()
#' x <- data.frame(a = letters[1:10], b = 1:10, c = 2:11)
#'
#' # ------------------ Auto-detect row names ------------------
#' # Write with rownames
#' utils::write.csv(x, f, row.names = LETTERS[2:11])
#'
#' # read csv with base library utils
#' table1 <- utils::read.csv(f, colClasses = c('character', 'character'))
#'
#' # 4 columns including row names
#' str(table1)
#'
#' # read csv via safe_read_csv
#' table2 <- safe_read_csv(f, colClasses = c('character', 'character'))
#'
#' # row names are automatically detected, hence 3 columns
#' # Only first columns are characters, the third column is auto
#' # detected as numeric
#' str(table2)
#'
#' # read table without row names
#' utils::write.csv(x, f, row.names = FALSE)
#' table2 <- safe_read_csv(f, colClasses = c('character', 'character'))
#'
#' # still 3 columns, and row names are 1:nrow
#' str(table2)
#'
#' # --------------- Blank data frame when nrow too large ---------------
#' # instead of raising errors, return blank data frame
#' safe_read_csv(f, skip = 1000)
#'
#'
#' @export
safe_read_csv <- function(file, header = TRUE, sep = ',',
                          colClasses = NA, skip = 0, quote = "\"",
                          ..., stringsAsFactors = FALSE){

  s <- readLines(file, n = skip+1, ok = TRUE)

  # Reach EOF
  if(length(s) != skip+1){
    return(data.frame())
  }

  # Parse headers
  s <- s[skip+1]
  s <- strsplit(s, sep)[[1]]
  s <- stringr::str_remove_all(s, quote)

  # If length(s) == 1
  if(length(s) == 1){
    colClasses <- colClasses[1]
    return(utils::read.csv(file = file, header = header, sep = sep,
                           colClasses = colClasses, skip = skip,
                           quote = quote, ..., stringsAsFactors = stringsAsFactors))
  }


  if(!header || s[1] != ''){
    col_class <- rep(NA, length(s))
    col_class[seq_along(colClasses)] <- colClasses
    return(utils::read.csv(file = file, header = header, sep = sep,
                           colClasses = col_class, skip = skip,
                           quote = quote, ..., stringsAsFactors = stringsAsFactors))
  }else{
    # first blank header will be rownames
    col_class <- rep(NA, length(s))
    col_class[seq_along(colClasses) + 1] <- colClasses
    dat <- utils::read.csv(file = file, header = header, sep = sep,
                          colClasses = col_class, skip = skip,
                          quote = quote, ..., stringsAsFactors = stringsAsFactors)
    row.names(dat) <- dat[,1]
    dat <- dat[,-1]
    return(dat)
  }

  utils::read.csv(file = file, header = header, sep = sep,
                  colClasses = colClasses, skip = skip,
                  quote = quote, ..., stringsAsFactors = stringsAsFactors)
}


#' Read comma separated value file and ignore headers
#' @description Resolved some irregular 'iEEG' format where
#' the header could be missing.
#' @param file comma separated value file to read from. The file must contains
#' all numerical values
#' @param nrows number of rows to read
#' @param drop passed to \code{\link[data.table]{fread}}
#' @details The function checks the first two rows of comma separated value file
#' If the first row has different \code{\link{storage.mode}} than
#' the second row, then the first row is considered header, otherwise
#' header is treated missing. Note \code{file} must have at least two
#' rows.
#' @export
read_csv_ieeg <- function(file, nrows = Inf, drop = NULL){
  header1 <- utils::read.csv(file = file, nrows = 1, header = FALSE)
  header2 <- utils::read.csv(file = file, nrows = 1, header = FALSE, skip = 1)

  ncols <- ncol(header1)

  same_mode <- sapply(seq_len(ncols), function(ii){
    storage.mode(header1[[ii]]) == storage.mode(header2[[ii]])
  })

  if(all(same_mode)){
    header <- FALSE
  }else{
    header <- TRUE
  }
  data.table::fread(file = file, header = header, nrows = nrows, drop = drop, fill = TRUE)
}


