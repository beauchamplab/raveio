# File IO: HDF5 file wrapper
#
# ensure_rhdf5 <- function(prompt = TRUE){
#   if( dipsaus::package_installed('rhdf5') ){ return(TRUE) }
#
#   ans <- TRUE
#   if( prompt && interactive() && !dipsaus::shiny_is_running() ){
#     ans <- utils::askYesNo("BioConductor package `rhdf5` has not been installed. Do you want to install now?")
#   }
#   if(!isTRUE(ans)){
#     stop("Abort. Please manually call `BiocManager::install('rhdf5')` to install.")
#   }
#
#   BiocManager::install('rhdf5', update = FALSE, ask = FALSE, type = 'source')
# }
#
# h5FileIsOpen <- function (filename) {
#   filename <- normalizePath(filename, mustWork = FALSE)
#   L <- rhdf5::h5validObjects()
#   isobject <- sapply(L, function(x) {
#     tryCatch({
#       rhdf5::H5Iget_type(x) %in% c("H5I_FILE", "H5I_GROUP", "H5I_DATASET")
#     }, error = function(e){ FALSE })
#   })
#   if (length(isobject) > 0) {
#     isopen <- any(sapply(L[which(isobject)], function(x) {
#       tryCatch({
#         rhdf5::H5Fget_name(x) == filename
#       }, error = function(e){ return(FALSE) })
#     }))
#   }
#   else {
#     isopen <- FALSE
#   }
#   isopen
# }
#
# # used to close connections for writing
# H5FcloseAll <- function(filename) {
#   filename <- normalizePath(filename, mustWork = FALSE)
#   if(!h5FileValid(filename)){ return(0L) }
#   L <- rhdf5::h5validObjects()
#   isobject <- sapply(L, function(x) {
#     tryCatch({
#       rhdf5::H5Iget_type(x) %in% c("H5I_FILE", "H5I_GROUP", "H5I_DATASET")
#     }, error = function(e){ FALSE })
#   })
#   if (length(isobject) > 0 && any( isobject, na.rm = TRUE )) {
#     nclosed <- sapply(L[which(isobject)], function(x) {
#       tryCatch({
#         if(rhdf5::H5Fget_name(x) == filename){
#           # close
#           itype <- rhdf5::H5Iget_type(x)
#           switch (itype,
#                   'H5I_FILE' = {
#                     rhdf5::H5Fclose(x)
#                   },
#                   'H5I_GROUP' = {
#                     rhdf5::H5Gclose(x)
#                   }, {
#                     rhdf5::H5Dclose(x)
#                   }
#           )
#           return( 1L )
#         } else {
#           return( 0L )
#         }
#       }, error = function(e){
#         return( 0L )
#       })
#
#     })
#     return( sum(nclosed) )
#   } else {
#     return( 0L )
#   }
# }
#
# H5FcloseOthers <- function(h5obj, filename, exclude = NULL) {
#   tryCatch({
#     # message(3)
#     if(missing(filename)){
#       filename <- rhdf5::H5Fget_name(h5obj)
#     }
#     filename <- normalizePath(filename, mustWork = FALSE)
#     if(!h5FileValid(filename)){ return(0L) }
#     L <- rhdf5::h5validObjects()
#     isobject <- sapply(L, function(x) {
#       rhdf5::H5Iget_type(x) %in% c("H5I_FILE", "H5I_GROUP", "H5I_DATASET")
#     })
#     if (length(isobject) > 0 && any( isobject, na.rm = TRUE )) {
#       exclude <- c(exclude, h5obj)
#       exclude_id <- unlist(lapply(exclude, function(x){
#         if(!isS4(x)){ return(NULL) }
#         return( x@ID )
#       }))
#       nclosed <- sapply(L[which(isobject)], function(x) {
#         if(rhdf5::H5Fget_name(x) == filename){
#           # identical to h5obj
#           if(isTRUE(h5obj@ID %in% exclude_id)){ return( 0L ) }
#           # close
#           itype <- rhdf5::H5Iget_type(x)
#           switch (itype,
#                   'H5I_FILE' = {
#                     rhdf5::H5Fclose(x)
#                   },
#                   'H5I_GROUP' = {
#                     rhdf5::H5Gclose(x)
#                   },
#                   H5I_DATASET = {
#                     rhdf5::H5Dclose(x)
#                   }
#           )
#           return( 1L )
#         } else {
#           return( 0L )
#         }
#       })
#       return( sum(nclosed) )
#     } else {
#       return( 0L )
#     }
#
#   }, error = function(e){
#     0L
#   })
# }
#
# h5FileValid <- function(filename){
#   if(!length(filename)){ return(FALSE) }
#   filename <- filename[[1]]
#   if(!file.exists(filename)){ return(FALSE) }
#   if(isTRUE(file.info(filename)[['isdir']])){ return(FALSE) }
#   filename <- normalizePath(filename)
#   return(tryCatch({
#     rhdf5::H5Fis_hdf5(filename)
#   }, error = function(e){ FALSE }))
# }
#
# h5FileObject <- function(filename){
#   filename <- normalizePath(filename, mustWork = FALSE)
#   if(!h5FileValid(filename)){ return(NULL) }
#   L <- rhdf5::h5validObjects()
#   for(x in L){
#     try({
#       if(rhdf5::H5Iget_type(x) %in% c("H5I_FILE")){
#         if(rhdf5::H5Fget_name(x) == filename){
#           return(x)
#         }
#       }
#     }, silent = TRUE)
#   }
#   return(NULL)
# }
#
# h5fileHasData <- function(filename, dataname){
#   if(!h5FileValid(filename)){ return(FALSE) }
#   fobj <- h5FileObject(filename)
#   df <- NULL
#   try({
#     if(!is.null(fobj)){
#       df <- rhdf5::h5ls(fobj, recursive = TRUE)
#     }
#   }, silent = TRUE)
#   if(is.null(df)){
#     df <- rhdf5::h5ls(filename, recursive = TRUE)
#   }
#
#   dnames <- sprintf('%s/%s', df$group, df$name)
#   dnames <- dnames[df$otype == 'H5I_DATASET']
#   dnames <- stringr::str_remove_all(dnames, '^[/]+')
#   dataname <- stringr::str_remove_all(dataname, '^[/]+')
#   dataname %in% dnames
# }
#
# h5guessChunk <- function (
#   space_maxdims,
#   chunk_size = getOption("raveio.h5.chunk_size", 4096)
# ) {
#   chunk_num_elem <- floor(chunk_size)
#   space_rank <- length(space_maxdims)
#   chunk_dim <- rep(ceiling(chunk_num_elem^(1/space_rank)),
#                    space_rank)
#   chunk_dim <- pmin(space_maxdims, chunk_dim)
#   bounded <- chunk_dim == space_maxdims
#   while (prod(chunk_dim) < chunk_num_elem & !all(bounded)) {
#     mult_factor <- (chunk_num_elem/prod(chunk_dim))^(1/sum(!bounded))
#     chunk_dim[!bounded] <- ceiling(chunk_dim[!bounded] *
#                                      mult_factor)
#     chunk_dim <- pmin(space_maxdims, chunk_dim)
#     bounded <- chunk_dim == space_maxdims
#   }
#   return(chunk_dim)
# }
#
# h5dataType <- function (storage.mode, size = 255L) {
#   tid <- switch(
#     storage.mode[1],
#     double = 'H5T_IEEE_F64LE',
#     integer = 'H5T_STD_I32LE',
#     integer64 = 'H5T_STD_I64LE',
#     logical = 'H5T_STD_I8LE',
#     raw = 'H5T_STD_U8LE',
#     character = 'H5T_STRING',
#     # {
#     #   tid <- H5Tcopy("H5T_C_S1")
#     #   H5Tset_strpad(tid, strpad = "NULLPAD")
#     #   if (!is.numeric(size)) {
#     #     stop("parameter 'size' has to be defined for storage.mode character.")
#     #   }
#     #   H5Tset_size(tid, size)
#     #   tid
#     # },
#     {
#       stop("datatype ", storage.mode, " not yet implemented.\n",
#            "Try 'logical', 'double', 'integer', 'integer64' or 'character'.")
#     }
#   )
# }

