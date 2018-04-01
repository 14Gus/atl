#' Get the default export file path
#' @export
#
getDefaultExportFilePath <- function(){
  file.path(getwd(),"data")
}


#' Build the file name from its components. WIll add the date by default.
#'

buildExportFilePath <- function(filepath, filename, filextension, hash=NULL){
  if(!is.null(hash)){
    full_file_name <- paste0(filename, "-", Sys.Date(),"-", hash, ".", filextension)

  } else{
    full_file_name <- paste0(filename, "-", Sys.Date(), ".", filextension)
  }

  file.path(filepath, full_file_name)
}


#' Gets the path to the script that is being run

getCallingScriptPath <- function(){
  if(!exists("ofile", envir = sys.frame(1))){
    stop("Unable to get calling script when command run from console")
  }

  sys.frame(1)$ofile
}
