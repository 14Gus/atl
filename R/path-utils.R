#' Get the default export file path
#' @export
#
getDefaultExportFilePath <- function(){
  file.path(getwd(),"data")
}


#' Build the file name from its components. Will add the date by default.
#' @param filepath Filepath to save directory
#' @param filename Name of the file
#' @param file_extension File extension or file type (.csv, .rds etc)
#' @param hash A hash to append to the file name
#' @param include_date Whether to include the date in the file name.

buildExportFileName <- function(filename, file_extension, filepath = getDefaultExportFilePath(), hash=NULL, include_date = TRUE){

  if(include_date){
    today_date <- as.character(Sys.Date())
  }

  full_file_name <- paste0(c(filename, today_date, hash), collapse="-")

  full_file_name <- paste(full_file_name, file_extension, sep=".")

  file.path(filepath, full_file_name)
}


#' Gets the path to the script that is being run

getCallingScriptPath <- function(){
  if(!exists("ofile", envir = sys.frame(1))){
    stop("Unable to get calling script when command run from console")
  }

  sys.frame(1)$ofile
}
