
#'Get the table of interest
#'
#'@param dataset Dataset of interest. To get a dataset stored in the global environment choose "local"
#'@param table Table of interest
#'@param source Source of interest
#'@export

getTable <- function(table, dataset=NULL, source=NULL) {
  # Capture table name as a quosure
  table <- rlang::enquo(table)

  rlang::eval_tidy(table)
}


#' Export table of interest
#'
#'@param table Table to export
#' @param filename The name of your file
#' @param filetype The type of your file. Options are "csv" to save as a comma separated csv file, "rds" to save an Rdata file.
#' @param filepath File path to save to.
#' @param ... parameters to pass to the file parsing function
#' @export

exportTable <- function(table, filename, filetype = c("csv","rds"), filepath=getDefaultExportFilePath(), ...){

  if(!dir.exists(filepath)){
    dir.create(filepath)
  }

  filetype <- match.arg(filetype)

  write_fun <- switch(filetype,
         "csv" = readr::write_csv,
         "rds" = readRDS)

  calling_script_path <- getCallingScriptPath()

  hash <- generateHashFromScript(calling_script_path)

  file_path <- buildExportFilePath(filepath, filename, filetype, hash)

  #write_fun(table, file_path, ...)

  message(paste("Table saved to", file_path))

  #Message dependent files tables found

  # Message file input statement
}

