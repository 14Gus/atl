
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

  data_file_path <- buildExportFileName(filepath, filename, filetype, hash)

  #Write to disk
  write_fun(table, file_path, ...)

  message(paste("Data saved to", data_file_path))

  suggested_file_name <- as.character(paste0(filename,'_path'))

  message(glue::glue("Copy and paste the following command to your script to reuse it for later:\n
          {suggest_file_name} <- {data_file})"))

  data_dependencies <- findDependentFilesInScript(calling_script_path)

  meta_data <- list(
    filename="new_test.csv"
    ,file_hash=hash
    ,creationDate =Sys.Date()
    ,creationTime = Sys.time()
    ,data_dependencies = data_dependencies
    )


  #Find file dependencies

  #Message dependent data found

  # write metadata to disk
}

