# List of patterns to find import functions


#' S3 class representation of an import function. Will suffice for the majority of import functions that read from a file path such as read.csv, read.table, fread etc.

baseImportFun <- function(name, con_arg_name, arg_pos){
  name  <- name
  con_arg_name <- con_arg_name
  arg_pos <- arg_pos

  structure(class="baseImportFun", environment())
}

#' S3 class representation of the default ATL import function for calling data from databases.

getTableImportFun <- function(name, table=NULL, dataset=NULL, source=NULL){
  name <- name
  table <- table
  dataset <- dataset
  source <- source

  structure(class="getTableImportFun", environment())
}

#' Parse a function "call" where the function is an import function that gets data such as "read.csv", or "getTable" and return an identifier to the connection or datasource.
#' For instance say the call is "read.csv("path/to/file/test.csv"), parseImportCall will return "test.csv".
#' Where the connection argument for the import function is called indirectly by referencing another name or function, such as in "read.csv(filepath), parseImportCall will try to retieve the underlying value by evaluating the connection argument in the global environment.
#'
#' @param import_fun An import function of class "importFun".
#' @param import_call An r language of type "call" where the function in the call corresponds to the function name of the argument "import_fun".
#' @return A string representing a unique identifier for the location of the table or data that was called by the import function. For most import functions this will be the file path of the file.
#' @export

parseImportCall <- function(import_fun, import_call){

  UseMethod("parseImportCall")

}

parseImportCall.default <- function(import_fun, import_call){

  stopifnot(rlang::lang_name(import_call) == import_fun$name)

  # This is going to add the argument names to any arguments called by position and rearrange them in their position as in the function definition
  import_call <- rlang::lang_standardise(import_call)

  con_arg_name <- import_fun$con_arg_name

  stopifnot(!is.null(con_arg_name))

  conn <- lang_args(import_call)[[con_arg_name]]

  # Conn at this point may be a string (hopefully a filepath), or may be something more tricky like a symbol or a function. To get the string that was passed to our input function, we are going to evaluate "conn" in the global environment. This isn't going to be accurate 100% of the time. Since in the script itself, conn may not have been evaluated in the global environment. Therefore, conn may not bind to any variables in the scope of the global environment (returning an "object not found" error) or alternatively may reference a different variable in the environment the import_fun was evaluated in. For instance if the import_fun was called in a function.

  stopifnot(!is.null(conn))

  conn_arg_string <- as.character(eval(conn, envir = sys.frame()))

  if(!is.character(conn_arg_string)){
    stop(glue::glue("Unable to identify the file path or other data connection used in the statement {as.character(import_call)}."))
  }

  return(conn_arg_string)
}

parseImportCall.getTableImportFun <- function(import_fun, import_call){

  import_call <- rlang::lang_standardise(import_call)


  table <- as.character(import_call$table)

  # If the call has no table argument get default value from the import function setup
  if(length(table) == 0){
    table <- as.character(import_fun$table)
  }

  dataset <- as.character(import_call$dataset)

  # If the call has no dataset argument get default value from the import function setup
  if(length(dataset) == 0){
    dataset <- as.character(import_fun$dataset)
  }

  source <- as.character(import_call$source)

  # If the call has no source argument get default value from the import function setup
  if(length(source) == 0){
    source <- as.character(import_fun$source)
  }

  paste(table, dataset, source, sep=".")

}
