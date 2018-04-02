# List of patterns to find import functions


#' S3 class representation of an import function. Will suffice for the majority of import functions that read from a file path such as read.csv, read.table, fread etc.

baseImportFun <- function(name, con_arg_name, arg_pos){
  name  <- name
  con_arg_name <- con_arg_name
  arg_pos <- arg_pos

  structure(class="baseImportFun", environment())
}

getTableImportFun <- function(name){
  name <- name

  structure(class="getTableImportFun", environment())
}

parseImportCall <- function(import_fun, import_call){

  UseMethod("parseImportCall")

}

parseImportCall.default <- function(import_fun, import_call){

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
  # get table

  # get database

  # get source

  # concatenate source, table, database

}
