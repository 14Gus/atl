#' To parse the script we are going to need to walk the "abstract syntax tree".
#' See guide on walking the syntax tree http://adv-r.had.co.nz/Expressions.html#ast-funs for more
#' information on how this all works.

#' Iterate through a script and find all file paths or other dependent files that were imported using the list of functions supplied in file_import_functions
#'
#' @param script_path Path to the script of the file
#' @param file_import_functions Name of functions as string that could be used to import files such as "read.csv" or "read.table"
#' @export

findDependentFilesInScript <-  function(script_path, file_import_functions=IMPORT_FUNS){
    script_expressions <- parse(script_path)

    import_calls <- getImportCalls(script_expressions, file_import_functions)

    import_fun_names <- getImportFunctionNames(import_calls)

    import_funs <- file_import_functions[names(file_import_functions) %in% import_fun_names]

    data_dependencies <- getDataDependencies(import_calls, import_funs)

    list(
      import_calls=import_calls
      ,import_fun_names=import_fun_names
      ,data_dependencies=data_dependencies
      )

}



#' Finds any import functions
#' @return A vector of calls
getImportCalls <- function(expr, file_import_functions){

  findImportCall <- function(expr){
    import_fun_names <- names(file_import_functions)
    getCallIf(expr, isImportFunction, import_fun_name=import_fun_names)
  }

  c(unlist(lapply(expr, findImportCall)))

}

getImportFunctionNames <- function(import_calls){
  fun_names <- lapply(import_calls, function(x) x[[1]])

}

getDataDependencies <- function(import_calls, import_funs){
  Map(parseImportCall, import_calls, import_funs)
}


#' Parse a call and return TRUE if the function name in the call is in the vector of functions provided
#' in the parameter import_fun_name
isImportFunction <- function(call, import_fun_names){

  function_string <- deparse(call[1])
  function_string %in% import_fun_names

}


getImportFunctionConnectionArg <- function(import_table_call){

  conn <- parseImportCall(import_table_call)

  fun_name <- import_table_call[[1]]

  # If the connection argument for the import function uses non-standard evaluation evaluating it in the global environment is going to return a dataframe or more likely an "object not found" error. So we are going to guess that the function call was made in the global environment and convert the name to a string.
  if(connArgUsesNSE(fun_name)){
    return(as.character(conn_arg))
  }

  # We are going to evaluate our connection argument, conn_arg, in the global environment. If it's a string, it will return a string. If it's a name, symbol or function that exists in the global environment or on the global environment search path and returns a string this should also work. If the connection argument is created outside the lexical scope of the global environment, then either the code will fail or the wrong connection argument will be returned.

  return(conn_arg_string)
}

connArgUsesNSE <- function(fun_name, use_nse_funs=USE_NSE_FUNS){
  fun_name %in% use_nse_funs
}

