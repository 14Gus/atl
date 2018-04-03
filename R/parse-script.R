#' To parse the script we are going to need to walk the "abstract syntax tree".
#' See guide on walking the syntax tree http://adv-r.had.co.nz/Expressions.html#ast-funs for more
#' information on how this all works.

#' Iterate through a script and find all file paths or other dependent files that were imported using the list of functions supplied in file_import_functions
#'
#' @param script_path Path to the script of the file
#' @param file_import_functions List of functions that could be used to import files such as "read.csv" or "read.table" structured as class importFun. Default list provided in IMPORT_FUNS
#' @export

findDependentFilesInScript <-  function(script_path, file_import_functions=IMPORT_FUNS){
    script_expressions <- parse(script_path)

    import_calls <- getImportCalls(script_expressions, file_import_functions)

    import_fun_names <- getImportFunctionNames(import_calls)

    import_funs <- file_import_functions[import_fun_names]

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
    getCallIf(expr, isImportFunction, import_funs = file_import_functions)
  }

  # Squash is going to turn the output into a flat vector
  import_calls <- rlang::squash(lapply(expr, findImportCall))

  import_calls[lapply(import_calls, length)>0]
}

getImportFunctionNames <- function(import_calls){
  fun_names <- as.character(lapply(import_calls, rlang::lang_name))

}

getDataDependencies <- function(import_calls, import_funs){
  Map(parseImportCall, import_calls, import_funs)
}


#' Parse a call and return TRUE if the function name in the call is in the vector of functions provided
#' in the parameter import_fun_name
isImportFunction <- function(call, import_funs){
  import_fun_names <- names(import_funs)
  function_name <- rlang::lang_name(call)
  function_name %in% import_fun_names

}

