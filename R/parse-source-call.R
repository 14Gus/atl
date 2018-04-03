#' Gets the filepaths for any external scripts that have been sourced into a file
#'
#' @param script_path The script path of interest
getSourceFiles <- function(script_path){
  script_expression <- parse(script_path)

  source_calls <- getSourceCalls(script_expression)

  source_paths <- lapply(source_calls, parseSourceCall)

  source_path

}

#Takes an expression and gets all dependent calls from that expression
getSourceCalls <- function(expr){
  findSourceCall <- function(expr){
    getCallIf(expr, isSourceCall)
  }

  # Squash is going to turn the output into a flat vector
  source_calls <- rlang::squash(lapply(expr, findSourceCall))

  source_calls[lapply(source_calls, length)>0]

}

isSourceCall <- function(call){
  rlang::lang_name(call) == "source"
}


parseSourceCall <- function(source_call, called_env =sys.frame()) {
  source_call <- rlang::lang_standardise(source_call)

  file_arg <- source_call$file

  if(is.null(file_arg)){
    stop(paste("No file found with source call:", as.character(source_call)))
  }

  source_file_path <- as.character(eval(file_arg, called_env))

  source_file_path
}
