
generateHashFromScript <- function(script_path){
  digest::digest(readLines(script_path), algo="crc32")
}

is_assignment <- function(expr){
  class(expr[[1]] == "<-")
}

is_project <- function(path="."){

  length(dir(path = path, pattern = ".Rproj$")) == 1

}

#' Apply grepl to a vector of patterns rather than a single pattern
#' @inheritParams grepl
#' @param reduce Return the ouput to a single dimensional vector rather than a list

greplVectorised <- function(pattern, x, reduce=FALSE, ...){

  out <- lapply(pattern, function(y) unlist(grepl(y, x, ...)))
  if(reduce) {
    return(Reduce(`|`, out))
  }
  out
}


#' Walk the "abstract syntax tree". Return any call that meets the predicate condition defined by the predicate argument
#' @param x An expression
#' @param predicate_fun A predicate function that takes a call as its first argument and returns TRUE or FALSE
#' @return A vector of calls where the predicate function was met.
#' then extract an output by applying "output_fun" to the call.
getCallIf <- function(x, predicate_fun, ...){

  # Test if expression is atomic or a name
  if (is.atomic(x) || is.name(x)) {

    #Do nothing
    character()
    # Test if expression is a call
  } else if (is.call(x)) {

    # if it's a call does it meet the condition defined by our predicate function?
    if (predicate_fun(x, ...)) {
      # if so, return something as defined by our output function
      out <- x
    } else {
      out <- character()
    }

    # Let's walk the next branch of the tree
    unique(c(out, unlist(lapply(x, function(x) getCallIf(x, predicate_fun, ...)))))

    # Pairlists are argument key value pairs by the way
  } else if (is.pairlist(x)) {

    unique(unlist(lapply(x, function(x) walkAst(x, predicate_fun))))

  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

