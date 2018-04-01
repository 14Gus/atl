do_nothing_fun <- function(x){
  x
}

# Test if all the objects in a vector or list are calls
all_calls <- function(x){
  all(unlist(lapply(x, class))=="call")
}

# Test if all the objects in a vector or list are names
all_names <- function(x){
  all(unlist(lapply(x, class))=="name")
}

generateCall <- function(fun_name, ...){
  fun_name <- substitute(fun_name)
  browser()
  arg_list <- substitute(...)
  as.call(c(fun_name, arg_list))

}
