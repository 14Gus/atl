# Test script 3. Import function in a wrapper

wrapper_fun <- function(){
  getTable(mtcars)
}

table <- wrapper_fun()

table2 <- do_nothing_fun(table)

exportTable(table2,filename = "test-mtcars",filetype = "csv")
