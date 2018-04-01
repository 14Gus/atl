# Test 7. Complex scope.

file_path <- "tests/data/mtcars.csv"

wrapper_fun <- function(){
  file_path <- "tests/data/iris.csv"

  read.csv(file_path)
}

table <- wrapper_fun()

table2 <- do_nothing_fun(table)

exportTable(table2, filename = "test-mtcars", filetype = "csv")
