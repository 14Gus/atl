# Test script 1

table <- getTable(mtcars)

table2 <- do_nothing_fun(table)

exportTable(table2,filename = "test-mtcars",filetype = "csv")
