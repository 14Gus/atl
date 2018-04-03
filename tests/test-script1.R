# Test script 1

table <- read.csv("tests/data/mtcars.csv")

table2 <- do_nothing_fun(table)

exportTable(table2, filename = "test-mtcars", filetype = "csv")
