# Test script 4. Import multiple tables

table <- read.csv("tests/data/mtcars.csv")

table2 <- do_nothing_fun(table)

table3 <- read.csv("tests/data/mtcars2.csv")

table4 <- left_join(table2, table3)

exportTable(table4, filename = "test-mtcars",filetype = "csv")

