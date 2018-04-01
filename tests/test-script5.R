#Test 5. Indirect file path

FILE_PATH <- "tests/data/mtcars.csv"

table <- read.csv(FILE_PATH)

table2 <- do_nothing_fun(table)

exportTable(table2, filename = "test-mtcars", filetype = "csv")

