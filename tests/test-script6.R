#Test 6. compound creation

FILE_PATH = "data"

file_name = "mtcars.csv"


table <- read.csv(file.path(FILE_PATH, file_name))

table2 <- do_nothing_fun(table)

exportTable(table2, filename = "test-mtcars", filetype = "csv")
