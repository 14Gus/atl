# Test 8. Single pipe

library(dplyr)

getTable(mtcars) %>%

  do_nothing_fun %>%

  exportTable(filename = "test-mtcars", filetype = "csv")
