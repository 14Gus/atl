context("Parsing import script")

test_that("Can correctly identify import calls",{
  test_script <- expression(
    table <- getTable(mtcars),
    table2 <- do_nothing_func(table),
    exportTable(table2, filename = "test-mtcars", filetype = "csv")
  )

  import_calls <- getImportCalls(test_script, IMPORT_FUNS)
  expect_true(is.null(dim(import_calls)))
  expect_true(length(import_calls) == 1)
  expect_true(all_calls(import_calls))
  expect_true(import_calls == c(quote(getTable(mtcars))))
})

test_that("Gets the name of the function from the import call",{
  test_importcalls <- c(
    generateCall(write.csv, "test.csv"),
    as.call(quote(getTable(mtcars)))
  )

  import_fun_names <- getImportFunctionNames(test_importcalls)

  expect_true(length(import_fun_names) == 2)
  expect_true(all_names(import_fun_names))
  expect_true(as.character(import_fun_names) == c("write.csv","getTable"))
})

