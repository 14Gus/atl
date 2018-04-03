context("Parsing import script")

test_that("Can correctly identify import calls",{
  test_script <- expression(
    table <- read.csv("test.csv"),
    table2 <- do_nothing_func(table),
    exportTable(table2, filename = "test-mtcars", filetype = "csv")
  )

  import_calls <- getImportCalls(test_script, IMPORT_FUNS)
  expect_true(is.null(dim(import_calls)))
  expect_equal(length(import_calls), 1)
  expect_true(all_calls(import_calls))
  expect_equal(import_calls, c(rlang::lang("read.csv", "test.csv")))
})

test_that("isImportFunction predicate is working",{
  import_funs = list(
    read.csv = baseImportFun("read.csv", "file", 1)
    ,read.table = baseImportFun("read.table", "file", 1)
  )

  read.csv_call <- rlang::lang("read.csv", file="test.csv")

  expect_true(isImportFunction(read.csv_call, import_funs))
})

test_that("Gets the name of the function from the import call",{
  test_importcalls <- c(
    rlang::lang("write.csv", "test.csv"),
    rlang::lang("getTable", quote(mtcars))
  )

  import_fun_names <- getImportFunctionNames(test_importcalls)

  expect_equal(length(import_fun_names), 2)
  expect_equal(import_fun_names, c("write.csv","getTable"))
})

test_that("getDataDependencies working correclty",{
  test_importcalls <- list(
    rlang::lang("read.csv", "test.csv"),
    rlang::lang("read_csv", "test2.csv")
  )

  test_importfuns <- list(
      read.csv = baseImportFun("read.csv", "file", 1)
      ,read_csv = baseImportFun("read_csv", "file", 1)
    )

  expected <- structure(
    c("test.csv", "test2.csv"),
    .Names = c("read.csv","read_csv")
    )

  actual <- getDataDependencies(test_importfuns, test_importcalls)

  expect_equal(expected, actual)


})

