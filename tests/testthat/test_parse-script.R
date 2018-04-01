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

test_that("parsing import function works with indirect calls",{

  file_name <- "test.csv"
  import_fun <- baseImportFun("read.csv", "file", 1)
  import_call_simple <- quote(read.csv("test.csv"))
  import_call_name <- quote(read.csv(file_name))

  expect_equal(parseImportCall(import_fun, import_call_simple), "test.csv")
  expect_equal(parseImportCall(import_fun, import_call_name), "test.csv")


})




test_that("correctly parsing each function in IMPORT_FUNS", {
  library(readr)
  library(data.table)
  library(readxl)

  read.csv_call <- generateCall(read.csv, "test.csv")
  read.csv2_call <- generateCall(read.csv2, "test.csv")
  read.delim_call = generateCall(read.delim, "test.csv")
  read.delim2_call = generateCall(read.delim2, "test.csv")
  read.table_call = generateCall(read.table, "test.csv")
  read.fwf_call = generateCall(read.fwf, "test.csv")
  read_csv_call = generateCall(read_csv, "test.csv")
  read_csv2_call = generateCall(read_csv2, "test.csv")
  read_tsv_call = generateCall(read_tsv, "test.csv")
  read_table_call = generateCall(read_table, "test.csv")
  read_file_call = generateCall(read_file, "test.csv")
  read_fwf_call = generateCall(read_fwf, "test.csv")
  read_xls_call = generateCall(read_xls, "test.csv")
  read_excel_call = generateCall(read_excel, "test.csv")
  read_xlsx_call = generateCall(read_xlsx, "test.csv")
  fread_call = generateCall(fread, "test.csv")

  read.csv_fun <- IMPORT_FUNS["read.csv"]
  read.csv2_fun <- IMPORT_FUNS["read.csv2"]
  read.delim_fun = IMPORT_FUNS["read.delim"]
  read.delim2_fun = IMPORT_FUNS["read.delim2"]
  read.table_fun = IMPORT_FUNS["read.table"]
  read.fwf_fun = IMPORT_FUNS["read.fwf"]
  read_csv_fun = IMPORT_FUNS["read_csv"]
  read_csv2_fun = IMPORT_FUNS["read_csv2"]
  read_tsv_fun = IMPORT_FUNS["read_tsv"]
  read_table_fun = IMPORT_FUNS["read_table"]
  read_file_fun = IMPORT_FUNS["read_file"]
  read_fwf_fun = IMPORT_FUNS["read_fwf"]
  read_xls_fun = IMPORT_FUNS["read_xls"]
  read_excel_fun = IMPORT_FUNS["read_excel"]
  read_xlsx_fun = IMPORT_FUNS["read_xlsx"]
  fread_fun = IMPORT_FUNS["fread"]


  expect_equal(parseImportCall(read.csv_fun, read.csv_call), "test.csv")
  expect_equal(parseImportCall(read.csv2_fun, read.csv2_call), "test.csv")
  expect_equal(parseImportCall(read.delim_fun, read.delim_call), "test.csv")
  expect_equal(parseImportCall(read.delim2_fun, read.delim2_call), "test.csv")
  expect_equal(parseImportCall(read.table_fun, read.table_call), "test.csv")
  expect_equal(parseImportCall(read.fwf_fun, read.fwf_call), "test.csv")
  expect_equal(parseImportCall(read_csv_fun, read_csv_call), "test.csv")
  expect_equal(parseImportCall(read_csv2_fun, read_csv2_call), "test.csv")
  expect_equal(parseImportCall(read_table_fun, read_table_call), "test.csv")
  expect_equal(parseImportCall(read_file_fun, read_file_call), "test.csv")
  expect_equal(parseImportCall(read_fwf_fun, read_fwf_call), "test.csv")
  expect_equal(parseImportCall(read_xls_fun, read_xls_call), "test.csv")
  expect_equal(parseImportCall(read_excel_fun, read_excel_call), "test.csv")
  expect_equal(parseImportCall(read_xlsx_fun, read_xlsx_call), "test.csv")
  expect_equal(parseImportCall(fread_fun, fread_call), "test.csv")


})
