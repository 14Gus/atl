context("baseImportFun class constructor")

test_that("constructing class correctly",{

  read.csv_fun <- baseImportFun("read.csv", "file", 1)

  expect_true(is.environment(read.csv_fun))
  expect_equal(read.csv_fun$name, "read.csv")
  expect_equal(read.csv_fun$con_arg_name, "file")
  expect_equal(read.csv_fun$arg_pos, 1)
  expect_equal(class(read.csv_fun), "baseImportFun")

})

context("baseImportFun methods")

test_that("parseImportCall works when conn passed by position",{

  file_name <- "test.csv"

  import_fun <- baseImportFun("read.csv", "file", 1)
  import_call <- rlang::lang("read.csv","test.csv")

  expect_equal(parseImportCall(import_fun, import_call), "test.csv")


})

test_that("parseImportCall works when conn passed by name",{

  import_fun <- baseImportFun("read.csv", "file", 1)
  import_call <- rlang::lang("read.csv", file="test.csv")

  expect_equal(parseImportCall(import_fun, import_call), "test.csv")

})

test_that("parseImportCall works when conn passed with other named argument",{

  import_fun <- baseImportFun("read.csv", "file", 1)
  import_call <- rlang::lang("read.csv", delim=",", "test.csv")

  expect_equal(parseImportCall(import_fun, import_call), "test.csv")

})

test_that("parseImportCall works when conn passed indirectly",{

  file_name <- "test.csv"
  import_fun <- baseImportFun("read.csv", "file", 1)
  import_call <- rlang::lang("read.csv", file=quote(file_name))

  expect_equal(parseImportCall(import_fun, import_call), "test.csv")

})



test_that("method parseImportCall works for functions of class baseImportFun in IMPORT_FUNS", {
  library(readr)
  library(data.table)
  library(readxl)

  read.csv_call <- rlang::lang(read.csv, "test.csv")
  read.csv2_call <- rlang::lang(read.csv2, "test.csv")
  read.delim_call = rlang::lang(read.delim, "test.csv")
  read.delim2_call = rlang::lang(read.delim2, "test.csv")
  read.table_call = rlang::lang(read.table, "test.csv")
  read.fwf_call = rlang::lang(read.fwf, "test.csv")
  read_csv_call = rlang::lang(read_csv, "test.csv")
  read_csv2_call = rlang::lang(read_csv2, "test.csv")
  read_tsv_call = rlang::lang(read_tsv, "test.csv")
  read_table_call = rlang::lang(read_table, "test.csv")
  read_file_call = rlang::lang(read_file, "test.csv")
  read_fwf_call = rlang::lang(read_fwf, "test.csv")
  read_xls_call = rlang::lang(read_xls, "test.csv")
  read_excel_call = rlang::lang(read_excel, "test.csv")
  read_xlsx_call = rlang::lang(read_xlsx, "test.csv")
  fread_call = rlang::lang(fread, "test.csv")

  read.csv_fun <- IMPORT_FUNS[["read.csv"]]
  read.csv2_fun <- IMPORT_FUNS[["read.csv2"]]
  read.delim_fun = IMPORT_FUNS[["read.delim"]]
  read.delim2_fun = IMPORT_FUNS[["read.delim2"]]
  read.table_fun = IMPORT_FUNS[["read.table"]]
  read.fwf_fun = IMPORT_FUNS[["read.fwf"]]
  read_csv_fun = IMPORT_FUNS[["read_csv"]]
  read_csv2_fun = IMPORT_FUNS[["read_csv2"]]
  read_tsv_fun = IMPORT_FUNS[["read_tsv"]]
  read_table_fun = IMPORT_FUNS[["read_table"]]
  read_file_fun = IMPORT_FUNS[["read_file"]]
  read_fwf_fun = IMPORT_FUNS[["read_fwf"]]
  read_xls_fun = IMPORT_FUNS[["read_xls"]]
  read_excel_fun = IMPORT_FUNS[["read_excel"]]
  read_xlsx_fun = IMPORT_FUNS[["read_xlsx"]]
  fread_fun = IMPORT_FUNS[["fread"]]


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

context("Testing getTableImportFUn constructor and methods")

test_that("getTableImportFun class constuctor",{

  getTable_fun <- getTableImportFun("getTable", "test_table", "test_dataset", "test_source")

  expect_equal(getTable_fun$table, "test_table")
  expect_equal(getTable_fun$dataset, "test_dataset")
  expect_equal(getTable_fun$source, "test_source")
})

test_that("getTableFUn parseImportCall default method works correctly",{
  getTable_fun <- getTableImportFun("getTable",dataset = "local_env", source="local_env")

  getTable_call <- rlang::lang("getTable", quote(mtcars))

  expect_equal(parseImportCall(getTable_fun, getTable_call), "mtcars.local_env.local_env")

})

test_that("getTableFun parseImportCall works for functions of class baseImportFun in IMPORT_FUNS",{
  getTable_fun <- IMPORT_FUNS$getTable

  getTable_call <- rlang::lang("getTable", quote(mtcars))

  expect_equal(parseImportCall(getTable_fun, getTable_call), "mtcars.local.local_env")

})
