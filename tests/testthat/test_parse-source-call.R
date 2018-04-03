context("Parsing source calls")



test_that("isSourceCall identifies calls",{

  expect_true(isSourceCall(rlang::lang("source","test-script.R")))
  expect_true(isSourceCall(rlang::lang("source",file="test-script.R")))
  expect_false(isSourceCall(rlang::lang("read.csv","test-script.R")))
  expect_error(isSourceCall(list(a=1,b=2)))

})

test_that("parseSourceCall parses source calls correctly",{

  source_call <- rlang::lang("source","test-script.R")


  expect_equal(parseSourceCall(source_call), "test-script.R")
})

test_that("parseSourceCall parses source calls correctly with indirect calls",{

  file_path <- "test-script.R"
  source_call <- rlang::lang("source",file_path)


  expect_equal(parseSourceCall(source_call), "test-script.R")

})

test_that("correctly finds source calls in script",{
  test_expr <- expression(
    source("test/import_script.R")
    ,table2 <- do_nothing_fun(table)
  )

  expect_equal(getSourceCalls(test_expr)[[1]], rlang::lang("source","test/import_script.R"))

})

test_that("getSourceFiles works",{
  file_path <- "tests/test-script2.R"

  expect_equal(getSourceFiles(file_path), "test/import_script.R")
})
