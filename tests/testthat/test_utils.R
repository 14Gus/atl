context("Test util functions")

test_that("call if works for assigment functions",{
  test_expr <- expr(table <- getTable(mtcars))

  calls <- getCallIf(test_expr, function(x) rlang::lang_name(x) == "<-")
  expect_equal(length(calls),1)
  expect_equal(calls[[1]], rlang::lang("<-", quote(table), quote(getTable(mtcars))))
})


test_that("call if works where predicate passed with an argument",{
  test_script <- expr(table <- getTable(mtcars))

  callNameInVector <- function(call, x){
    callName <- rlang::lang_name(call)
    callName %in% x
  }

  calls <- getCallIf(test_script, callNameInVector, "getTable")

  expect_equal(calls[[1]], rlang::lang("getTable", quote(mtcars)))
})

test_that("call if works with expression arguments",{
  test_script <- expression(
    table <- getTable(mtcars)
    ,table2 <- do_nothing_fun(table)
    ,exportTable(table2, "mtcars"))

  callNameInVector <- function(call, x){
    callName <- rlang::lang_name(call)
    callName %in% x
  }

  calls <- squash(lapply(test_script, function(x) getCallIf(x, callNameInVector, "getTable")))

  expect_equal(calls[[1]], rlang::lang("getTable", quote(mtcars)))
})

test_that("call if works with nestedfunctions",{
  test_script <- expression(
    table <- do_nothing_fun(getTable(mtcars))
    ,table2 <- table
    ,exportTable(table2, "mtcars"))

  callNameInVector <- function(call, x){
    callName <- rlang::lang_name(call)
    callName %in% x
  }

  calls <- squash(lapply(test_script, function(x) getCallIf(x, callNameInVector, "getTable")))

  expect_equal(calls[[1]], rlang::lang("getTable", quote(mtcars)))
})

