context("testing utils")

test_that("Generate call works",{

  expect_equal(generateCall(getTable, "test.csv"), quote(getTable("test.csv")))
  expect_equal(generateCall(getTable, mtcars), quote(getTable(mtcars)))
  expect_equal(generateCall(getTable, file_name="test.csv"), quote(getTable(file_name="test.csv")))

})
