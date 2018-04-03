context("Test finds file dependencies from static scripts")

test_that("Finds dependencies in simple import, apply function, export script",{

  script <- "tests/test-script1.R"

  dependent_files <- findDependentFilesInScript(script)

  expect_equal(dependent_files$data_dependencies, "tests/data/mtcars.csv")
})

test_that("Finds dependencies sourced in external script",{
  script <- "tests/test-script2.R"

  dependent_files <- findDependentFilesInScript(script)

  expect_equal(dependent_files$data_dependencies, "tests/data/mtcars.csv")

})

test_that("Finds dependencies sourced inside a function",{
  script <- "tests/test-script3.R"

  dependent_files <- findDependentFilesInScript(script)

  expect_equal(dependent_files$data_dependencies, "tests/data/mtcars.csv")

})

test_that("Finds dependencies in multiple sourced tables",{

  script <- "tests/test-script4.R"

  dependent_files <- findDependentFilesInScript(script)

  expect_equal(dependent_files$data_dependencies, c("tests/data/mtcars.csv","tests/data/mtcars2.csv"))
})

