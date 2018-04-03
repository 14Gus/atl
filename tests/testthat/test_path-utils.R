context("path utils")

test_that("builds filename correctly",{
  hash = "1234"
  file = "test"
  extension ="csv"

  actual <- buildExportFileName(filename=file, filepath = getDefaultExportFilePath(), hash=hash,file_extension = extension)

  expected <- file.path(getwd(),"data",paste0("test","-", Sys.Date(),"-1234.csv"))
  # Need to find a better way to test this...
  expect_equal(actual, expected)

})
