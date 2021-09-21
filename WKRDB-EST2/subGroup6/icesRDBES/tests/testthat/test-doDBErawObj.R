test_that("Test doDBErawObj does not produce errors or warnings for v1.18 input data",  {

  myRDBESextractPath <- ".\\h1_v_1_18\\"
  myDBErawPath <- myRDBESextractPath

  expect_warning(doDBErawObj(rdbesExtractPath = myRDBESextractPath, dbeRawPath = myDBErawPath ),NA)
  expect_error(doDBErawObj(rdbesExtractPath = myRDBESextractPath, dbeRawPath = myDBErawPath),NA)
})
test_that("Test doDBErawObj does not produce errors or warnings for v1.19 input data",  {

  myRDBESextractPath <- ".\\h1_v_1_19\\"
  myDBErawPath <- myRDBESextractPath

  expect_warning(doDBErawObj(rdbesExtractPath = myRDBESextractPath, dbeRawPath = myDBErawPath ),NA)
  expect_error(doDBErawObj(rdbesExtractPath = myRDBESextractPath, dbeRawPath = myDBErawPath),NA)
})
test_that("Test doDBErawObj produces error when required files not present",  {

  myRDBESextractPath <- "."
  myDBErawPath <- myRDBESextractPath


  expect_error(doDBErawObj(rdbesExtractPath = myRDBESextractPath, dbeRawPath = myDBErawPath),'DE SD SL VDmissing from the directory: .. Please check if the given directory is correct.')
})
