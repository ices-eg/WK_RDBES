test_that("createRDBESRawObject can create an empty object without errors
          or warnings",  {

  expect_warning(createRDBESRawObject(),NA)
  expect_error(createRDBESRawObject(),NA)
})
test_that("createRDBESRawObject can create an object from an H1 data extract
          without errors or warnings",  {

  myPath <- ".\\h1_v_1_19"

  myObject <- expect_warning(createRDBESRawObject(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(createRDBESRawObject(rdbesExtractPath = myPath),NA)
})
test_that("createRDBESRawObject can create an object from an H5 data extract
          without errors or warnings",  {

  myPath <- ".\\h5_v_1_19"

  myObject <- expect_warning(createRDBESRawObject(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(createRDBESRawObject(rdbesExtractPath = myPath),NA)
})
test_that("createRDBESRawObject will give a warning if given a dir with no relevent files in it",  {

  myPath <- "."
  myObject <- expect_warning(createRDBESRawObject(rdbesExtractPath = myPath),"No relevent files found in given directory - an empty object will be created")
})
