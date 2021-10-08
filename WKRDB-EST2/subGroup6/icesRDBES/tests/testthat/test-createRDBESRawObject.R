test_that("createRDBESRawObject can create an empty object without errors
          or warnings",  {

  myObject <- expect_warning(createRDBESRawObject(),NA)
  myObject <-expect_error(createRDBESRawObject(),NA)
})
test_that("createRDBESRawObject can create an object from an H1 data extract
          without errors or warnings",  {

  myPath <- ".\\h1_v_1_19"

  myObject <- expect_warning(createRDBESRawObject(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(createRDBESRawObject(rdbesExtractPath = myPath),NA)
})
test_that("createRDBESRawObject can create an object from an H1 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

  myPath <- ".\\h1_v_1_19"

  myObject <- expect_warning(
    createRDBESRawObject(rdbesExtractPath = myPath,
                          castToCorrectDataTypes = FALSE),NA)
  myObject <- expect_error(
    createRDBESRawObject(rdbesExtractPath = myPath,
                          castToCorrectDataTypes = FALSE),NA)
})
test_that("createRDBESRawObject can create an object from an H5 data extract
          without errors or warnings",  {

  myPath <- ".\\h5_v_1_19"

  myObject <- expect_warning(createRDBESRawObject(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(createRDBESRawObject(rdbesExtractPath = myPath),NA)
})
test_that("createRDBESRawObject can create an object from an H5 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

    myPath <- ".\\h5_v_1_19"

    myObject <- expect_warning(
              createRDBESRawObject(rdbesExtractPath = myPath,
                                   castToCorrectDataTypes = FALSE),NA)
    myObject <- expect_error(
              createRDBESRawObject(rdbesExtractPath = myPath,
                                   castToCorrectDataTypes = FALSE),NA)
})
test_that("createRDBESRawObject can create an object from an H1 data extract by specifying file names without errors or warnings",  {

  myPath <- ".\\h1_v_1_19"
  myFileNames <- list("DE"="DE.csv","SD"="SD.csv")

  myObject <- expect_warning(createRDBESRawObject(rdbesExtractPath = myPath, listOfFileNames = myFileNames),NA)
  expect_error(createRDBESRawObject(rdbesExtractPath = myPath, listOfFileNames = myFileNames),NA)
})
test_that("createRDBESRawObject will give a warning if given a dir with no relevent files in it",  {

  myPath <- "."
  myObject <- expect_warning(createRDBESRawObject(rdbesExtractPath = myPath),"No relevent files found in given directory - an empty object will be created")
})
test_that("createRDBESRawObject creates an object with the correct data types",  {

  myPath <- ".\\h1_v_1_19"

  myRDBESRawObject <- createRDBESRawObject(rdbesExtractPath = myPath,
                         castToCorrectDataTypes = TRUE)

  myDiffs <- checkRDBESRawObjectDataTypes(myRDBESRawObject)
  numberOfDifferences <- nrow(myDiffs)
  expect_equal(numberOfDifferences,0)


})
