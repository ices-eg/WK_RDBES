test_that("createRDBESRawObject can create an empty object without errors
          or warnings",  {

  expect_warning(createRDBESRawObject(),NA)
  expect_error(createRDBESRawObject(),NA)
})
test_that("createRDBESRawObject can create an object from a data extract
          without errors or warnings",  {

  myPath <- ".\\h1_v_1_19"

  myObject <- expect_warning(createRDBESRawObject(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(createRDBESRawObject(rdbesExtractPath = myPath),NA)
})
