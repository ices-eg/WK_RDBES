test_that("checkDBEobject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESRawObject()

  expect_warning(checkRDBESRawObject(objectToCheck = myEmptyObject),NA)
  expect_error(checkRDBESRawObject(objectToCheck = myEmptyObject),NA)
})
test_that("checkRDBESRawObject returns F for NA",  {

  myReturn <- checkRDBESRawObject(objectToCheck = NA)
  expect_false(myReturn)

})
test_that("checkRDBESRawObject returns T for valid object",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_true(myReturn)

})
test_that("checkRDBESRawObject returns F for object with extra name",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['XX']] <- F
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_false(myReturn)

})
test_that("checkRDBESRawObject returns F for object with name removed",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']] <- NULL
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_false(myReturn)

})
test_that("checkRDBESRawObject returns F for object with required field
          removed",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']]$DEsampScheme <- NULL
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_false(myReturn)

})

