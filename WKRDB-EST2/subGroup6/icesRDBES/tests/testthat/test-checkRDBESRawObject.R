test_that("Test checkDBEobject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESRawObject()

  expect_warning(checkRDBESRawObject(objectToCheck = myEmptyObject),NA)
  expect_error(checkRDBESRawObject(objectToCheck = myEmptyObject),NA)
})
test_that("Test checkRDBESRawObject does return F for NA",  {

  myReturn <- checkRDBESRawObject(objectToCheck = NA)
  expect_false(myReturn)

})

