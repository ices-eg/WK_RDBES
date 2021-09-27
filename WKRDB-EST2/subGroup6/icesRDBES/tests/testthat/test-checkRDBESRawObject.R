test_that("checkDBEobject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESRawObject()

  expect_warning(checkRDBESRawObject(objectToCheck = myEmptyObject),NA)
  expect_error(checkRDBESRawObject(objectToCheck = myEmptyObject),NA)
})
test_that("checkRDBESRawObject returns T for valid empty object",  {

  myEmptyObject <- createRDBESRawObject()
  myReturn <- checkRDBESRawObject(objectToCheck = myEmptyObject)
  expect_true(myReturn)

})
test_that("checkRDBESRawObject returns T for valid object from H1 data",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_true(myReturn)

})
test_that("checkRDBESRawObject returns T for valid object from H5 data",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h5_v_1_19")
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_true(myReturn)

})
test_that("checkRDBESRawObject returns F for NA",  {

  myReturn <- checkRDBESRawObject(objectToCheck = NA)
  expect_false(myReturn)

})
test_that("checkRDBESRawObject returns F for object that is not a list",  {

  myNonList <- data.frame (tableNames  = c("DE", "SE"))
  myReturn <- checkRDBESRawObject(objectToCheck = myNonList)
  expect_false(myReturn)

})

test_that("checkRDBESRawObject returns F for object with extra name",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['XX']] <- F
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_false(myReturn)

})
test_that("checkRDBESRawObject returns F for object without all names",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']] <- NULL
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_false(myReturn)

})
test_that("checkRDBESRawObject returns F for object with a required field
          removed",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']]$DEsampScheme <- NULL
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_false(myReturn)

})
test_that("checkRDBESRawObject returns F for object with duplicate rows",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']] <- data.table::rbindlist(list(myObject[['DE']],myObject[['DE']]))
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_false(myReturn)

})
test_that("checkRDBESRawObject returns F for object with duplicate DEid values",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  myReturn <- checkRDBESRawObject(objectToCheck = myObject)
  expect_false(myReturn)

})

