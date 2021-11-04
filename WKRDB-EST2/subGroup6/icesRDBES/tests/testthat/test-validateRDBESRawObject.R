test_that("validateDBEobject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESRawObject()

  expect_warning(validateRDBESRawObject(objectToCheck = myEmptyObject,
                                        verbose = FALSE),NA)
  expect_error(validateRDBESRawObject(objectToCheck = myEmptyObject,
                                      verbose = FALSE),NA)
})
test_that("validateRDBESRawObject returns T for valid empty object",  {

  myEmptyObject <- createRDBESRawObject()
  myReturn <- validateRDBESRawObject(objectToCheck = myEmptyObject,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESRawObject returns T for valid object from H1 data",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESRawObject returns T for valid object from H1 data (also checking data types)",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESRawObject returns T for valid object from H5 data",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h5_v_1_19")
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESRawObject returns T for valid object from H5 data (also checking data types)",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h5_v_1_19")
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESRawObject returns F for NA",  {

  myReturn <- validateRDBESRawObject(objectToCheck = NA,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESRawObject returns F for object that is not a list",  {

  myNonList <- data.frame (tableNames  = c("DE", "SE"))
  myReturn <- validateRDBESRawObject(objectToCheck = myNonList,
                                     verbose = FALSE)
  expect_false(myReturn)

})

test_that("validateRDBESRawObject returns F for object with extra name",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['XX']] <- F
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESRawObject returns F for object without all names",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']] <- NULL
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESRawObject returns F for object with a required field
          removed",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']]$DEsampScheme <- NULL
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESRawObject returns F for object with duplicate rows",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']] <- data.table::rbindlist(list(myObject[['DE']],myObject[['DE']]))
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESRawObject returns F for object with duplicate DEid values",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESRawObject returns F for object with invalid data types (when checking data types)",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[["DE"]]$DEid <- as.character(myObject[["DE"]]$DEid)
  myObject[["SD"]]$SDid <- as.character(myObject[["SD"]]$SDid)
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESRawObject returns F for object with with duplicate DEid values and invalid data types (when checking data types)",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  myObject[["DE"]]$DEid <- as.character(myObject[["DE"]]$DEid)
  myObject[["SD"]]$SDid <- as.character(myObject[["SD"]]$SDid)
  myReturn <- validateRDBESRawObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_false(myReturn)

})
