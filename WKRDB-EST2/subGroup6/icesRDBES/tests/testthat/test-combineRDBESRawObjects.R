test_that("combineRDBESRawObjects returns error for NA",  {

    expect_error(combineRDBESRawObjects(rdbesRawObject1=NA,
                                        rdbesRawObject2=NA),"At least one of the rdbesRawObjects is not valid - mergeRDBESRawObjects will not proceed")
})
test_that("combineRDBESRawObjects returns invalid rdbesRawObject when supplied with the duplicate rdbesRawObjects",  {

  myPath <- ".\\h1_v_1_19"
  myObject1 <- createRDBESRawObject(rdbesExtractPath = myPath)
  myObject2 <- createRDBESRawObject(rdbesExtractPath = myPath)

  # Check these are valid objects before we try and combine them
  expect_true(validateRDBESRawObject(myObject1))
  expect_true(validateRDBESRawObject(myObject2))

  myCombinedObject <- combineRDBESRawObjects(rdbesRawObject1=myObject1,
                                             rdbesRawObject2=myObject2)

  expect_false(validateRDBESRawObject(myCombinedObject))
})
test_that("combineRDBESRawObjects returns valid rdbesRawObject when supplied with valid, different rdbesRawObjects",  {

  myObject1 <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myObject2 <- createRDBESRawObject(rdbesExtractPath = ".\\h5_v_1_19")

  # Check these are valid objects before we try and combine them
  expect_true(validateRDBESRawObject(myObject1))
  expect_true(validateRDBESRawObject(myObject2))

  myCombinedObject <- combineRDBESRawObjects(rdbesRawObject1=myObject1,
                                             rdbesRawObject2=myObject2)

  expect_true(validateRDBESRawObject(myCombinedObject))
})



