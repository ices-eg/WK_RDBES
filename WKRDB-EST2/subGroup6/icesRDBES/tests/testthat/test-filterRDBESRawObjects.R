test_that("filterRDBESRawObject returns the correct result",  {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")
  myFields <- c("FTarvLoc")
  myValues <- c("ZWBFO" )
  myFilteredObject <- filterRDBESRawObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the filtered object is ok (should return TRUE)
  expect_true(validateRDBESRawObject(myFilteredObject, verbose = FALSE))
  # Check the expected number of FT rows are returned
  expect_equal(nrow(myFilteredObject[["FT"]]),25)

})
