test_that("Test checkDBEobject does not produce errors or warnings",  {

  myRDBESextractFile <- ".\\h1_v_1_18\\DBErawObj_2015_H1.rds"

  myObject <- readRDS(myRDBESextractFile)
  #myObject <- NA

  expect_warning(checkDBEobject(objectToCheck = myObject),NA)
  expect_error(checkDBEobject(objectToCheck = myObject),NA)
})
test_that("Test checkDBEobject does produce errors for NA",  {

  #myRDBESextractPath <- ".\\h1_v_1_18\\"

  #myObject <- load(path=myRDBESextractPath)
  myObject <- NA

  expect_error(checkDBEobject(objectToCheck = myObject)
               ,"objectToCheck was NA - this is not valid")

})
test_that("Test checkDBEobject does state input is wrong format",  {

  myRDBESextractFile <- ".\\h1_v_1_18\\BV.csv"

  myObject <- read.csv(myRDBESextractFile)
  #myObject <- NA

  expect_error(checkDBEobject(objectToCheck = myObject)
               ,"objectToCheck is not valid")
})
