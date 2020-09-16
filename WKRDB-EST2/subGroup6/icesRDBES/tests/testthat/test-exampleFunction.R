test_that("concatenating strings works", {
  expectedResult <- "A B"
  stringX <- "A"
  stringY <- "B"
  expect_equal(exampleFunction(stringX, stringY), expectedResult)
})
