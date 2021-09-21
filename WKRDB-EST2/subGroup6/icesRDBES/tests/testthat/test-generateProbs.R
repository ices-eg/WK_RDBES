test_that("Test generateProbs fails for incorrect input", {

  # We might want to be more specific about the error messages we test for here
  expect_error(generateProbs(x = "banana", probType = "selection"))
  expect_error(generateProbs(x = list("a" = "silly"), probType = "selection"))
  expect_error(generateProbs(x = NA, probType = "selection"))
  expect_error(generateProbs(x = NULL, probType = "selection"))
  expect_error(generateProbs(x = "banana", probType = "anotherMethod"))
})
