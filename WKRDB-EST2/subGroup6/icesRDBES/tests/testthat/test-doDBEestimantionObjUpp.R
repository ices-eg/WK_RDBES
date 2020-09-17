test_that("Test doDBEestimantionObjUpp works for H1", {

  #First lets set up some variables we'll use in the tests

  expectedLengthOfOutputList <- 6
  expectedNames <- c("expectedTables", "de", "sd", "su1", "su2", "su3")
  suNames <- c("id", "recType", "unitName", "stratification", "stratumName"
               , "clustering", "clusterName", "numTotal", "numSamp", "selProb"
               , "incProb", "selectMeth", "selectMethCluster"
               , "numTotalClusters", "numSampClusters", "selProbCluster"
               , "incProbCluster")

  # Now define our function input and output
  functionInput <- h1DBErawObj
  functionOutput <- doDBEestimantionObjUpp(functionInput)

  # Now lets start testing!

  # List should be the correct length with the correct named entries
  expect_equal(length(functionOutput), expectedLengthOfOutputList)
  expect_equal(names(functionOutput), expectedNames)

  # Now lets test the content of the list

  # If de is in the output it should equal DE in the input
  if (!is.null(functionOutput$de)) {
    expect_equivalent(h1DBErawObj$DE, functionOutput$de)
  } else {
    fail(message = "'de' not present in the output")
  }

  # If sd is in the output it should equal sd in the input
  if (!is.null(functionOutput$sd)) {
    expect_equivalent(h1DBErawObj$SD, functionOutput$sd)
  } else {
    fail(message = "'sd' not present in the output")
  }

  # Now test whether the values of su1 etc have been correctly copied
  # from the input
  expectedTables <- functionOutput$expectedTables

  for (suToCheck in c("su1", "su2", "su3")) {
    # Check that selected values in suX are the same as in the input data
    if (!is.null(functionOutput[[suToCheck]])) {

      # Get the correct input data
      suTableName <- expectedTables[
        expectedTables$su_level == suToCheck, "table_names"]
      suInputValues <- functionInput[[suTableName]]
      suInputNames <- paste(suTableName, suNames, sep = "")

      expect_equivalent(
        suInputValues[, suInputNames], functionOutput[[suToCheck]][, suNames])
    } else {
      fail(message = paste(suToCheck, "not present in the output"))
    }
  }
})
test_that("Test doDBEestimantionObjUpp fails for incorrect input", {

  # We might want to be more specific about the error messages we test for here
  expect_error(doDBEestimantionObjUpp("banana"))
  expect_error(doDBEestimantionObjUpp(list("a" = "silly")))
  expect_error(doDBEestimantionObjUpp(NA))
  expect_error(doDBEestimantionObjUpp(NULL))

})
