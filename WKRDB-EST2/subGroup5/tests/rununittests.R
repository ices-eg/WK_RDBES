library(testthat)
for (f in list.files("funs", pattern="*.R")){
  source(file.path("funs", f))  
}
#' adapted from Davids script
#' Hack for now.
#' Will assume this is provided in data.
fixInclusionProbabilities <- function(BVtable){
  uniqueSelMethod <- unique(BVtable$BVselectMeth)
  if(length(uniqueSelMethod)>1) { 
    stop ("Cannot proceed: two different selection methods")
  }
  #print(a)
  if( uniqueSelMethod %in% c("SRSWOR") )
  {
    if(sum(is.na(BVtable$BVnumTotal))>0) stop ("Cannot proceed: NAs in total")
    if(sum(is.na(BVtable$BVnumSamp))>0) stop ("Cannot proceed: NAs in sampled")
    
    if (nrow(BVtable[BVtable$BVnumTotal <0,]))
    {
      stop ("Cannot proceed: Zeros in BVnumTotal")
    }
    BVtable$BVinclProp <-  BVtable$BVnumSamp/BVtable$BVnumTotal
  }
  else{
    stop("SelectionMehtod", uniqueSelMethod, "not supported")
  }
  return(BVtable)
}


exampleData <- readRDS("inputs/modified_FMBV_raw_list2.rds")

#extract FM and BV for one sample
FMsa <- exampleData$FM[exampleData$FM$SAid==exampleData$FM$SAid[1],]
BVsa <- exampleData$BV[exampleData$BV$FMid %in% FMsa$FMid,]

BVsa <- fixInclusionProbabilities(BVsa)

#
# Checks folder ./tests/unittests for tests to run
# test files must be named test-<something>
# test does not otherwise have to bother with importing anything
# 
# This test setup is desinged some that functions and tests can be copied to apprioriate place in a packages that uses testthat
#

testthat::test_dir("tests/unittests/")