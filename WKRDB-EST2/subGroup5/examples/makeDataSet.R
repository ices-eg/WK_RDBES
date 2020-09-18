#
# Generate example results for subg 7, using preliminary data structure for output
#

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

SAids <- unique(exampleData$FM$SAid)

results <- list()
for (said in SAids){
  #extract FM and BV for one sample
  FMsa <- exampleData$FM[exampleData$FM$SAid==said,]
  BVsa <- exampleData$BV[exampleData$BV$FMid %in% FMsa$FMid,]
  
  BVsa <- fixInclusionProbabilities(BVsa)
  
  #extract estimation objects
  estimationObjectCountAtAge6 <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHiearchy = "A", stat="numberAtAge6")

  #estimation
  result <- computeDBEresultsTotalPointLowSingleSample(estimationObjectCountAtAge6, unitId = said)  
  results[[paste("id", said)]] <- result
}

