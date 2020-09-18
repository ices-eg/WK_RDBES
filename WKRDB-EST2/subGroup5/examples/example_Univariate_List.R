#source files
for (f in list.files("./WKRDB-EST2/subGroup5/funs/", pattern="*.R")){
  source(file.path("./WKRDB-EST2/subGroup5/funs/", f))
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


exampleData <- readRDS("./WKRDB-EST2/subGroup5/inputs/modified_FMBV_raw_list2.rds")


FMsa <- exampleData$FM
BVsa <- exampleData$BV
BVsa <- fixInclusionProbabilities(BVsa)

#extract estimation objects
estimationObjectNumber <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHierarchy = "A", stat="number")
estimationObjectCountAtAge6 <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHierarchy = "A", stat="numberAtAge6")

# It should break
estimationObjectCountAtAge <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHierarchy = "A", stat="numberAtAge")


#estimation
# computeDBEresultsTotalPointLowSingleSample(estimationObjectCountAtAge6)
# computeDBEresultsTotalPointLowSingleSample(estimationObjectNumber)

Reduce(c, lapply(estimationObjectCountAtAge6, computeDBEresultsTotalPointLowSingleSample))
Reduce(c, lapply(estimationObjectNumber, computeDBEresultsTotalPointLowSingleSample))

result_List_CAtAge6 <- lapply(estimationObjectCountAtAge6, computeDBEresultsTotalPointLowSingleSample)
result_List_Num <- lapply(estimationObjectNumber, computeDBEresultsTotalPointLowSingleSample)

# #mutlivariate estimation
# computeDBEresultsTotalPointLowSingleSample(estimationObjectCountAtAge)
