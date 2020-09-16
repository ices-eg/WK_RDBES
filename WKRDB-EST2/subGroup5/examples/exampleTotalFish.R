#source files
for (f in list.files("funs", pattern="*.R")){
  source(file.path("funs", f))  
}

exampleData <- readRDS("inputs/modified_FMBV_list1.rds")
estimationObject <- doDBEestimationObjLow(exampleData$FM, exampleData$BV, lowerHiearchy = "A")