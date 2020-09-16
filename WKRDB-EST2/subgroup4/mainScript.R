# demonstrates use of generateZerosInSA in adding zeros and missing values to SA table based on SL information


load("Inputs/H1_SRSWR.Rdata")
load("Inputs/H1_SA_SRSWOR.Rdata")
source("Funs/generateZerosInSA.R")

SL <- myNewTestData$SL
SA <- myNewTestData$SA
SS <- myNewTestData$SS

out<-generateZerosInSA(sppCode = SL$SLspeciesCode, SA = SA, SS = SS, SL = SL)
newSA<-do.call("rbind", out)
newSA


# demonstrates a query of a species to the SA table (comparing it with what was reported in SL)

source("Funs/newSA_query.R")

out<-newSA_query(sppCode = "9999", SA = newSA, SL = SL)
newSA<-do.call("rbind", out)

  # example test: spp queried is not in the list
	SA$SAspeciesCode[1]<-"-9"
	out<-generate_zeros_in_SA(sppCode = 100685, SA = SA, SS = SS, SL = SL)
	SA$SAspeciesCode[1]<-"100684"
	newSA<-do.call("rbind", out)

