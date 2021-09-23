## Updated to have the short column names in the code

## data with the long column names is replaced by a dataset with the R names (short ones) 
# load("Inputs/H1_SRSWR.Rdata")
# load("Inputs/H1_SA_SRSWOR.Rdata")

myNewTestData<-readRDS("inputs/DBErawObj_DK_1966_H3.rds")

source("Funs/generateZerosInSA.R")

SL <- myNewTestData$SL
SA <- myNewTestData$SA
SS <- myNewTestData$SS

# data checks
source("funs/checkSampMeth.R")
source("funs/checkStratif.R")
source("funs/checkCluster.R")
source("funs/findDiffObsTyp.R")
	# x3 is data with issues for testing the functions
	x3 <- data.frame(SSid=c('11','21','31','41','51','61'), FOid=c(1,2,3,4,5,6),SSobsTyp=c('vis','vol','vis','vol','vis',NA),
	SSselectMeth=c('SRSWR','CENSUS','SRSWR','CENSUS','CENSUS','CENSUS'),
	SSstratification=c('Y','N','N','N','N','N'),SSclustering=c('N','N','Y','N','N','N'))

findDiffObsTyp(x=SS)
checkCluster(x=SS)
checkSampMeth(x=SS)
checkStratif(x=SS)

# demonstrates use of generateZerosInSA in adding zeros and missing values to SA table based on SL information

out<-generateZerosInSA(sppCode = SL$SLspeCode, SA = SA, SS = SS, SL = SL)
newSA<-do.call("rbind", out)
newSA


# demonstrates a query of a species to the SA table (comparing it with what was reported in SL)

source("funs/newSAquery.R")

out<-newSAquery(sppCode = c("9999"), SA = newSA, SL = SL)
newSA<-do.call("rbind", out)


  # example test: spp queried is not in the list
	SA$SAspeCode[1]<-"-9"
	out<-generateZerosInSA(sppCode = 100685, SA = SA, SS = SS, SL = SL)
	SA$SAspeCode[1]<-"100684"
	newSA<-do.call("rbind", out)


