


load("Inputs/H1_SRSWR.Rdata")
load("Inputs/H1_SA_SRSWOR.Rdata")
source("Funs/generateZerosInSA.R")

SL <- myNewTestData$SL
SA <- myNewTestData$SA
SS <- myNewTestData$SS

# data checks
source("funs/checkSampMeth.R")
source("funs/checkStratif.R")
source("funs/checkCluster.R")
source("funs/findDiffObsTyp.R")
	# x3 is data with issues
	x3 <- data.frame(SSid=c('11','21','31','41','51','61'), FOid=c(1,2,3,4,5,6),SSobsTyp=c('vis','vol','vis','vol','vis',NA),
	SSselectMeth=c('SRSWR','CENSUS','SRSWR','CENSUS','CENSUS','CENSUS'),
	SSstratification=c('Y','N','N','N','N','N'),SSclustering=c('N','N','Y','N','N','N'))

	findDiffObsTyp(x=x3)
	checkCluster(x=x3)
	checkSampMeth(x=x3)
	checkStratif(x=x3)

# demonstrates use of generateZerosInSA in adding zeros and missing values to SA table based on SL information

out<-generateZerosInSA(sppCode = SL$SLspeciesCode, SA = SA, SS = SS, SL = SL)
newSA<-do.call("rbind", out)
newSA


# demonstrates a query of a species to the SA table (comparing it with what was reported in SL)

source("funs/newSAquery.R")

out<-newSAquery(sppCode = "9999", SA = newSA, SL = SL)
newSA<-do.call("rbind", out)


  # example test: spp queried is not in the list
	SA$SAspeciesCode[1]<-"-9"
	out<-generate_zeros_in_SA(sppCode = 100685, SA = SA, SS = SS, SL = SL)
	SA$SAspeciesCode[1]<-"100684"
	newSA<-do.call("rbind", out)

