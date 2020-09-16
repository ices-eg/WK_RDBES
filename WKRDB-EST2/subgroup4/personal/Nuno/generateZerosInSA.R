# ===================
# Demonstration of approach to the generation of true zeros and NAs in SA table using SL table of RDBES
# Nuno with Lucia, Henrik, Sven, Josefina, Liz, Karolina @ ICES WKRDB-EST, 10/2019
# ===================

    # wishlist
        # multi-species consultation
        # handling of the generated SAid (suggestion in "1" -> "1a"
        # handling of parentIds and stratification

################################################################################################
##############################################
##### WKRDB-EST2 14-18.Sept.2020
##### ACF
## Read data
setwd('D:/Documents/PNAB/2020/006_WKRDBES/WKRDB-EST2/WK_RDBES-master/WKRDBEST2/
testData/output/DBErawObj/')
dataset<-readRDS("DBErawObj_DK_1966_H3.rds")
str(dataset)
SL<-dataset$SL
SS<-dataset$SS
SA<-dataset$SA
# test data
x <- data.frame(FOid=c(1,2,2,3,4,5,5,6),SSobsTyp=c('vis','vis','vol','vis','vol','vis','vol',NA))
x1 <- data.frame(FOid=c(1,2,2,3,4,5,5),SSobsTyp=c('vis','vis','vol','vis','vol','vis','vol'))
x2 <- data.frame(FOid=c(1,2,3,4,5,6),SSobsTyp=c('vis','vol','vis','vol','vis',NA))
# function
findDiffObsTyp<- function(FOid, x1 = x1){
x <- x1
for (i in 1:length(unique(x$FOid))) {
a <- as.character(unique(x$SSobsTyp))
if(sum(is.na(x$SSobsTyp))>0) stop ("cannot proceed: NAs in obsTyp")
if(length(unique(x$SSobsTyp))>1) stop ("two different observation types in the same Fishing Operation")
y=table(x$FOid,x$SSobsTyp)
}
print(y)
}
out <- findDiffObsTyp(FOid = x1$FOid, x1 = x1)


generate_zeros_in_SA<-function(sppCodes, SA = SA, SS = SS, SL = SL){
	
	 # accidentally sampled: If spp in SA is not in SL, means not systematically recorded by observes so...
		# SA$SAsppNotInList<-FALSE
		# test <- !SA$SAspeciesCode %in% SL$SLspeciesCode 
		# SA$SAsppNotInList[test,]<-TRUE
		
		# else {
		# add column
		# }
			# SA[test,"SAnumberTotal"]<-NA
			# SA[test,"SAnumberSampled"]<-NA
			# SA[test,"SAtotalWeightLive"]<-NA
			# SA[test,"SAsampleWeightLive"]<-NA
			# SA[test,"SAlowerHierarchy"]<-"D" 
			# SA[test,"SAsampled"]<-"N" 
		
		# SArecordsToEliminateInLowerRecords<-SA$SAid[test]
		# lowerRecordsToEliminate_B<-SA$SAid[test & is.na(SA$SAlowerHierarchy) & SA$SAlowerHierarchy=="B"]
		# lowerRecordsToEliminate_C<-SA$SAid[test & is.na(SA$SAlowerHierarchy) & SA$SAlowerHierarchy=="C"]
		# if(length(lowerRecordsToEliminate_A)>0)
			# {
			# FM<-FM[!FM$SAid %in% lowerRecordsToEliminate_A,]
			# BV<-BV[!BV$FMid %in% FM$FMid,]
			# }
		# if(length(lowerRecordsToEliminate_A)>0)
			# {
			# delete on FM
			# }
		# if(length(lowerRecordsToEliminate_A)>0)
			# {
			# delete on BV
			# }
	



	
		#findDiffObsTyp (x = SS)
	
	
	
	ls1 <- split(SA, SA$SSid)
    ls2 <- lapply(ls1, function(x) {
		for (sppCode in sppCodes)
		{
		if(sppCode %in% SL$SLspeciesCode) { # sppCode is not in list
                    if(!sppCode %in% x$SAspeciesCode) { 
						# duplicates SA row
							y<-x[1,]
							y$SAspeciesCode<-sppCode
							y$SAnumberTotal<-0
							y$SAnumberSampled<-0
							y$SAtotalWeightLive<-0
							y$SAsampleWeightLive<-0
							y$SAid<-min(x$SAid)-0.001 # maintain a count
							y$SAsequenceNumber<-min(x$SAsequenceNumber)-0.001 # maintain a count
							y$SAunitName<-min(x$SAid)-0.001 # maintain a count
							y$SAsex<-NA
							y$SAlowerHierarchy<-"D"
							y$SAsampled<-"N" 
	
					x <- rbind(y, x); x} else x
					}
		}
        x<-x[order(x$SAid, decreasing=F),]
        x
		})

    # shows execution steps
    cat("\n sampled \n")
    print(SA)
    # print("list")
    # print(sppFrame)
    cat("\n outcome with zeros \n")   
    print(ls2)   

}

#dataset<-readRDS("Inputs/DBErawObj_DK_1966_H1.rds")
load("Inputs/H1_SRSWR.Rdata")
load("Inputs/H1_SA_SRSWOR.Rdata")
SL <- myNewTestData$SL
SA <- myNewTestData$SA
SS <- myNewTestData$SS

 out<-generate_zeros_in_SA(sppCode = SL$SLspeciesCode, SA = SA, SS = SS, SL = SL)
	newSA<-do.call("rbind", out)

# if we query a species not in list we get an NA

newSA_query<-function(sppCodes, SA = SA, SS = SS, SL = SL){
	
	ls1 <- split(SA, SA$SSid)
    ls2 <- lapply(ls1, function(x) {
		for (sppCode in sppCodes)
		{
			if(!sppCode %in% SL$SLspeciesCode){
				y<-x[1,]
				y$SAspeciesCode<-sppCode
				y$SAnumberTotal<-NA
				y$SAnumberSampled<-NA
				y$SAtotalWeightLive<-NA
				y$SAsampleWeightLive<-NA
				y$SAid<-min(x$SAid)-0.001 # maintain a count
				y$SAsequenceNumber<-min(x$SAsequenceNumber)-0.001 # maintain a count
				y$SAunitName<-min(x$SAid)-0.001 # maintain a count
				y$SAsex<-NA
				y$SAlowerHierarchy<-"D"
				y$SAsampled<-"N" 
				x <- rbind(y, x); x} else x
		}			
        x<-x[order(x$SAid, decreasing=F),]
        x
		})
	# shows execution steps
    cat("\n sampled \n")
    print(SA)
    # print("list")
    # print(sppFrame)
    cat("\n outcome with zeros \n")   
    print(ls2)

} 
 
 	newSA_query(sppCode = "9999", SA = newSA, SL = SL)

 
 # example test: spp outside the list
	SA$SAspeciesCode[1]<-"-9"
	out<-generate_zeros_in_SA(sppCode = 100685, SA = SA, SS = SS, SL = SL)
	SA$SAspeciesCode[1]<-"100684"

	newSA<-do.call("rbind", out)

	

