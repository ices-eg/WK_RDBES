# ===================
# Demonstration of approach to the generation of true zeros and NAs in SA table using SL table of RDBES
# Nuno with Lucia, Henrik, Sven, Josefina, Liz, Karolina @ ICES WKRDB-EST, 10/2019
# ===================

    # wishlist
        # multi-species consultation
        # handling of the generated SAid (suggestion in "1" -> "1a"
        # handling of parentIds and stratification
handleSSissues <- 

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

	

