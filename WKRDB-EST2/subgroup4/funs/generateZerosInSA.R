generateZerosInSA<-function(sppCodes, SA = SA, SS = SS, SL = SL){
	
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
