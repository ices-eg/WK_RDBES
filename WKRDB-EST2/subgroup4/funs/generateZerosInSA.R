generateZerosInSA<-function(sppCodes, SA = SA, SS = SS, SL = SL){
	
	 # accidentally sampled: If spp in SA is not in SL, means not systematically recorded by observes so...
		# SA$SAsppNotInList<-FALSE
		# test <- !SA$SAspeCode %in% SL$SLspeCode 
		# SA$SAsppNotInList[test,]<-TRUE
		
		# else {
		# add column
		# }
			# SA[test,"SAnumTotal"]<-NA
			# SA[test,"SAnumSamp"]<-NA
			# SA[test,"SAtotalWtLive"]<-NA
			# SA[test,"SAsampleWtLive"]<-NA
			# SA[test,"SAlowHierarchy"]<-"D" 
			# SA[test,"SAsamp"]<-"N" 
		
		# SArecordsToEliminateInLowerRecords<-SA$SAid[test]
		# lowerRecordsToEliminate_B<-SA$SAid[test & is.na(SA$SAlowHierarchy) & SA$SAlowHierarchy=="B"]
		# lowerRecordsToEliminate_C<-SA$SAid[test & is.na(SA$SAlowHierarchy) & SA$SAlowHierarchy=="C"]
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
		if(sppCode %in% SL$SLspeCode) { # sppCode is not in list
                    if(!sppCode %in% x$SAspeCode) { 
						# duplicates SA row
							y<-x[1,]
							y$SAspeCode<-sppCode
							y$SAnumTotal<-0
							y$SAnumSampled<-0
							y$SAtotalWtLive<-0
							y$SAsampWtLive<-0
							y$SAid<-min(x$SAid)-0.001 # maintain a count
							y$SAseqNum<-min(x$SAseqNum)-0.001 # maintain a count
							y$SAunitName<-min(x$SAid)-0.001 # maintain a count
							y$SAsex<-NA
							y$SAlowHierarchy<-"D"
							y$SAsamp<-"N" 
	
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
