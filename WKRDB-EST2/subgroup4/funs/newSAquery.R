
newSAquery<-function(sppCodes, SA = SA, SS = SS, SL = SL){
	
	ls1 <- split(SA, SA$SSid)
    ls2 <- lapply(ls1, function(x) {
		for (sppCode in sppCodes)
		{
			if(!sppCode %in% SL$SLspeCode){
				y<-x[1,]
				y$SAspeCode<-sppCode
				y$SAnumTotal<-NA
				y$SAnumSamp<-NA
				y$SAtotalWtLive<-NA
				y$SAsampWtLive<-NA
				y$SAid<-min(x$SAid)-0.001 # maintain a count
				y$SAseqNum<-min(x$SAseqNum)-0.001 # maintain a count
				y$SAunitName<-min(x$SAid)-0.001 # maintain a count
				y$SAsex<-NA
				y$SAlowHierarchy<-"D"
				y$SAsamp<-"N" 
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
