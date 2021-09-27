# ===================
# Demonstration of approach to the handling of stratification in SA table of RDBES
# Nuno @ ICES WKRDB-EST, 10/2019
# ===================

    # wishlist
        # application to full table
        # check the handling of stratification
        # remove for cycle

# generate hypothetical SA 
    SAsub <- data.frame(SAseqNum=c(1:5,6,7,8), SAparSequNum=c(NA, 1:4,NA,6,NA), stratification="N")
    SAstrat <- data.frame(SAseqNum=c(9:14), SAparSequNum=c(NA, rep(9,5)), stratification="Y")
	SAboth<-rbind(SAsub,SAstrat)

	SA<-SAstrat

# handling of subsampling
    df2 <- data.frame(id = SA$SAseqNum, parentId = SA[,2])
    i=2
    while(sum(!is.na(df2[,ncol(df2)]))>0)
    {
    df2[paste("sample",i, sep="")]<-df2[[paste("sample",1, sep="")]] [match(df2[[paste("sample",i-1, sep="")]], df2$id)]
    i=i+1
    }
# re-ordering (optional)
    aux<-t(apply(df2[,2:ncol(df2)],1,order))
    for(i in 1:nrow(df2)) {df2[i,2:ncol(df2)]<-df2[i,aux[i,]+1]}
    for(i in 1:nrow(df2)) {df2[i,min(which(is.na(df2[i,2:ncol(df2)])))+1]<-df2[i,"id"]}

# example of matching of inclusionProbs
    df_probs<-data.frame(id=c(1:8),probs=runif(8,0,1))
    for(i in 2:ncol(df2)) df2[,i]<-df_probs$probs[match(df2[,i], probs$id)] 
    df2
	ls2<-c(df2)
	
	as.data.frame(lapply(ls2,function(y=x[2:length(x)]) {y<-SA$SAincProb[match(y, SA$SAseqNum)];y}))
	m2<-as.matrix(df2)
	m2<-SA$SAincProb[match(m2, SA$SAseqNum)]
	df2[match(