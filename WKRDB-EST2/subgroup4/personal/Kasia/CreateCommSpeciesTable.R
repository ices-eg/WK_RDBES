SL<-myNewTestData$SL
SL$SLspeciesCode<-c('125802','126554','126555','125946','126822','126821','126820','126417','126425')
SL$SLcommercialTaxon<-c('125802','125802','125802','125946','125946','125946','125946','126417','126425')
SA<-myNewTestData$SA[myNewTestData$SA$SAid==1,]
SA$SAspeciesCode <-'125802'
SA$SAtotalWeightLive <- 400
SA$SAsampleWeightLive <- 20
SA$SAnumberTotal <- 20

SA <- SA[, c("SAid","SAsequenceNumber","SAparentSequenceNumber","SAunitType","SAspeciesCode", "SAnumberSampled","SAnumberTotal",
             "SAsampleWeightLive","SAtotalWeightLive","SAstratification","SAstratumName")]

new_rows_EX2 <- data.frame(SAid = 2:3, SAsequenceNumber = 2:3, SAparentSequenceNumber = c(1,1), SAunitType = c('Basket content','Basket content'),
                       SAspeciesCode = c(126554,126555), SAnumberSampled = c(7,5),SAnumberTotal =  c(14,10),
                       SAsampleWeightLive = c(5,5), SAtotalWeightLive = c(10,10),SAstratification= c('Y','Y'),
                       SAstratumName=c('within_box_strata1','within_box_strata2'))
SA <- rbind(SA, new_rows_EX2)

Commnew<-NULL
Sapmle2<-NULL
sppcode <- 125802 #Lophiidae

###function
CreateCommSpeciesTable <- function (sppcode, SA=SA, SL=SL){

###Example 1

newSL <- SL[SL$SLcommercialTaxon == sppcode,c("SLcommercialTaxon","SLspeciesCode")]
newSL <- newSL[newSL$SLspeciesCode != sppcode,]

  ls1 <- split(SA, SA$SAsequenceNumber)
  ls2 <- lapply(ls1, function(x) {
    for (i in 1:nrow(newSL))
      {
      if (is.na(x$SAparentSequenceNumber) && SA$SAstratification =='N' && nrow(newSL) != 0){
        for (j in 1:nrow(newSL))
        {
         y<-x[j,]
        y$SAspeciesCode <- newSL$SLspeciesCode[j] 
        y$SAnumberTotal <- 0
        y$SAnumberSampled <- 0
        y$SAtotalWeightLive <- 0
        y$SAsampleWeightLive <- 0
        y$SAid<- c('')
        y$SAsequenceNumber<- c('')
        y$SAparentSequenceNumber<- c('')
        x <- rbind(y, x); x}} else x
    }
    x<-x[order(x$SAid, decreasing=F),]
    x
  })
  
  newSA<-do.call("rbind", ls2)
  SA<-newSA
  newSA<-newSA[newSA$SAparentSequenceNumbe=='' | is.na(newSA$SAparentSequenceNumber),c("SAparentSequenceNumber","SAspeciesCode","SAtotalWeightLive")]
  #
    new<-newSA[,c("SAspeciesCode","SAtotalWeightLive")]
    names(new) <- c("SAspeciesCode",sppcode)
    Commnew<-rbind(Commnew,new)
  
#Example 2
#############

ls1 <- split(SA, SA$SAsequenceNumber)
ls2 <- lapply(ls1, function(x) {
  for (i in 1:nrow(SA[is.na(SA$SAparentSequenceNumber),]))
  {
    if( is.na(x$SAparentSequenceNumber) && SA$SAstratification !='Y'){
        y<-x[1,]
        y$SAparentSequenceNumber<-x$SAsequenceNumber
        y$SAunitType <- paste(x$SAunitType," content", sep="")
        y$SAnumberTotal <- 0
        y$SAnumberSampled <- 0
        y$SAtotalWeightLive <- 0
        y$SAsampleWeightLive <- 0
        y$SAid<-c('')
        y$SAsequenceNumber <- c('')
        x <- rbind(y, x); x
        cos<-x
        } else x
}
    x<-x[order(x$SAid, decreasing=F),]
    x
  })

newSA<-do.call("rbind", ls2)

newSA<-newSA[!is.na(newSA$SAparentSequenceNumber) & newSA$SAparentSequenceNumber!='',c("SAparentSequenceNumber","SAspeciesCode","SAtotalWeightLive")]

for (i in 1:length(unique(newSA$SAparentSequenceNumber))){
  new<-newSA[newSA$SAparentSequenceNumber == i ,c("SAspeciesCode","SAtotalWeightLive")]
  names(new) <- c("SAspeciesCode",sppcode)
  Sapmle2<-rbind(Sapmle2,new)
}
CommercialNew<-merge(Commnew,Commnew2, by.x="SAspeciesCode", by.y="SAspeciesCode")
}
out<-CreateCommSpeciesTable(sppcode = 125802, SA=SA, SL=SL)
out
