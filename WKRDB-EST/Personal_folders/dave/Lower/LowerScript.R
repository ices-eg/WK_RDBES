
# Load the data

load('./data/BV.Rdata')
load('./data/FM.Rdata')

myList <- list(
  "FM" = FM
  ,"BV" = BV
)

myoutput <- getLowerIncProbs(table = myList,hierarchyType = "A",  BVtype = "age")

#' getLowerIncProbs Gets the inclusion probabilities for the RDBES lower hierarchies
#'
#' @param table Named list of tables (FM,BV)
#' @param hierarchyType Lower hierarchy type (A,B,C, or D)
#' @param BVtype Type of BV value we are interested in (e.g. weight)
#'
#' @return
#' @export
#'
#' @examples myoutput <- getLowerIncProbs(table = myList,hierarchyType = "A",  BVtype = "age")
getLowerIncProbs <- function(table, hierarchyType = "A", BVtype = "weight") {

  BV <- table[["BV"]]
  FM <- table[["FM"]]
  
  #head(FM)
  #BVtype <- "weight"
  
  # subset BV to the type we are interested in (e.g. weight)
  myBV <- BV[tolower(BV$BVtype)==tolower(BVtype),]
  
  # check each fish only has 1 weight row
  #mean(table(myBV$BVfishID))
  
  myBV$incProb <- myBV$BVprob
  
  # get rid of 0s and NAs
  #myBV <- myBV[myBV$BVtotal != 0 
   #            & !is.na(myBV$BVtotal) 
    #           & !is.na(myBV$BVsampled),]
  
  # inclusion probability 
  myBV$incProb <- ifelse(is.na(myBV$incProb) & 
                           (!is.na(myBV$BVsampled) & !is.na(myBV$BVtotal)
                            & myBV$BVtotal >0) 
                         ,myBV$BVsampled / myBV$BVtotal,myBV$incProb)
  
  #all( myBV$incProb >= 0 , myBV$incProb<=1)
  
  
  if (nrow(myBV[is.na(myBV$incProb) | myBV$incProb <= 0 | myBV$incProb>1, ,]) >0){
    #print("Error!")
    warning("Error - inclusion probability is either NA, less than zero or greater than 1. These values were replaced with NA")
    myBV[is.na(myBV$incProb) | myBV$incProb <= 0 | myBV$incProb>1, "incProb"] <- NA
  }
  
  #myBV[,c("BVvalue","incProb")]
  
  
  myFM <-FM
  # always a census at FM level
  #myFM$incProb <- 1.0
  
  
  FMBV <- merge(myFM,myBV,by="FMid")
  FMBV$SAid.y <- NULL
  FMBV$SAid <- FMBV$SAid.x
  FMBV$SAid.x <- NULL
  
  #FMBV[,c("SAid","FMid","incProb","BVvalue")]
  
  myOutput <- list()
  
  myOutput[["hierarchy"]] <- hierarchyType
  
  
  # 1 row for each length class/said/fmid/number at unit
  PSU <- unique(FMBV[,c("SAid","FMid","FMclass","FMnumAtUnit")])
  
  # now do R stuff to get 1 row for each fish 
  PSU_multiplied <- PSU[rep(1:nrow(PSU),PSU$FMnumAtUnit),]
  # Set the number equal to 1 (we have 1 row for each fish now)
  PSU_multiplied$FMnumAtUnit <- 1
  
  # Create our inc prob matrix
  incProbMatrix <- as.matrix(rep(1,nrow(PSU_multiplied)))
  # Assing the same row names as the design table (PSU_multiplied)
  rownames(incProbMatrix)<- rownames(PSU_multiplied)
  
  myOutput[["PSU"]] <- list(
    "tableName" = "FM"
    ,"designTable" = PSU_multiplied
    ,"incProbMatrix" = incProbMatrix
  )
  
  
  myBVDesignTable <- myBV[,c("FMid","BVid","BVvalue")]
  rownames(myBVDesignTable) <- myBVDesignTable$BVid
  
  incProbMatrixBV <- as.matrix(myBV$incProb)
  rownames(incProbMatrixBV) <- rownames(myBVDesignTable)
  
  
  myOutput[["SSU"]] <- list(
    "tableName" = "BV"
    ,"designTable" = myBVDesignTable
    ,"incProbMatrix" = incProbMatrixBV
  )
  
  myOutput

}


