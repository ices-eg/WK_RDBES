
# Load the data

load('./data/BV.Rdata')
load('./data/FM.Rdata')

# Use this value for testing the function
BV$BVselectMeth <- 'SRSWOR'

myList <- list(
  "FM" = FM
  ,"BV" = BV
)

myoutput <- getLowerProbs(table = myList,hierarchyType = "A",  BVtype = "age", probType = "inclusion" )



#' getLowerIncProbs Gets the inclusion probabilities for the RDBES lower hierarchies
#'
#' @param table Named list of tables (FM,BV)
#' @param hierarchyType Lower hierarchy type (A,B,C, or D)
#' @param BVtype Type of BV value we are interested in (e.g. weight)
#' @param type Either selection or inclusion
#'
#' @return
#' @export
#'
#' @examples myoutput <- getLowerIncProbs(table = myList,hierarchyType = "A",  BVtype = "age")
getLowerProbs <- function(table, hierarchyType = "A", BVtype = "weight", probType = "inclusion") {

  # For testing
  #table <- myList
  #hierarchyType <- 'A'
  #BVtype <- "weight"
  #probType <- "inclusion"
  
  if (hierarchyType != "A") stop ("This function is only implemented for lower hierarchy A at the moment")
  
  BV <- table[["BV"]]
  FM <- table[["FM"]]
  
  #head(FM)
  #BVtype <- "weight"
  
  # subset BV to the type we are interested in (e.g. weight)
  myBV <- BV[tolower(BV$BVtype)==tolower(BVtype),]
  
  uniqueSelMethod <- as.character(unique(myBV$BVselectMeth))
  
  if(sum(is.na(uniqueSelMethod))>0) stop ("Cannot proceed: NAs in selectMeth")
  if(length(uniqueSelMethod)>1) stop ("Cannot proceed: two different selection methods")
  
  
  # check each fish only has 1 weight row
  #mean(table(myBV$BVfishID))
  
  # Selection probabilities
  if (tolower(probType) == "selection")
  {
    if( uniqueSelMethod %in% c("SRSWR" , "SRSWOR") )
    {
      if( uniqueSelMethod == "SRSWR") {
        if (nrow(myBV[myBV$BVtotal <0,]))
        {
          stop ("Zeros in BVtotal - can't proceed")
        }
        myBV$prob <-  1/myBV$BVtotal
      }
      if( uniqueSelMethod == "SRSWOR") stop ("Cannot proceed: Selection probability for SRSWOR depends on order") # Could do this if we assume fish are numbered sequentially....
    }
    if( uniqueSelMethod %in% c("UPSWR" , "UPSWOR"))
    {
      stop ("Cannot proceed: Selection probability is not in the data model (yet?)")

    }
  }
  # Inclusion probability
  else if (tolower(probType) == "inclusion")
  {
    if(length(uniqueSelMethod)>1) { 
      stop ("Cannot proceed: two different selection methods")
    } else {
      #print(a)
      if( uniqueSelMethod %in% c("SRSWR" , "SRSWOR") )
      {
        if(sum(is.na(myBV$BVtotal))>0) stop ("Cannot proceed: NAs in total")
        if(sum(is.na(myBV$BVsampled))>0) stop ("Cannot proceed: NAs in sampled")
        if( uniqueSelMethod == "SRSWR") {
          if (nrow(myBV[myBV$BVtotal <0,]))
          {
            stop ("Cannot proceed: Zeros in BVtotal")
          }
          myBV$prob <-  1-(1-1/myBV$BVtotal)^myBV$BVsampled
        }
        if( uniqueSelMethod == "SRSWOR"){
          if (nrow(myBV[myBV$BVtotal <0,]))
          {
            stop ("Cannot proceed: Zeros in BVtotal")
          }
          myBV$prob <-  myBV$BVsampled/myBV$BVtotal
        }
      }
      if( uniqueSelMethod %in% c("UPSWR" , "UPSWOR"))
      {
        if(sum(is.na(myBV$BVprob))>0) stop ("Cannot proceed: NAs in BVprob - for UPSWR or UPSWOR these values are needed")
      }
    }
    
  }

  

  myFM <-FM
  # IMPORTANT: always a census at FM level

  # Join FM and BV together
  FMBV <- merge(myFM,myBV,by="FMid")
  FMBV$SAid.y <- NULL
  FMBV$SAid <- FMBV$SAid.x
  FMBV$SAid.x <- NULL
  
  # Create the structure to hold our output
  myOutput <- list()
  
  myOutput[["hierarchy"]] <- hierarchyType
  
  
  # 1 row for each length class/said/fmid/number at unit
  PSU <- unique(FMBV[,c("SAid","FMid","FMclass","FMnumAtUnit")])
  # Rename columns
  PSU$aboveID <- PSU$SAid
  PSU$SAid <- NULL
  PSU$id <- PSU$FMid
  PSU$FMid <- NULL
  
  # now do clever R stuff to get 1 row for each fish 
  PSU_multiplied <- PSU[rep(1:nrow(PSU),PSU$FMnumAtUnit),]
  # Set the number equal to 1 (we have 1 row for each fish now)
  PSU_multiplied$FMnumAtUnit <- 1
  
  # Create our prob matrix
  probMatrix <- as.matrix(rep(1,nrow(PSU_multiplied)))
  # Assing the same row names as the design table (PSU_multiplied)
  rownames(probMatrix)<- rownames(PSU_multiplied)
  
  # Here's our first output
  myOutput[["PSU"]] <- list(
    "tableName" = "FM"
    ,"designTable" = PSU_multiplied
    ,"incProbMatrix" = probMatrix
  )
  
  # Now deal with the BV table
  myBVDesignTable <- myBV[,c("FMid","BVid","BVvalue")]
  # Rename columns
  myBVDesignTable$aboveID <- myBVDesignTable$FMid
  myBVDesignTable$FMid <- NULL
  myBVDesignTable$id <- myBVDesignTable$BVid
  myBVDesignTable$BVid  <- NULL
  
  # Set the row names to be equal to BVid (this is guaranteed unique)
  rownames(myBVDesignTable) <- myBVDesignTable$BVid
  
  probMatrixBV <- as.matrix(myBV$prob)
  rownames(probMatrixBV) <- rownames(myBVDesignTable)
  
  # Here's our SSU output
  myOutput[["SSU"]] <- list(
    "tableName" = "BV"
    ,"designTable" = myBVDesignTable
    ,"probMatrix" = probMatrixBV
  )
  
  myOutput

}


