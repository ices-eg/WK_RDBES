#
# Drafts for function signatures. Dont implement yet
#

#' Transforms a FM and BV table into a length-stratified BV table
#' 
#' @details 
#'  if BVtable is NULL, a BVtable with BVtype and BVvalue set to NA will be created
#' 
#' @param FMtable
#' @param BVtable
#' @param stratificationYes code for encoding stratification in BVstratification. Included as default argument for now.
#' @noRd
makeBVtable <- function(FMtable, BVtable, stratificationYes="Y"){
  if (is.null(FMtable)){
    return(BVtable)
  }
  
  FMtable$stratname <- as.character(FMtable$FMclass)
  BVtable <- merge(BVtable, FMtable[,c("FMid", "stratname")], by="FMid", all.x=T)
  BVtable$BVstratification <- stratificationYes
  
  #make sure any additional stratification is added to the FM-stratification
  BVtable$BVstratumname <- paste(paste("FM:", BVtable$stratname, sep=""), BVtable$BVstratumname, sep="-") 

  #check that totals match FM table
  getfirst <- function(x){stopifnot(length(unique(x))==1); return(x[1]);}
  totalsInFmclass <- aggregate(list(tot=BVtable$BVnumTotal), by=list(FMid=BVtable$FMid), FUN=getfirst)
  comptotals <- merge(totalsInFmclass, FMtable)
  stopifnot(all(comptotals$tot == comptotals$FMnumAtUnit))
  
  #remove column not in BV definition
  BVtable$stratname <- NULL
  
  return(BVtable)
}

#' sampling unit totals
#' 
#' @details contains design parameters and sample totals for a sampling unit
#'  \describe{
#'   \item{StatisticTable}{data.frame with the statistic of interest for each sampling unit}
#'   \item{DesignTable}{data.frame with columns 'aboveId', 'id',  and the design parameters for each sampling unit}
#'   \item{jiProb}{matrix with joint inclusion probabilites}
#'  }
#' 
"sampUnitData"

#' Estimation objecy lower hierarhcy
#'
#'  @details list with the following members
#'  \describe{
#'   \item{Hierarchy}{character: Code identifying the type of lower hiearchy (A,B,C or D).}
#'   \item{Value}{character: Code identifying the total provided in the PSU design table}
#'   \item{sampleData}{list of data for each sample with members formatted as \code{\link{sampUnitData}}}
#'  }
#' 
#' @rname DBEestimObjLow
#' 
"DBEestimObjLow"

#' Prepares specimen parameters
#' 
#' @description 
#'  Prepares data for estimation of statistics calculated for specimen parameters.
#'  Should be applied to BV and FM table from a single sample. This is enforced.
#'
#' @details 
#'  Estimation of number at length can be based on individual parameters (BV table) or sorting in length groups (FM table). 
#'  This is not supported by this function.
#'  (Perhaps FMtable could be prepared directly to the format 'DBEresultsTotalPointLow' returned by computeDBEresultsTotalPointLow)
#' 
#'  The parameter 'stat' specifies the statstic of interest and may be specified as:
#'  \descibe{
#'   \item{number}{the total number of specimens}
#'  }
#' 
#' @param FMtable for a specific sample
#' @param BVtable for a specific sample
#' @param lowerHierarchy character identifying the lower hierarchy to extract
#' @param stat character identifying the statistic of interest, for now this supports the somewhat contrived options 'number' and 'countAtAge6'
#' @return \code{\link{sampUnitData}} lower hiearchy data prepared for estimation
doDBEestimationObjLowSpecimenParams <- function(FMtable=NULL, BVtable=NULL, lowerHierarchy=c("A","C"), stat=c("number", "numberAtAge6", "numberAtAge6"), ages=1:20){
  
  if (lowerHierarchy == "A"){
    if (any(is.na(FMtable$SAid)) | length(unique(FMtable$SAid))>1){
      stop("Need unique sample (SAid)")
    }
    if (any(is.na(BVtable$FMid)) | !all(BVtable$FMid %in% FMtable$FMid)){
      stop("BVtable does not correspond to FMtable")
    }
    BVtable <- makeBVtable(FMtable, BVtable)
  }
  else if (lowerHierarchy == "B"){
    stop("No estimation from specimen parameters is possible for lower hierarchy B.")  }
  else if (lowerHierarchy == "C"){
    stopifnot(is.null(FMtable))
    if (any(is.na(BVtable$SAid)) | length(unique(BVtable$SAid))>1){
      stop("Need unique sample (SAid)")
    }
    BVtable <- BVtable
  }
  else if (lowerHierarchy == "D"){
    stop("No lower hierarchy estimation possible for lower hierarchy D.")
  }
  else{
    stop("Lower hierarchy " + lowerHierarchy + " is not implemented.")
  }
  
  if (stat=="number"){
    BVtable <- BVtable[!duplicated(BVtable$BVfishId),]
    BVtable$count <- 1
    var <- "count"
  }
  else if (stat=="numberAtAge6"){
    BVtable <- BVtable[BVtable$BVtype=="Age",]
    stopifnot(!any(duplicated(BVtable$BVfishId)))
    BVtable$countAtAge6 <- as.numeric(BVtable$BVvalue == 6)
    var <- "countAtAge6"
  }
  else if (stat=="numberAtAge"){
    BVtable <- BVtable[BVtable$BVtype=="Age",]
    stopifnot(!any(duplicated(BVtable$BVfishId)))
    
    catnames <- c()
    for (cat in ages){
      catn <- paste("Age", cat)
      BVtable[,catn] <- as.numeric(BVtable$BVvalue == cat) 
      catnames <- c(catnames, catn)
    }
    
    var <- catnames
  }
  
  else{
    stop("Option ", stat, " is not supported for parameter 'stat'.")
  }
  
  output <- list()
  output$StatisticTable <- BVtable[,c("BVfishId", var)]
  names(output$StatisticTable) <- c("id", var)
  output$DesignTable <- BVtable[,c("BVfishId", "BVstratification", "BVstratumname", "BVselectMeth", "BVnumTotal", "BVnumSamp", "BVselProp", "BVinclProp")]
  names(output$DesignTable) <- c("id", "stratification", "stratumname", "selectMeth", "numTotal", "numSamp", "selProb", "inclProb")
  output$jiProb <- matrix(nrow=nrow(output$DesignTable), ncol=nrow(output$DesignTable))
  colnames(output$jiProb) <- output$DesignTable$id
  rownames(output$jiProb) <- output$DesignTable$id
  
  return(output)
}