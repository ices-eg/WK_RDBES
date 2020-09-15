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
#' @noRd
makeBVtable <- function(FMtable, BVtable=NULL){
  if (is.null(FMtable)){
    return(BVtable)
  }
  if (is.null(BVtable)){
    
    bvnames <- c("BVid","BVrecType","BVfishId","BVstratification","BVstratumname","BVtype","BVvalue","BVvalTyp","BVmethod","BVmeasEquip","BVnumTotal","BVnumSamp","BVselProp","BVinclProp","BVselectMeth","BVunitName","BVsampler","FMid","SAid")
    
  }
  
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

#' @details 
#'  The parameter 'stat' specifies the statstic of interest and may be specified as:
#'  \descibe{
#'   \item{number}{the total number of specimens}
#'  }
#' 
#' @param FMtable
#' @param BVtable
#' @param lowerHierarchy character identifying the lower hierarchy to extract
#' @param stat character identifying the statistic of interest
#' @return \code{\link{DBEestimObjLow}} lower hiearchy data prepared for estimation
doDBEestimationObjLow <- function(FMtable=NULL, BVtable=NULL, lowerHiearchy=c("A","B","C","D"), stat=c("number")){
  
  stop("Figure out how SA should factor in")
  
  if (lowerHiearchy == "A"){
    BVtable <- makeBVtable(FMtable, BVtable)
  }
  else if (lowerHiearchy == "B"){
    stopifnot(is.null(BVtable))
    BVtable <- makeBVtable(FMtable)
  }
  else if (lowerHiearchy == "C"){
    stopifnot(is.null(FMtable))
    BVtable <- BVtable
  }
  else if (lowerHiearchy == "D"){
    stop("No lower hierarchy estimation possible for lower hierarchy D.")
  }
  else{
    stop("Lower hierarchy " + lowerHiearchy + " is not implemented.")
  }
  
  if (stat=="number"){
    
  }
  else{
    stop("Option " + stat + " is not supported for parameter 'stat'.")
  }
}