#
# Drafts for function signatures. Dont implement yet
#

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
#'   \item{PSUs}{list of data for each PSU with members formatted as \code{\link{sampUnitData}}}
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
  
  if (lowerHiearchy == "A"){
    BVtable <- makeBVtable(FMtable, BVtable)
  }
  else if (lowerHiearchy == "B"){
    stopifnot(is.null(BVtable))
    BVtable <- makeBVtable(FMtable, NULL)
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