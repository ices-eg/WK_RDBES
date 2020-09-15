#
# Drafts for function signatures. Dont implement yet
#

#' sampling unit totals
#' 
#' @details contains design parameters and sample totals for a sampling unit
#'  \describe{
#'   \item{DesignTable}{data.frame with columns 'aboveId', 'id' and 'value' and design variables}
#'   \item{IncProbMatrix}{\code{\link{IncProbMatrix}}}
#'  }
#' 
"sampUnitTotals"

#' Estimation objecy lower hierarhcy
#'
#'  @details list with the following members
#'  \describe{
#'   \item{Hierarchy}{character: Code identifying the type of lower hiearchy (A,B,C or D).}
#'   \item{Value}{character: Code identifying the total provided in the PSU design table}
#'   \item{PSU}{\code{\link{sampUnitTotals}}}
#'  }
#' 
#' @rname DBEestimObjLow
#' 
"DBEestimObjLow"

#' @param FMtable
#' @param BVtable
#' @param lowerHierarchy
#' @param poi character: idenitfying the paramter of interest
#' @return \code{\link{DBEestimObjLow}} lower hiearchy data prepared for estimation
doDBEestimationObjLow <- function(FMtable=NULL, BVtable=NULL, lowerHiearchy=c("A","B","C","D"), poi=c()){
  stop("Not Implemented")
}