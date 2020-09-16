#
# Drafts for function signatures. Dont implement yet
#


#' Results from estimation
#' 
#' @details results from restimation, list with members
#'  \describe{
#'    \item{statistic}{character: description of statistic}
#'    \item{sampleUnit}{character: name of sampling unit, e.g. SA}
#'    \item{sampleId}{list: ids of sampling unit, e.g. SAid}
#'    \item{totals}{list: estimate of total for statistic}
#'    \item{variance}{list: estimate of variance for statistic}
#'  }
#' 
#' @rname DBEresultsTotalPointLow
#' 
"DBEresultsTotalPointLow"

#' @param estimationObj \code{\link{DBEestimObjLow}}
#' @return \code{\link{DBEresultsTotalPointLow}} with point estimate
computeDBEresultsTotalPointLow <- function(estimationObj){
  stop("Not Implemented")
}