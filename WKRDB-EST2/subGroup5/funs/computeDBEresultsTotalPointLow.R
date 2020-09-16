#
# Drafts for function signatures. Dont implement yet
#


#' Results from estimation
#' 
#' @details results from restimation, list with members
#'  \describe{
#'    \item{statistic}{character: description of statistic}
#'    \item{unitId}{list: ids of unit the estimate is made for, e.g. SAid}
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

computeDBEresultsTotalPointLowSingleSample <- function(estimationObj, unitId="Uknown"){
  stratas <- unique(estimationObj$DesignTable$stratumname)
  statistic <- names(estimationObj$StatisticTable)[names(estimationObj$StatisticTable)!="id"]
  stratatotals <- 0
  for (s in stratas){
    stratafilter <- estimationObj$DesignTable$stratumname == s
    values <- estimationObj$StatisticTable[stratafilter,statistic]
    incProbs <- estimationObj$DesignTable[stratafilter, "inclProb"]
    stratatot <- sum(values/incProbs)
    stratatotals <- stratatotals + stratatot
  }
  
  
  #'    \item{statistic}{character: description of statistic}
  #'    \item{sampleId}{list: ids of sampling unit, e.g. SAid}
  #'    \item{totals}{list: estimate of total for statistic}
  #'    \item{variance}{list: estimate of variance for statistic}
  #'  }
  #'  
  output <- list()
  output$statistic <- statistic
  output$unitId <- unitId
  output$totals <- stratatotals
  output$variance <- NA
  
  return(output)
}