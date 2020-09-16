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

#' Estimation for a single sample
computeDBEresultsTotalPointLowSingleSample <- function(estimationObj, unitId="Uknown"){
  
  if (any(estimationObj$StatisticTable$id != estimationObj$DesignTableTable$id)){
    stop("Malformed estimationObj")
  }
  
  stratas <- unique(estimationObj$DesignTable$stratumname)
  statistic <- names(estimationObj$StatisticTable)[names(estimationObj$StatisticTable)!="id"]
  stratatotals <- 0
  for (s in stratas){
    stratafilter <- estimationObj$DesignTable$stratumname == s
    incProbs <- estimationObj$DesignTable[stratafilter, "inclProb"]
    
    if (length(statistic)>1){
      stratatot <- colSums(estimationObj$StatisticTable[stratafilter,statistic]/incProbs)      
    }
    else{
      stratatot <- sum(estimationObj$StatisticTable[stratafilter,statistic]/incProbs)      
    }

    stratatotals <- stratatotals + stratatot
  }
  
  
  output <- list()
  output$statistic <- statistic
  output$unitId <- unitId
  output$totals <- stratatotals
  output$variance <- NA
  
  return(output)
}

#' extract number at class from raw FM samples for a single sample
#' works for lower hiearchy A and B
extractNumberAtClassSingleSample <- function(FMtable, unitId="Uknown"){
  
  if (length(unique(FMtable$SAid)) != 1){
    stop("Need data for a single sample")
  }
  
  if (length(unique(FMtable$FMtype)) != 1){
    stop("Heterogenous FMtype. Cannot extract number at length")
  }
  
  output <- list()
  output$statistic <- FMtable$FMclass
  output$unitId <- unitId
  output$totals <- FMtable$FMnumAtUnit
  output$variance <- matrix(data = 0, nrow=length(output$total), ncol=length(output$total))
  
  return(output)
}