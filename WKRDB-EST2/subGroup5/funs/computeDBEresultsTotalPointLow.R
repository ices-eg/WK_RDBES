#
# Drafts for function signatures. Dont implement yet
#


#' Estimation result
#' 
#' @details results from a set of sampling units, list of \code{\link{sampleUnitResult}} objects:
#' 
#' @rname DBEresultsTotalPointLow
#' 
"DBEresultsTotalPointLow"

#' Estimation result
#'
#' @details 
#'  Result for a single samping unit, list with members:
#'  \describe{
#'    \item{statistic}{character: description of statistic}
#'    \item{unitId}{list: ids of unit the estimate is made for, e.g. SAid}
#'    \item{totals}{list: estimate of total for statistic}
#'    \item{variance}{list: estimate of variance for statistic}
#'  }
#'
#' @rname sampleUnitResult
"sampleUnitResult"

#' @param estimationObj \code{\link{DBEestimObjLow}}
#' @return \code{\link{DBEresultsTotalPointLow}} with point estimate
computeDBEresultsTotalPointLow <- function(estimationObj){
  stop("Not Implemented")
  # for each sammple
  #  extract sampUnitData from estimationObj
  #  pass to computeDBEresultsTotalPointLowSingleSample
  # collate results in DBEresultsTotalPointLow
}

#' Estimation for a single sample
#' @param estimationObj \code{\link{sampUnitData}}
#' @return \code{\link{sampleUnitResult}}
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

#' Estimation for multiple samples
#' @param estimationObj \code{\link{sampUnitData}}
#' @return \code{\link{sampleUnitResult}}
computeDBEresultsTotalPointLowMultSample <- function(estimationObj, unitId="Uknown"){
  
  # Test
  estimationObj <- estimationObjectCountAtAge
  
  if(any(sapply(1:length(estimationObj), function(x){
    all.equal(estimationObj[[x]][["StatisticTable"]][["id"]], estimationObj[[x]][["DesignTable"]][["id"]])
  })) %in% FALSE){
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
#' @return \code{\link{sampleUnitResult}}
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

#' Extract number at class for a set of samples
extractNumberAtClass <- function(){
  stop("Not implmented")
  
  # potential issue for further estimation.
  
  # if I extract the number at class using extractNumberAtClassSingleSample, 
  # I get the information encoded in FMclass copied over to sampleUnitResult$statistic
  # If I want to aggregate these furter I need to know that the FMclass variables mean the same thing
  # If only classes sampled are recorded, and two samples have been sampled with coinciding lower limits
  
  # use FMaccuracy ... consider if the generic "statistic" is sufficient
  
}
