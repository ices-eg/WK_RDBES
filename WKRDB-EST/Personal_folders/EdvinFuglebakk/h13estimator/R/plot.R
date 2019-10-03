#' Compiles report of estimate
#' @description Compiles a report of estimate with estimat, SE and RSE for each age group
#' @param grandTotals data.table estimates of totals, format described in \code{\link[h13estimator]{estimateTotalHH}}
#' @param totalCovar matrix Variance Covariance estimate for totals, format described in \code{\link[h13estimator]{estimateTotalHHVar}}
#' @return data.table with columns
#'  \item{age}{age estimates are provided for}
#'  \item{numberAtAge}{Esitimate of catch at age in numbers}
#'  \item{numberAtAgeSE}{Standard error estimates}
#'  \item{numberAtAgeRSE}{Relative standard error estimates}
#' @export
makeReportTable <- function(grandTotals, totalCoVar){

  grandTotals$numberAtAgeSE <- sqrt(diag(totalCoVar))
  grandTotals$numberAtAgeRSE <- grandTotals$numberAtAgeSE / grandTotals$numberAtAge

  # order ages
  grandTotals$age <- as.numeric(grandTotals$age)
  setorder(grandTotals, age)
  grandTotals$age <- as.integer(grandTotals$age)

  return(grandTotals)

}

#' Plot Catch at age in numbers
#' @param caaReprot estimates of catch at age, format described in \code{\link[h13estimator]{makeReportTable}}
#' @param main plot tile
#' @export
plotCaa <- function(caaReport, main="95% CI"){
  nage <- caaReport$numberAtAge/(1000*1000)
  sd <- caaReport$numberAtAgeSE/(1000*1000)
  CI95 <- 1.96*sd
  plot(caaReport$age, nage, xlab="Age", ylab="CAA (millions)", ylim=c(0,max(nage+CI95)), main=main)
  segments(caaReport$age, nage-CI95 , caaReport$age, nage+CI95)
}
