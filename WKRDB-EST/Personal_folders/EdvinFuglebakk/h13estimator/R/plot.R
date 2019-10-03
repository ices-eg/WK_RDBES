#' Plot Catch at age in numbers
#' @param caaReprot estimates of catch at age, format described in \code{\link[h13estimator]{makeReportTable}}
#' @param main plot tile
plotCaa <- function(caaReport, main="95% CI"){
  nage <- caaReport$numberAtAge/(1000*1000)
  sd <- caaReport$numberAtAgeSE/(1000*1000)
  CI95 <- 1.96*sd
  plot(caaReport$age, nage, xlab="Age", ylab="CAA (millions)", ylim=c(0,max(nage+CI95)), main=main)
  segments(caaReport$age, nage-CI95 , caaReport$age, nage+CI95)
}
