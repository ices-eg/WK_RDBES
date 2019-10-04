#' Estimate total catch at age for the herring lottery
#' @description Example workflow for estimating from the herring lottery pilot (2018)
#' @details Assumptions made:
#'
#'  - Assumes systematic sampling of typically small fraction of catch as simple random with replacement
#'  - Assumes unequal probability sampling without replacement of typically small fraction of hauls is unequal probability sampling with replacement
#'
#' @noRd
#' @keywords internal
herringlottery_workflow <- function(){
  data <- herringlottery
  proportionsAtAgeBV <- calculateBVProportions(data$BV, "Age", stratified = F)
  meanWeightsBV <- calculateBVmeans(data$BV, "Weight", stratified = F)

  data$SA <- assumeSelectionMethod(data$SA, "SYSS", "SRSWR")
  sampleTotals <- estimateSAcaa(data$SA, data$SS, data$SL, "126417", proportionsAtAgeBV, meanWeightsBV, stratified = F)

  data$FO <- assumeSelectionMethod(data$FO, "UPSWOR", "UPSWR")
  # consider post-stratifying on gear: need post-stratification method, and support for stratification in variance estimator
  haulTotals <- estimateFOCatchAtAge(data$FO, data$SS, data$SA, sampleTotals, stratified=F)

  grandTotals <- estimateTotalHH(data$FO, haulTotals)


  # within FO variance
  # options:
  # - impute resampled
  # - assume zero
  FOvarZero <- assumeFOconstantVar(haulTotals, 0, ages=grandTotals$age)
  # - model assisted


  #grand total variance
  covar <- estimateTotalHHVar(data$FO, grandTotals, haulTotals, FOvarZero)

  report <- makeReportTable(grandTotals, covar)
  return(report)
}

#' @noRd
#' @keywords internal
compute_and_plot_herring_lottery <- function(){
  plotCaa(herringlottery_workflow())
}

#' Compares estimate with that obtained from model based estimation (based on additional samples from other sampling programs)
#' @noRd
#' @keywords internal
#' @import utils
#' @import graphics
compare_with_eca <- function(){
  report <- herringlottery_workflow()
  ecaPath <- system.file("exampledata", "eca_2018.csv", package="h13estimator")
  ecaResults <- read.csv(ecaPath, sep="\t", comment.char = "#") #other reports and plusgroup is commented out in file
  plotCaa(report, main="95% CI vs ECA point (excl. plusgr)")
  points(ecaResults$age, ecaResults$mean, col="red") #mean is actually totals, it refers to mean over Bayesian simulations
  legend("topright", legend = c("Design based", "Model based (ECA)"), fill=c("black", "red"), bty="n")
}
