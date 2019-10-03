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
  plotCaa(report)
  return(report)
}
