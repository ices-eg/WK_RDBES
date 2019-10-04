#' Compiles report of estimate
#' @description Compiles a report of estimate with estimat, SE and RSE for each age group
#' @param grandTotals data.table estimates of totals, format described in \code{\link[h13estimator]{estimateTotalHH}}
#' @param totalCoVar matrix Variance Covariance estimate for totals, format described in \code{\link[h13estimator]{estimateTotalHHVar}}
#' @return data.table with columns
#'  \item{age}{age estimates are provided for}
#'  \item{numberAtAge}{Esitimate of catch at age in numbers}
#'  \item{numberAtAgeSE}{Standard error estimates}
#'  \item{numberAtAgeRSE}{Relative standard error estimates}
#' @export
makeReportTable <- function(grandTotals, totalCoVar){
  requireNamespace("ggplot2")
  grandTotals$numberAtAgeSE <- sqrt(diag(totalCoVar))
  grandTotals$numberAtAgeRSE <- grandTotals$numberAtAgeSE / grandTotals$numberAtAge

  # order ages
  grandTotals$age <- as.numeric(grandTotals$age)
  setorder(grandTotals, age)
  grandTotals$age <- as.integer(grandTotals$age)

  return(grandTotals)

}

#' Format Variance-Covariance matrix
#' @description Format Variance-Covariance matrix with natural ordering of ages
#' @param coVar matrix Variance Covariance estimate for totals, format described in \code{\link[h13estimator]{estimateTotalHHVar}}
#' @param correlations logical() whether to standardize matrix
#' @param removeZeroes logical() whether to remove zero rows and columns
#' @return matrix with ages along both dimensions
#' @export
formatVarCovar <- function(coVar, correlations=F, removeZeroes=F){

  roworder <- as.character(sort(as.integer(rownames(coVar))))
  colorder <- as.character(sort(as.integer(colnames(coVar))))

  coVar <- coVar[roworder, colorder]

  if (removeZeroes){
    coVar <- coVar[rowSums(coVar)!=0, colSums(coVar)!=0]
  }

  diag(diag(coVar) %*% coVar %*% diag(diag(coVar)))

  if (correlations){
    coVar <- cov2cor(coVar)
  }

  return(coVar)

}

#' Plot Catch at age in numbers
#' @param caaReport estimates of catch at age, format described in \code{\link[h13estimator]{makeReportTable}}
#' @param main plot tile
#' @export
#' @import graphics
plotCaa <- function(caaReport, main="95% CI"){
  nage <- caaReport$numberAtAge/(1000*1000)
  sd <- caaReport$numberAtAgeSE/(1000*1000)
  CI95 <- 1.96*sd
  plot(caaReport$age, nage, xlab="Age", ylab="CAA (millions)", ylim=c(0,max(nage+CI95)), main=main)
  segments(caaReport$age, nage-CI95 , caaReport$age, nage+CI95)
}

#' Plot correlation matrix
#' @description Produces image plot of matrix
#' @param matrix matrix to plot
#' @param main title for plot
#' @export
plotCorrelationMatrix <- function(matrix, main="correlations"){
  melted_matrix <- melt(matrix)
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
    scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "#f7f7f7",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="value") +
    theme_minimal()+ # minimal theme
    xlab("") +
    ylab("") +
    ggtitle(main)
}
