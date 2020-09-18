#' Generate vector of selection or inclusion probabilities
#' (version of fuction by John Ball)
#'
#' @param x RDBES data frame
#' @param probType  Value == "selection" | "inclusion" ,
# for selection or inclusion probabilities respectively
#'
#' @return A vector or probabilities
#' @export
#'
#' @examples
#' \dontrun{
#' generateProbs(x=y, probType="inclusion")
#' }
generateProbs <- function(x, probType) {


  a <- unique(x[grepl("selectMeth", names(x)) == T])
  a <- as.character(unique(a[grepl("selectMethCluster", names(a)) == F]))

  if (sum(is.na(a)) > 0) stop("cannot proceed: NAs in SSselectMeth")
  if (length(a) > 1) stop("two different selection methods")

  vecSmallN <- x[grepl("numSamp", names(x)) == T]
  vecSmallN <- vecSmallN[grepl("SampCluster", names(vecSmallN)) == F]
  vecBigN <- x[grepl("numTotal", names(x)) == T]
  vecBigN <- vecBigN[grepl("TotalCluster", names(vecBigN)) == F]

  if (probType == "selection") {
    vecProb <- a[grepl("selProbUnit", names(a)) == T] # not defined

    print(a)
    if (a %in% c("SRSWR", "SRSWOR")) {
      if (a == "SRSWR") vecProb <- 1 / vecBigN
      if (a == "SRSWOR") stop("depends on order")
    }
    if (a %in% c("UPSWR", "UPSWOR")) {
      if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
      vecProb <- vecProb
    }
  }


  if (probType == "inclusion") {
    vecProb <- a[grepl("incProbUnit", names(a)) == T]

    if (length(a) > 1) {
      stop("two different selection methods")
    } else {
      print(a)
      if (a %in% c("SRSWR", "SRSWOR")) {
        if (sum(is.na(vecBigN)) > 0) stop("cannot proceed: NAs in total")
        if (sum(is.na(vecSmallN)) > 0) stop("cannot proceed: NAs in sampled")
        if (a == "SRSWR") vecProb <- 1 - (1 - 1 / vecBigN)^vecSmallN
        if (a == "SRSWOR") vecProb <- vecSmallN / vecBigN
      }
      if (a %in% c("UPSWR", "UPSWOR")) {
        if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
        vecProb <- vecProb
      }
    }
  }

  vecProb
}
