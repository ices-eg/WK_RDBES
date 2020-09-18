generateClusterProbs <- function(x, probType) {

  # generates selection and inclusion probs from RDBES N and n
  # x is RDBES data.frame
  # probType is string. Value == "selection" | "inclusion" , for selection or inclusion probabilities respectively


  a <- unique(x[grepl("selectMethCluster", names(x)) == T])
  a <- as.character(unique(a[grepl("selectMethCluster", names(a)) == T]))

  if (sum(is.na(a)) > 0) stop("cannot proceed: NAs in SSselectMeth")
  if (length(a) > 1) stop("two different selection methods")

  vecsmallN <- x[grepl("numSampClusters|numSampCluster|clusterName", names(x)) == T]
  vecN <- x[grepl("numTotalClusters|numTotalCluster|clusterName", names(x)) == T]

  if (probType == "selection") {
    vecProb <- a[grepl("selProbUnit", names(a)) == T] # not defined

    print(a)
    if (a %in% c("SRSWR", "SRSWOR")) {
      if (a == "SRSWR") {
        vecProb <- vecN
        vecProb <- 1 / vecN[grepl("numTotalClusters|numTotalCluster", names(vecN)) == T]
      }
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
        if (sum(is.na(vecN)) > 0) stop("cannot proceed: NAs in total")
        if (sum(is.na(vecsmallN)) > 0) stop("cannot proceed: NAs in sampled")
        if (a == "SRSWR") {
          vecProb <- vecsmallN
          vecProb <- 1 - (1 - 1 / vecN[grepl("numTotalClusters|numTotalCluster", names(vecN)) == T])^vecsmallN[grepl("numSampClusters|numSampCluster", names(vecsmallN)) == T]
        }
        if (a == "SRSWOR") {
          vecProb <- vecsmallN
          vecProb <- vecsmallN[grepl("numSampClusters|numSampCluster", names(vecsmallN)) == T] / vecN[grepl("numTotalClusters|numTotalCluster", names(vecN)) == T]
        }
      }
      if (a %in% c("UPSWR", "UPSWOR")) {
        if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
        vecProb <- vecProb
      }
    }
  }
  names(vecProb)[grepl("numTotalClusters|numTotalCluster|numSampClusters|numSampCluster", names(vecProb)) == T] <- "CalcValues"
  vecProb
}

# generate_probs(x=y, probType="inclusion")
