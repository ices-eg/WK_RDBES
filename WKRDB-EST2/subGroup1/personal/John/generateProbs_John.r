generateProbs <- function(x, probType) {

  # generates selection and inclusion probs from RDBES N and n
  # x is RDBES data.frame
  # probType is string. Value == "selection" | "inclusion" , for selection or inclusion probabilities respectively


  a <- unique(x[grepl("selectMeth", names(x)) == T])
  a <- as.character(unique(a[grepl("selectMethCluster", names(a)) == F]))

  if (sum(is.na(a)) > 0) stop("cannot proceed: NAs in SSselectMeth")
  if (length(a) > 1) stop("two different selection methods")

  vecsmallN <- x[grepl("numSamp", names(x)) == T]
  vecsmallN <- vecsmallN[grepl("SampCluster", names(vecsmallN)) == F]
  vecN <- x[grepl("numTotal", names(x)) == T]
  vecN <- vecN[grepl("TotalCluster", names(vecN)) == F]

  if (probType == "selection") {
    vecProb <- a[grepl("selProbUnit", names(a)) == T] # not defined

    print(a)
    if (a %in% c("SRSWR", "SRSWOR")) {
      if (a == "SRSWR") vecProb <- 1 / vecN
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
        if (a == "SRSWR") vecProb <- 1 - (1 - 1 / vecN)^vecsmallN
        if (a == "SRSWOR") vecProb <- vecsmallN / vecN
      }
      if (a %in% c("UPSWR", "UPSWOR")) {
        if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
        vecProb <- vecProb
      }
    }
  }

  vecProb
}

# generate_probs(x=y, probType="inclusion")
