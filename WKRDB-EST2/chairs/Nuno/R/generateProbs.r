#' Generate selection and inclusion probabilities
#' (worked from version of function by John Ball)
#'
#' @param x RDBES data frame
#' @param probType  Value == "selection" | "inclusion" | "both" ,
#' for selection and/or inclusion probabilities respectively
#' @param overwrite  Value == TRUE ,
#' for selection and/or inclusion probabilities respectively
#'
#' @return A vector or probabilities
#' @export
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'


generateProbs <- function(x, probType, overwrite) {
  x <- data.frame(x)

  a <- x[grepl("selectMeth", names(x)) == T]
  a <- as.character(a[, grepl("selectMethCluster", names(a)) == F])

  if (sum(is.na(a)) > 0) stop("cannot proceed: NAs in SSselectMeth")

  if (length(unique(a)) > 1) {
    print("warning: two different selection methods")
  }

  # set in line with guideline RDBES issue #71



  vecSmallN <- x[grepl("numSamp", names(x)) == T]
  vecSmallN <- vecSmallN[, grepl("SampCluster", names(vecSmallN)) == F]
  vecBigN <- x[grepl("numTotal", names(x)) == T]
  vecBigN <- vecBigN[, grepl("TotalCluster", names(vecBigN)) == F]

  if (overwrite == T) { # can only overwrite if there is info on vecBigN
    if (sum(is.na(vecBigN)) > 0) stop("cannot proceed: NAs in numTotal")
    if (sum(is.na(vecSmallN)) > 0) {
      if (!x$recType[1] == "BV") {
        print("missing numSamp: generating numSamp from row number")
        x$numSamp <- vecSmallN <- rep(length(vecSmallN), length(vecSmallN))
      } else {
        print("missing numSamp: generating numSamp from number of
              unique fishId")
        x$numSamp <- vecSmallN <- rep(length(unique(x$fishId)),
                                      length(vecSmallN))
      }
    }
  }

  if (probType %in% c("selection", "both")) {
    vecProb <- x[grepl("selProb", names(x)) == T] # not defined
    if (!x$recType[1] %in% c("SA")) vecProb <-
        vecProb[, grepl("selProbCluster", names(vecProb)) == F]

    if (a[1] %in% c("SRSWR", "SRSWOR")) {
      if (overwrite == T) {
        if (a[1] == "SRSWR") vecProb <- 1 / vecBigN
        if (a[1] == "SRSWOR") {
          print("warning: prob selection depends on order...assuming
                samples taken sequentially in order of rows")
          vecProb <- 1 / (vecBigN[1]:(vecBigN[1] - vecSmallN[1] + 1))
        }
      }
    }
    if (a[1] %in% c("UPSWR", "UPSWOR")) {
      if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
      vecProb <- vecProb
      if (overwrite == T) {
        stop("cannot overwrite probSelection in UP designs")
      }
    }
    x$selProb <- vecProb
  }


  if (probType %in% c("inclusion", "both")) {
    vecProb <- x[grepl("incProb", names(x)) == T]
    if (x$recType[1] != "SA") vecProb <-
        vecProb[, grepl("incProbCluster", names(vecProb)) == F]
    if (a[1] %in% c("SRSWR", "SRSWOR", "CENSUS")) {
      if (overwrite == T) {
        if (a[1] == "SRSWR") vecProb <- 1 - (1 - 1 / vecBigN)^vecSmallN
        if (a[1] == "SRSWOR") vecProb <- vecSmallN / vecBigN
        if (a[1] == "CENSUS") vecProb <- rep(1, length(vecBigN))
      }
    }
    if (a[1] %in% c("UPSWR", "UPSWOR")) {
      if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
      vecProb <- vecProb
      if (overwrite == T) {
        stop("cannot overwrite probInclusion in UP designs")
      }
    }
    x$incProb <- vecProb
  }
  x
}
