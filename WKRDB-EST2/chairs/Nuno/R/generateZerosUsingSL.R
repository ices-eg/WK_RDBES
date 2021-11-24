#' Generate zeros in samples using Species List information
#'
#' @param x RDBES data frame
#'
#' @return RDBES data frame where SA was complemented with species looked for
#' (sensu in sampling objectives) but not registered in sample
#'
#' @export
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'


generateZerosUsingSL <- function(x) {
  if (is.null(x$SA) | is.null(x$SL)) stop("no SA and/or SL")

  tmpSA <- x$SA
  tmpSL <- x$SL

  ls1 <- split(tmpSA, tmpSA$SSid)
  ls2 <- lapply(ls1, function(x) {
    for (sppCode in tmpSL$sppCode) {
      if (sppCode %in% tmpSL$sppCode) { # sppCode is not in list
        if (!sppCode %in% x$speCode) {
          # duplicates SA row
          y <- x[1, ]
          y$speCode <- sppCode
          y$totalWtLive <- 0
          y$sampWtLive <- 0
          y$totalWtMes <- 0
          y$sampWtMes <- 0
          y$SAid <- min(x$SAid) - 0.001 # maintain a count
          y$seqNum <- min(x$seqNum) - 0.001 # maintain a count
          y$unitName <- min(x$SAid) - 0.001 # maintain a count
          y$sex <- NA
          y$lowHierarchy <- "D"
          y$samp <- "N"
          x <- rbind(y, x)
          x
        } else {
          x
        }
      }
    }
    x <- x[order(x$SAid, decreasing = F), ]
    x
  })
  x$SA <- data.table::rbindlist(ls2)
  x
}
