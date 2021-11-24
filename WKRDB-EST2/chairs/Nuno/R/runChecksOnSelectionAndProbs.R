#' Run basic checks on selection methods and probabilities
#'
#' This function runs some basic checks on selection methods and
#' and probabilities of the different sampling tables of a hierarchy.
#' It should be run ahead of generateProbs to secure its correct execution
#' and for that reason it is included in the wrapper applyGenerateProbs.
#'
#' @param x - RDBES raw object
#' @param printStopIssue - If TRUE prints the issue behind the stop
#'
#' @return nothing
#'
#' @export
#'
#' @seealso \code{\link{applyGenerateProbs}}
#' \code{\link{generateProbs}}
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'


runChecksOnSelectionAndProbs <- function(x, printStopIssue = TRUE)  {
  if (length(unique(x[["DE"]]$hierarchy)) > 1) stop(">1 hierarchy in data not
                                                    yet specified")
  if (x[["DE"]]$hierarchy[1] %in% c(1, 7)) {
    if (x[["DE"]]$hierarchy[1] == 1) {
      targetTables <- c("VS", "FT", "FO", "SS", "SA", "BV")
      parentId <- c("SDid", "VSid", "FTid", "FOid", "SSid", "SAid")
      # aspects needing development
      if (any(!is.na(x[["SA"]]$parentID))) stop("multiple sub-sampling present
                                                in SA: not yet developed")
      if (!is.null(x[["FM"]]) & nrow(x[["FM"]]) != 0) stop("lower hierarchy A
                                                           and B present: not
                                                           yet developed")
    }
    if (x[["DE"]]$hierarchy[1] == 7) {
      targetTables <- c("OS", "SS", "SA", "BV")
      parentId <- c("SDid", "OSid", "SSid", "SAid")
      # aspects needing development
      if (any(!is.na(x[["SA"]]$parentID))) stop("multiple sub-sampling present
                                                in SA: not yet developed")
      if (!is.null(x[["FM"]]) & nrow(x[["FM"]]) != 0) stop("lower hierarchy A
                                                           and B present: not
                                                           yet developed")
    }
  } else {
    stop(paste0(
      "generateProbs not yet specified for H",
      x[["DE"]]$hierarchy[1]
    ))
  }

  for (i in targetTables) {
    print(paste("====", i, "====="))
    if (!is.null(x[[i]])) {
      # following code will be worth setting in data.table
      sampIdentifiers <-
        unique(x[[i]][[eval(noquote(parentId[targetTables == i]))]])
      print(paste0(
        length(sampIdentifiers), " unique ",
        noquote(parentId[targetTables == i])
      ))
      ls1 <- split(x[[i]], x[[i]][[eval(noquote(parentId[targetTables == i]))]])
      lapply(ls1, function(x, ...) {
        print(paste0(
          "analysing ", parentId[targetTables == i], ":",
          x[[parentId[targetTables == i]]][1]
        ))

        # stops
        # on not yet developed features
        if (length(unique(x$stratumName)) > 1 | any(x$stratification == "Y")) {
          if (printStopIssue) {
            print(x)
          }
          stop("strata present: not yet specified")
        }
        if (length(unique(x$clusterName)) > 1 | any(x$clustering == "Y")) {
          if (printStopIssue) {
            print(x)
          }
          stop("clusters present: not yet specified")
        }

        # on methods
        nonProbMethods <- c(
          "FIXED", "NPQSRSWOR", "NPCLQS-O", "NPCLQS-T",
          "NPCS", "NPJS", "NPQSRSWOR", "NPQSRSWR",
          "NPQSYSS", "Unknown"
        )
        if (any(x$selectMeth %in% nonProbMethods) == TRUE) {
          if (printStopIssue) {
            print(x)
          }
          stop("NonProbabilistic or Unknown selection methods present. Action
          needed: Overwrite with probabilistic alternative and re-run")
        }
        if (any(paste(x$selectMeth, x$selProb) %in%
          paste(c("UPSWR", "UPSWOR"), NA))) {
          if (printStopIssue) {
            print(x)
          }
          stop("UP methods without selProb present. Action needed: specify
               probs and re-run")
        }
        if (any(paste(x$selectMeth, x$incProb) %in%
          paste(c("UPSWR", "UPSWOR"), NA))) {
          if (printStopIssue) {
            print(x)
          }
          stop("UP methods without incProb present. Action needed: specify
               probs and re-run")
        }

        # on numbers and probs
        if (length(unique(x$numTotal)) > 1) {
          if (printStopIssue) {
            print(x)
          }
          stop("more than 1 numTotal per parentId not allowed when
               stratification == N")
        }
        if (length(unique(x$numSamp)) > 1) {
          if (printStopIssue) {
            print(x)
          }
          stop("more than 1 numSamp per parentId not allowed when
               stratification == N")
        }

        # warnings
        if (length(unique(x$selectMeth)) > 1) print("warning: more than 1
                                                    selection method")
        if (sum(x$selectMeth == "OutOfFrame") > 1) print("warning: outOfFrame
                                                         present")
        if (any(x$selectMeth %in% c("WR")) == TRUE) print("warning: WR selection
                                                          methods present")
        if (any(x$selectMeth %in% c("UPSWR", "UPSWOR")) == TRUE) {
          print("warning:UP selection methods present")
        }
        if (any(is.na(x$numTotal)) == TRUE) print("warning: NAs in numTotal")
        if (any(is.na(x$numSamp)) == TRUE) print("warning: NAs in numSamp")
        if (any(is.na(x$numSamp)) == FALSE & unique(x$numSamp) != nrow(x)) {
          print("warning: numSamp!= nrows")
        }
      })
    }
  }
}
