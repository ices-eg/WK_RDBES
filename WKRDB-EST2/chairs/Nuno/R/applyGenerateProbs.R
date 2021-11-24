#' Generate probabilities missing from RDBES Data
#'
#' Wrapper to generate probabilities. The wrapper calls
#' runChecksOnSelectionAndProbs which main tests need to be passed before
#' probabilities can be calculated. The it calls generateProbs for
#' each sample in each sampling level of the hierarchy.
#'
#' @param x - RDBES raw object
#' @param probType - string. Can be set to "selection" (only selection
#' probabilities are calculated), "inclusion" (only inclusion probabilities are
#' calculated) or "both" (both types of probabilities are calculated)
#' @param overwrite - if TRUE will overwrite probabilties already existing for
#' SRSWR and SRSWOR
#' @param runInitialProbChecks - if TRUE runs runChecksOnSelectionAndProbs
#'
#' @return a list of all the RDBES data tables with probabilites calculated
#'
#' @export
#'
#' @seealso \code{\link{runChecksOnSelectionAndProbs}}
#' \code{\link{generateProbs}}
#'
#' @examples
#' for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'

applyGenerateProbs <- function(x, probType, overwrite,
                               runInitialProbChecks = TRUE) {
  if (runInitialProbChecks) {
    print("========start runChecksOnSelectionAndProbs=======")
    runChecksOnSelectionAndProbs(x)
    print("========end runChecksOnSelectionAndProbs=======")
  }



  print("========start generateProbs=======")

  if (length(unique(x[["DE"]]$hierarchy)) > 1) stop(">1 hierarchy in data:
                                                    not yet developed")
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
      targetTables <- c("OS", "LE", "SS", "SA", "BV")
      parentId <- c("SDid", "OSid", "LEid", "SSid", "SAid")
      # aspects needing development
      if (any(!is.na(x[["SA"]]$parentID))) stop("multiple sub-sampling present
                                                in SA: not yet developed")
      if (!is.null(x[["FM"]]) & nrow(x[["FM"]]) != 0) stop("lower hierarchy A
                                                           and B present: not
                                                           yet developed")
    }
  } else {
    stop(paste0("generateProbs not yet specified for H",
                x[["DE"]]$hierarchy[1]))
  }

  for (i in targetTables) {
    print(i)

    # following code will be worth setting in data.table
    ls1 <- split(x[[i]], x[[i]][[eval(noquote(parentId[targetTables == i]))]])
    ls2 <- lapply(ls1, function(x, ...) {
      # aspects needing development
      if (length(unique(x$stratumName)) > 1 | any(x$stratification == "Y"))
        stop("stratification present: not yet developed")
      if (length(unique(x$clusterName)) > 1 | any(x$clustering == "Y"))
        stop("clustering present: not yet developed")
      print(paste0(parentId[targetTables == i], ": ",
                   x[[parentId[targetTables == i]]][1]))
      x <- generateProbs(x, probType, overwrite)
      x
    })
    x[[i]] <- do.call("rbind", ls2)
  }

  print("========end generateProbs=======")
  lapply(x, function(x) if (!is.null(x)) data.table::data.table(x) else x)
}
