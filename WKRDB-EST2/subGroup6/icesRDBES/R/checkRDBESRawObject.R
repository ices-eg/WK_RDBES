#' Check whether an rdbesRawObject is in a valid format
#'
#' @param objectToCheck rdbesRawObject i.e. a list of data.tables
#' @param checkDataTypes (Optional) Set to TRUE if you want to check that
#' the data types of the required columns are correct, or FALSE if you don't care.
#' Default value is FALSE.
#'
#' @return TRUE if object is valid, FALSE is object is not valid
#' @export
#'
checkRDBESRawObject <- function(objectToCheck,
                                checkDataTypes = FALSE) {
  validRDBESRawObject <- TRUE
  warningText <- NA

  allowedNamesInList <- unique(mapColNamesFieldR$Table.Prefix)

  # CHECK 1 Have we just been passed NA?
  if (length(objectToCheck) == 1 && (is.null(nrow(objectToCheck)) ||
    nrow(objectToCheck) == 1)) {
    if (is.na(objectToCheck)) {
      validRDBESRawObject <- FALSE
      warningText <- "objectToCheck is NA"
    }
    # CHECK 2 Is this a list?  It should be!
  } else if (!(is.list(objectToCheck) & inherits(objectToCheck, "list"))) {
    validRDBESRawObject <- FALSE
    warningText <- "objectToCheck is not a list"
    # CHECK 3 Does this list have any names that aren't allowed?
  } else if (!all(names(objectToCheck) %in% allowedNamesInList)) {
    validRDBESRawObject <- FALSE
    warningText <- paste("objectToCheck is a list but has extra names ",
      paste(names(objectToCheck), collapse = ","),
      sep = ""
    )
    # CHECK 4 Does the list have an entry for all the required names?
  } else if (!all(allowedNamesInList %in% names(objectToCheck))) {
    validRDBESRawObject <- FALSE
    print(paste(names(objectToCheck), collapse = ","))
    warningText <- paste("objectToCheck is a list but does not contain ",
      "all the required names",
      paste(names(objectToCheck), collapse = ","),
      sep = ""
    )
  } else { #1

    # Get any objectToCheck entries which aren't null or data tables
    badEntries <- objectToCheck[!
    sapply(
      objectToCheck,
      function(x) {
        returnValue <- FALSE
        if (length(x) == 0 & is.null(x)) {
          returnValue <- TRUE
        } else if ("data.table" %in% class(x)) {
          returnValue <- TRUE
        }
        returnValue
      }
    )]
    # CHECK 5 Are there any entries which aren't NULL or data tables?
    if (length(badEntries) > 0) {
      validRDBESRawObject <- FALSE
      warningText <-
        paste("objectToCheck is a list but contains some entries which are ",
          "not NULL or data tables",
          paste(names(badEntries), collapse = ","),
          sep = ""
        )
    } else { #2

      # Just check non-NULL entries
      nonNullEntries <- objectToCheck[sapply(objectToCheck, Negate(is.null))]

      # The next checks are only relevent if we don't have an empty object
      if (length(nonNullEntries) > 0) { #3

        # Call a function to check whether the required field names
        # are present and that there aren't duplicates
        myReturnValue <- checkRDBESRawObjectContent(nonNullEntries)
        warningText <- myReturnValue[["warningText"]]
        validRDBESRawObject <- myReturnValue[["validRDBESRawObject"]]

        # If we also want to check the data types of the columns
        # then go ahead and call the function to do that
        if (checkDataTypes){
          myDiffs <- checkRDBESRawObjectDataTypes(nonNullEntries)
          numberOfDifferences <- nrow(myDiffs)
          if (numberOfDifferences >0 ){
            validRDBESRawObject <- FALSE
            if(is.na(warningText)){
              warningText <- ""
            } else {
              warningText <- paste0(warningText,". ")
            }
            warningText <- paste0(warningText,
              "objectToCheck has the following fields ",
              "with incorrect data types: ",
              paste(myDiffs[,"R.Name"], collapse = ","),
              sep = ""
              )
          }
        }

      } #3
    } #2
  } #1



  # Print out any information if we need to
  if (!is.na(warningText)) {
    print(warningText)
  }

  # Return the validation result
  validRDBESRawObject
}
