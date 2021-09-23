#' Check whether an rdbesRawObject is in a valid format
#'
#' @param objectToCheck
#'
#' @return T if object is valid, F is object is not valid
#' @export
#'
#' @examples
checkRDBESRawObject <- function(objectToCheck){

  validRDBESRawObject <- T
  warningText <- NA

  allowedNamesInList <- unique(mapColNamesFieldR$Table.Prefix)

  ## TODO - add more checks!

  # CHECK 1 Have we just been passed NA?
  if(length(objectToCheck) == 1){
    if (is.na(objectToCheck)) {
      validRDBESRawObject <- F
      warningText <- "objectToCheck is NA"
    }
  # CHECK 2 Is this a list?  It should be
  } else if (!is.list(objectToCheck)){
    validRDBESRawObject <- F
    warningText <- "objectToCheck is not a list"
  # CHECK 3 Does this list have any names that aren't allowed?
  } else if(!all(names(objectToCheck) %in% allowedNamesInList)){
    validRDBESRawObject <- F
    warningText <- paste("objectToCheck is a list but has extra names",
                  paste(names(objectToCheck),collapse=","),
                  sep = "")
  # CHECK 4 Does the list have an entry for all the required names?
  } else if(!all(allowedNamesInList %in% names(objectToCheck))){
    validRDBESRawObject <- F
    print(paste(names(objectToCheck),collapse=","))
    warningText <- paste("objectToCheck is a list but does not contain ",
                         "all the required names",
                         paste(names(objectToCheck),collapse=","),
                   sep = "")
  } else {

    # Get any objectToCheck entries which aren't null or data tables
    badEntries <- objectToCheck[!
                    sapply(objectToCheck,
                      function(x){
                        returnValue = F
                        if(length(x)==0 & is.null(x)){
                          returnValue = T
                        }
                        else if ("data.table" %in% class(x)) {
                          returnValue = T
                        }
                        returnValue
                        })
                  ]
    # CHECK 5 Are there any entries which aren't NULL or data tables?
    if (length(badEntries)>0){
      validRDBESRawObject <- F
      warningText <-
      paste("objectToCheck is a list but contains some entries which are ",
            "not NULL or data tables",
                     paste(names(badEntries),collapse=","),
                     sep = "")
    } else {

      # Just check non-NULL entries
      nonNullEntries <- objectToCheck[sapply(objectToCheck,Negate(is.null))]

      #For each non-null entry see if there have the required field names
      badEntries <- nonNullEntries[!
        sapply(nonNullEntries, function(x){
          returnValue = F
          # Assume the first field name accurately gives us the table name
          tableName <-  substring(names(x)[1],1,2)
          requiredColumnNames <-
            mapColNamesFieldR[mapColNamesFieldR$Table.Prefix == tableName,]
          requiredColumnNames[is.na(requiredColumnNames$R.Name),'R.Name'] <-
            requiredColumnNames[is.na(requiredColumnNames$R.Name),'Field.Name']
          requiredColumnNames <- requiredColumnNames$R.Name
          # Are all the required names present?
          if (all(requiredColumnNames %in% names(x))){
            returnValue = T
          }
          returnValue})
        ]

      # CHECK 6 Check if there are any entries which have invalid field names
      if (length(badEntries)>0){
        validRDBESRawObject <- F
        warningText <-
          paste("objectToCheck contains the following tables which don't ",
                "contain all required fields: ",
                paste(names(badEntries),collapse=","),
                sep = "")
      }
    }
  }

  # Print out any information if we need to
  if (!is.na(warningText)) {print(warningText)}

  # Return the validation result
  validRDBESRawObject

}
