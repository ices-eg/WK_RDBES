#' setRDBESRawObjectDataTypes For a given rdbesRawObject convert the required
#' columns to the correct data types.  (This function can cause an error if we
#' have data in the columns that can't be cast to the desired data type.)
#'
#' @param rdbesRawObject
#'
#' @return An rdbesRawObject with the correct date types for the required
#' fields
#'
#' @examples
#' @import data.table
setRDBESRawObjectDataTypes <- function(rdbesRawObjectToConvert){

  # For each entry in our list convert the columns to the correct format
  # This could cause an error if we have data in the columns that can't be
  # cast to the desired data type
  alteredObject <- lapply(rdbesRawObjectToConvert,function(x){
      # Only process the non-null entries
      if (!is.null(x)){
        # Assume the first field name accurately gives us the table name
        tableName <- substring(names(x)[1], 1, 2)
        # Find information for the relevent columns
        requiredColumns <-
          mapColNamesFieldR[mapColNamesFieldR$Table.Prefix == tableName, ]
        # Change to numeric
        myCols <-
          requiredColumns[requiredColumns$RDataType == "numeric","R.Name"]
        colsToChange <- names(x)[names(x) %in% myCols]
        if (length(colsToChange)>0){
          x[, colsToChange] <-
            x[, lapply(.SD, as.numeric), .SDcols = colsToChange]
        }
        # Change to integer
        myCols <-
          requiredColumns[requiredColumns$RDataType == "integer","R.Name"]
        colsToChange <- names(x)[names(x) %in% myCols]
        if (length(colsToChange)>0){
          x[, colsToChange] <-
            x[, lapply(.SD, as.integer), .SDcols = colsToChange]
        }
        # Change to character
        myCols <-
          requiredColumns[requiredColumns$RDataType == "character","R.Name"]
        colsToChange <- names(x)[names(x) %in% myCols]
        if (length(colsToChange)>0){
          x[, colsToChange] <-
            x[, lapply(.SD, as.character), .SDcols = colsToChange]
        }
      }
      x
    }
  )

  # Update the original object so we don't lose its class type
  for (myTable in names(rdbesRawObjectToConvert)){
    if (!is.null(alteredObject[[myTable]])){
      rdbesRawObjectToConvert[[myTable]] <- alteredObject[[myTable]]
    }

  }

  rdbesRawObjectToConvert


}
