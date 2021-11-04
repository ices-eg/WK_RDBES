#' filterRDBESRawObject Filter an RDBESRawObject - the returned object will
#' include all rows which either: a) do not included any of the field names in
#' fieldsToFilter, or b) do include the field names and have one of the allowed
#' values in valuesToFilter
#'
#' @param rdbesRawObjectToFilter The RDBESRawObject to filter
#' @param fieldsToFilter A vector of the field names you wish to check
#' @param valuesToFilter A vector of the field values you wish to filter for
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'    createRDBESRawObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#'
#' myFields <- c("SDctry","VDctry","VDflgCtry","FTarvLoc")
#' myValues <- c("ZW","ZWBZH","ZWVFA" )
#'
#' myFilteredObject <- filterRDBESRawObject(myH1RawObject,
#'                                          fieldsToFilter = myFields,
#'                                          valuesToFilter = myValues )
#' }
filterRDBESRawObject <- function(rdbesRawObjectToFilter,
                                 fieldsToFilter,
                                 valuesToFilter){

  alteredObject <- lapply(rdbesRawObjectToFilter, function(x) {
        foundNames <- names(x)[which(names(x) %in% fieldsToFilter)]
        if (length(foundNames)> 0){
          x <-
            dplyr::filter(x,dplyr::if_any(foundNames, ~ .x %in% valuesToFilter))
        }
        x
    }
  )

  # Update the original object so we don't lose its class type
  for (myTable in names(rdbesRawObjectToFilter)){
    if (!is.null(alteredObject[[myTable]])){
      rdbesRawObjectToFilter[[myTable]] <- alteredObject[[myTable]]
    }
  }

  rdbesRawObjectToFilter

}
