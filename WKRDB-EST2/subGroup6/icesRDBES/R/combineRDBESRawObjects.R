#' combineRDBESRawObjects combines 2 rdbesRawObjects into a single
#' rdbesRawObject
#'
#' @param rdbesRawObject1 The first object to combine
#' @param rdbesRawObject2 The second object to combine
#'
#' @return
#' @export
#'
#' @examples
combineRDBESRawObjects <- function(rdbesRawObject1, rdbesRawObject2) {
  if (!validateRDBESRawObject(rdbesRawObject1, verbose = FALSE) |
      !validateRDBESRawObject(rdbesRawObject2, verbose = FALSE)) {
    stop(paste0(
      "At least one of the rdbesRawObjects is not valid ",
      "- mergeRDBESRawObjects will not proceed"
    ))
  }

  # Create an empty rdbesRawObject as the basis of what we will return
  myRDBESRawObject <- createRDBESRawObject()

  # For each entry, bind the data tables together
  for (myTable in names(myRDBESRawObject)) {

    # Only bind the data tables if one of them is not null
    if (!(is.null(rdbesRawObject1[[myTable]]) &
      is.null(rdbesRawObject2[[myTable]]))) {
      myRDBESRawObject[[myTable]] <-
        data.table::rbindlist(list(
          rdbesRawObject1[[myTable]],
          rdbesRawObject2[[myTable]]
        ),
        use.names = T, fill = T
        )
    }
  }

  myRDBESRawObject
}
