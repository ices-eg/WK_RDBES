#' Remove table prefix from variable names
#'
#' @param x RDBES raw object
#'
#' @return updated RDBES raw object where table prefix has been removed from
#' all variables names except ids
#' @export
#'
#' @examples
#' \dontrun{
#' myH1RawObject <-
#'   createRDBESRawObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#' cleanPrefixFromVarNames(x = myH1RawObject)
#' }
removePrefixFromVarNames <- function(x) {
  for (i in names(x)) {
    print(i)
    if (!is.null(x[[i]])) {
      targetNames <- !grepl(names(x[[i]]), pat = paste0(i, "id"))
      colnames(x[[i]])[targetNames] <- gsub(i, "", names(x[[i]])[targetNames])
    }
  }
  x
}
