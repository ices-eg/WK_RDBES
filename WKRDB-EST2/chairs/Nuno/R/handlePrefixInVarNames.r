#' Add or Remove table prefix from variable names
#'
#' @param x RDBES raw object
#' @type value == "remove" | "add". "remove" removes prefixes (except ids). "add" added table prefixes. 
#'
#' @return updated RDBES raw object where table prefix has been removed
#' from or added to variables names (except ids)
#' @export
#'
#' @examples
#' \dontrun{
#' myH1RawObject <-
#'   createRDBESRawObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#' handlePrefixInVarNames(x = myH1RawObject, type="remove")
#' }
handlePrefixInVarNames <- function(x, type) {
  
    for (i in names(x)) {
    print(i)
    if (!is.null(x[[i]])) {
  if (type=="remove"){
      targetNames <- !grepl(names(x[[i]]), pat = paste0(i, "id"))
      if(length(targetNames)>0) colnames(x[[i]])[targetNames] <- gsub(i, "", names(x[[i]])[targetNames])
    }
  if (type=="add"){
      targetNames <- !grepl(names(x[[i]]), pat = paste0(i, "id")) & !grepl(names(x[[i]]), pat = "[A-Z][A-Z]")
      if(length(targetNames)>0) colnames(x[[i]])[targetNames] <- paste0(i, names(x[[i]])[targetNames])
    }  
  
  }
  }
   
   x
}

