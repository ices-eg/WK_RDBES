#' Title
#'
#' @param objectToCheck
#'
#' @return
#' @export
#'
#' @examples
checkDBEobject <- function(objectToCheck){


  if (any(is.na(objectToCheck))) {stop("objectToCheck was NA - this is not valid")}

  objectToCheck

}
