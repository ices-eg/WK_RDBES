#' Check whether an rdbesRawObject is in a valid format
#'
#' @param objectToCheck
#'
#' @return
#' @export
#'
#' @examples
checkRDBESRawObject <- function(objectToCheck){

  validRDBESRawObject <- T

  ## TODO - add more checks!
  if(length(objectToCheck) == 1){
    if (is.na(objectToCheck)) {
      validRDBESRawObject <- F
    }
  }

  validRDBESRawObject

}
