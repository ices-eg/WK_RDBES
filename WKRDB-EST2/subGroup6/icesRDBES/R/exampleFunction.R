#' Minimum Documentation Example
#'
#' See the file *"./tests/testthat/test-exampleFunction.R"*
#' for defining expected input and output
#'
#' @param stringX first string
#' @param stringY second string
#'
#' @return The  stringX and stringY pasted together
#'
#' @examples
#' exampleFunction("A", "B")
exampleFunction <- function(stringX, stringY) {
  returnString <- paste(stringX, stringY)

  returnString
}
