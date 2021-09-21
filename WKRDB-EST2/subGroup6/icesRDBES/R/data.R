#' The tables required for each RDBES hierarchy.
#'
#' A list containing the tables required for each RDBES hierachy
#'
#' @format A named list containing the tables required for each RDBES hierachy.
#' Each entry in the list is a character vecor
#' \describe{
#'   \item{tablesInRDBESHierarchies}{named list of character vectors}
#'   ...
#' }
#' @source \url{https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles}
"tablesInRDBESHierarchies"
#' A dataset containing test RDBES data for H1 in the DBErawObj structure
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data frame}
#'   \item{SD}{the Sampling Details data frame}
#'   \item{VS}{the Vessel Selection data frame}
#'   \item{FT}{the Fishing Trip data frame}
#'   \item{FO}{the Fishing Operation data frame}
#'   \item{SS}{the Species Selection data frame}
#'   \item{SA}{the Sample data frame}
#'   \item{FM}{the Frequency Measure data frame}
#'   \item{BV}{the Biological Variable data frame}
#'   \item{VD}{the Vessel Details data frame}
#'   \item{SL}{the Species List data frame}
#'   ...
#' }
#' @source \url{https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles}
"h1DBErawObj"
#' A dataset containing test RDBES data for H1
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data frame}
#'   \item{SD}{the Sampling Details data frame}
#'   \item{VS}{the Vessel Selection data frame}
#'   \item{FT}{the Fishing Trip data frame}
#'   \item{FO}{the Fishing Operation data frame}
#'   \item{SS}{the Species Selection data frame}
#'   \item{SA}{the Sample data frame}
#'   \item{FM}{the Frequency Measure data frame}
#'   \item{BV}{the Biological Variable data frame}
#'   \item{VD}{the Vessel Details data frame}
#'   \item{SL}{the Species List data frame}
#'   ...
#' }
#' @source \url{https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles}
"h1Example"
#' A dataset containing the mapping from database column names
#' to R field names
#'
#' @format A data frame containing database field names and their equivalent
#' R field name:
#' \describe{
#'   \item{Field.Name}{The database field names}
#'   \item{R.Name}{The equivalent R field name}
#'   ...
#' }
#' @source \url{https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles}
"mapColNamesFieldR"
