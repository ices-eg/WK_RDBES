% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createRDBESRawObject.R
\name{createRDBESRawObject}
\alias{createRDBESRawObject}
\title{createRDBESRawObject creates an rdbesRawObject}
\usage{
createRDBESRawObject(
  rdbesExtractPath = NA,
  listOfFileNames = NA,
  castToCorrectDataTypes = TRUE
)
}
\arguments{
\item{rdbesExtractPath}{(Optional) The path to the csv files produced as an
extract by the ICES RDBES.  If no path is suppled then an empty
rdbesRawObject will be returned.}

\item{listOfFileNames}{(Optional) A names list of file names - the list names
shoudl be the two-letter code for the relevent table e.g.
list("DE" = "DE.csv",... ).  If the parameter is not supplied then the
default file names used by the RDBES data download will be used e.g.
"Design.csv" etc.}

\item{castToCorrectDataTypes}{(Optional) If TRUE then the function
will attempt to cast the required columns to the correct data type.  If
FALSE then the column data types will be determined by how the csv files
are read in.  The default is TRUE}
}
\value{
A rdbesRawObject.  If a path to RDBES extract files is provided then
it will contain the data from those files.  If no path is supplied then
an empty rdbesRawObject will be returned.
}
\description{
createRDBESRawObject creates an rdbesRawObject
}
\examples{
myEmptyRDBESObject <- createRDBESRawObject()
}
