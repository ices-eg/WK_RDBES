#' createRDBESRawObject creates an rdbesRawObject
#'
#' @param rdbesExtractPath (Optional) The path to the csv files produced as an
#' extract by the ICES RDBES.  If no path is suppled then an empty
#' rdbesRawObject will be returned.
#'
#' @return A rdbesRawObject.  If a path to RDBES extract files is provided then
#' it will contain the data from those files.  If no path is supplied then
#' ' an empty rdbesRawObject will be returned.
#' @export
#'
#' @examples
#' myEmptyRDBESObject <- createRDBESRawObject()
createRDBESRawObject <- function(rdbesExtractPath = NA){


  # For testing
  #rdbesExtractPath <- "H:\\git\\WK_RDBES\\WKRDB-EST2\\subGroup6\\icesRDBES\\tests\\testthat\\h1_v_1_19"
  #load("H:\\git\\WK_RDBES\\WKRDB-EST2\\subGroup6\\icesRDBES\\data\\mapColNamesFieldR.RData")

  # A named list with the file names that are produced by the RDBES download
  fileNames <- list("DE" = "Design"
                    ,"SD"="SamplingDetails"
                    ,"VS"="VesselSelection"
                    ,"FT"="FishingTrip"
                    ,"FO"="FishingOperation"
                    ,"TE"="TemporalEvent"
                    ,"LO"="Location"
                    ,"OS"="OnshoreEvent"
                    ,"LE"="LandingEvent"
                    ,"SS"="SpeciesSelection"
                    ,"SA"="Sample"
                    ,"FM"="FrequencyMeasure"
                    ,"BV"="BiologicalVariable"
                    ,"VD"="VesselDetails"
                    ,"SL"="SpeciesList"
                    ,"CL"="CommercialLanding"
                    ,"CE"="CommercialEffort")

  # Create a named list using the short names - set all values to NULL
  myRDBESRawObject <- setNames(as.list(replicate(length(fileNames),NULL)),
                               names(fileNames))

  # If we have been supplied with a path to files we will try and read them
  # otherwise we'll just return an empty rdbesRawBoject
  if (!is.na(rdbesExtractPath)){

    # Determine the files which actually exist
    filesWhichExist <- names(
      fileNames[file.exists(
        paste(rdbesExtractPath,"\\",fileNames,".csv",sep="")
        )]
      )

    # Read the files which exist
    for (myFile in filesWhichExist){
      #myFile <- "SA"
      # Read the file
      myRDBESRawObject[[myFile]] <-
        read.csv(paste(rdbesExtractPath,"\\",fileNames[myFile],".csv",sep=""),
                 header=TRUE,sep=",",stringsAsFactors=FALSE)

      # Change each entry to a data table
      myRDBESRawObject[[myFile]] <-
        data.table::setDT(myRDBESRawObject[[myFile]])

      # Change database field names to R names where we can
      myNames <- mapColNamesFieldR[mapColNamesFieldR$Table.Prefix == myFile,]
      myNameMatches <- match(
              trimws(tolower(names(myRDBESRawObject[[myFile]]))),
                             trimws(tolower(myNames$Field.Name))
                        )
      myNameMatchesNotNA <- myNameMatches[!is.na(myNameMatches)]
      names(myRDBESRawObject[[myFile]])[!is.na(myNameMatches)] <-
        myNames[myNameMatchesNotNA,"R.Name"]

    }

  }

  ## DATA FIX - spelling mistake in 1 download file format ...
  if ("CLincidentialByCatchMitigationDevice" %in% names(myRDBESRawObject[['CL']])){
      data.table::setnames(myRDBESRawObject[['CL']],"CLincidentialByCatchMitigationDevice","CLIBmitiDev")
  }

  # Return the object
  myRDBESRawObject

}