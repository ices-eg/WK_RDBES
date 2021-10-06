#' createRDBESRawObject creates an rdbesRawObject
#'
#' @param rdbesExtractPath (Optional) The path to the csv files produced as an
#' extract by the ICES RDBES.  If no path is suppled then an empty
#' rdbesRawObject will be returned.
#' @param listOfFileNames (Optional) A names list of file names - the list names
#' shoudl be the two-letter code for the relevent table e.g.
#' list("DE" = "DE.csv",... ).  If the parameter is not supplied then the
#' default file names used by the RDBES data download will be used e.g.
#' "Design.csv" etc.
#'
#' @return A rdbesRawObject.  If a path to RDBES extract files is provided then
#' it will contain the data from those files.  If no path is supplied then
#' ' an empty rdbesRawObject will be returned.
#' @export
#'
#' @examples
#' myEmptyRDBESObject <- createRDBESRawObject()
createRDBESRawObject <- function(rdbesExtractPath = NA, listOfFileNames = NA) {


  # For testing - to be removed!
  # rdbesExtractPath <- "H:\\git\\WK_RDBES\\WKRDB-EST2\\subGroup6\\icesRDBES\\tests\\testthat\\h1_v_1_19"
  # load("H:\\git\\WK_RDBES\\WKRDB-EST2\\subGroup6\\icesRDBES\\data\\mapColNamesFieldR.RData")

  # If we have not been passed a list of file names use a default
  if (length(listOfFileNames) == 1 && is.na(listOfFileNames)){

    # A named list with the file names that are produced by the RDBES download
    fileNames <- list(
      "DE" = "Design",
      "SD" = "SamplingDetails",
      "VS" = "VesselSelection",
      "FT" = "FishingTrip",
      "FO" = "FishingOperation",
      "TE" = "TemporalEvent",
      "LO" = "Location",
      "OS" = "OnshoreEvent",
      "LE" = "LandingEvent",
      "SS" = "SpeciesSelection",
      "SA" = "Sample",
      "FM" = "FrequencyMeasure",
      "BV" = "BiologicalVariable",
      "VD" = "VesselDetails",
      "SL" = "SpeciesList",
      "CL" = "CommercialLanding",
      "CE" = "CommercialEffort"
    )

    # Stick ".csv" on to each default name
    fileNames <-lapply(fileNames, function(x){paste(x,".csv",sep="")})

  } else {
    fileNames <- listOfFileNames
  }

  # Create a named list using the short names - set all values to NULL
  myRDBESRawObject <- stats::setNames(
    as.list(replicate(length(fileNames), NULL)),
    names(fileNames)
  )

  # If we have been supplied with a path to files we will try and read them
  # otherwise we'll just return an empty rdbesRawBoject
  if (!is.na(rdbesExtractPath)) {

    # Determine the files which actually exist
    filesWhichExist <- names(
      fileNames[file.exists(
        paste(rdbesExtractPath, "\\", fileNames, sep = "")
      )]
    )

    # If we don't find relevent files in the dir give a warning
    if (length(filesWhichExist) == 0) {
      warning(paste0("No relevent files found in given directory",
      " - an empty object will be created"))
    } else {

      # Read the files which exist
      for (myFile in filesWhichExist) {
        # Read the file
        myRDBESRawObject[[myFile]] <-
          utils::read.csv(
            paste(rdbesExtractPath, "\\", fileNames[myFile],  sep = ""),
            header = TRUE, sep = ",", stringsAsFactors = FALSE
          )

        # Change each entry to a data table
        myRDBESRawObject[[myFile]] <-
          data.table::setDT(myRDBESRawObject[[myFile]])

        # Change database field names to R names where we can
        myNames <- mapColNamesFieldR[mapColNamesFieldR$Table.Prefix == myFile, ]
        myNameMatches <- match(
          trimws(tolower(names(myRDBESRawObject[[myFile]]))),
          trimws(tolower(myNames$Field.Name))
        )
        myNameMatchesNotNA <- myNameMatches[!is.na(myNameMatches)]
        names(myRDBESRawObject[[myFile]])[!is.na(myNameMatches)] <-
          myNames[myNameMatchesNotNA, "R.Name"]
      }
    }

    ## DATA FIX - spelling mistake in 1 download file format ...
    if ("CLincidentialByCatchMitigationDevice" %in%
        names(myRDBESRawObject[["CL"]])) {
      data.table::setnames(
        myRDBESRawObject[["CL"]]
        , "CLincidentialByCatchMitigationDevice"
        , "CLIBmitiDev")
    }

  }

  # Return the object
  myRDBESRawObject
}
