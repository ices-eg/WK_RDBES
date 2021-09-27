# Use the RDBES xsd files to determine which tables are
# required in the different RDBES hierachies

# Set to TRUE if you want to download the
# lastest xsd files from GitHub
downloadFromGitHub <- TRUE
gitHubFileLocation <-
  "https://api.github.com/repos/ices-tools-dev/RDBES/contents/XSD-files"
# The folder to read the files from.  If you are
# downloading from GitHub a copy of the latest files will be saved here
fileLocation <- "data-raw/"

# STEP 1) Get the BaseTypes file (if required)
if (downloadFromGitHub) {
  myHierarchyFiles <- NULL
  myResponse <- httr::GET(gitHubFileLocation)
  filesOnGitHub <- httr::content(myResponse)

  for (myFile in filesOnGitHub) {
    myGitHubFile <- data.frame(fileName = myFile$name
                               , downloadURL = myFile$download_url)
    if (is.null(myHierarchyFiles)) {
      myHierarchyFiles <- myGitHubFile
    } else {
      myHierarchyFiles <- rbind(myHierarchyFiles, myGitHubFile)
    }
  }
  # Sub-set to the files we are interested in
  myHierarchyFiles <- myHierarchyFiles[grepl("^H.*xsd$"
                                             , myHierarchyFiles$fileName), ]

  print(paste("Downloading ", nrow(myHierarchyFiles)
              , " files from GitHub", sep = ""))

  # Download our files
  for (i in 1:seq_len(myHierarchyFiles)) {
    anHierarchyFile <- RCurl::getURL(myHierarchyFiles[i, "downloadURL"])
    # save the file locally
    writeLines(anHierarchyFile
               , paste(fileLocation,
                       myHierarchyFiles[i, "fileName"], sep = "")
    )
  }
}

# Read all the H.*xsd files
filesToRead <- list.files(path = fileLocation
                          , pattern = "^H.*xsd$"
                          , recursive = FALSE
                          , full.names = FALSE)


myHierarchyTables <- list()
for (fileToParse in filesToRead) {

  fileToParse <- paste(fileLocation, fileToParse, sep = "")

  # STEP 2) Parse the XML
  doc <- XML::xmlTreeParse(fileToParse, useInternal = TRUE)
  myXML <- XML::xmlToList(doc)

  myResults <- NULL
  hierachyName <- NULL

  for (myElement in myXML[names(myXML) == "complexType"]) {
    myAttr <- myElement$.attrs
    names(myAttr) <- NULL

    if (grepl("^H.*", myAttr)) {
      hierachyName <- myAttr
    }
    if (nchar(myAttr) == 2 & !grepl("^H.*", myAttr)) {
      if (is.null(myResults)) {
        myResults <- c(myAttr)
      } else {
        myResults <- c(myResults, myAttr)
      }
    }
  }

  # Add to our list of results
  myHierarchyTables[[hierachyName]] <- myResults
}

tablesInRDBESHierarchies <- myHierarchyTables

usethis::use_data(tablesInRDBESHierarchies, overwrite = TRUE)
