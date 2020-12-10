#' generateSimpleExchangeFile Generate either a CE, CL, VD, or SL exchange format file for the RDBES stolen from Dave
#'
#' @param typeOfFile The file we want to create - allowed values are CL, CE, VD, or SL
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#' @param yearToUse The year we want to generate the CE file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param numberOfRows (Optional) Limit the output to this number of rows (just used for testing)
#' @param cleanData (Optional) if TRUE then remove any invalid rows from the data before generating the upload files - warning data will potentially be lost from your upload file if you do this!
#' @param RDBESvalidationdata (Optional) If you have selected to cleanData then you need to supply validation data (derived from BaseTypes.xsd)
#' @param RDBEScodeLists (Optional) If you have selected to cleanData then you need to supply reference data (derived from ICES vocabulary server)
#' 
#' @return
#' @export
#'
#' @examples generateSimpleExchangeFile(typeOfFile = 'CL', yearToUse = 2017, country = 'IE', RDBESdata = myRDBESData, numberOfRows=50,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
generateSimpleExchangeFile <- function(typeOfFile, outputFileName = "", yearToUse, country, RDBESdata,numberOfRows = NULL, cleanData = FALSE, RDBESvalidationdata = NULL, RDBEScodeLists = NULL){
  
  # We only need a 2 letter name for the file/table to use
  if (length(typeOfFile)>2){
    typeOfFile <- substr(typeOfFile,1,2)
  }
  
  # Stop if we don't have a valid file type
  if (!typeOfFile %in% c("CL","CE","VD","SL")){
    stop(paste("Invalid value for 'typeOfFile': ",typeOfFile))
  }
  
  ## Step 0 - Generate a file name if we need to 
  if (outputFileName == ""){
    fileName <- paste("H",typeOfFile,".csv", sep ="")
    outputFileName <- paste(country,yearToUse,fileName, sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)
  
  ## Step 1 - Filter the data and write it out
  
  RDBESdataForFile <- list(CE=RDBESdata[['CE']], CL=RDBESdata[['CL']], VD=RDBESdata[['VD']], SL=RDBESdata[['SL']])
  
  myData <- RDBESdataForFile[[typeOfFile]]
  
  # Filter the data by our input parameters
  if (typeOfFile == 'CE'){
    myDataForFile <- myData[myData$CEyear == yearToUse & myData$CEvesselFlagCountry == country,]
  } else if (typeOfFile == 'CL'){
    myDataForFile <- myData[myData$CLyear == yearToUse & myData$CLvesselFlagCountry == country,]
  } else if (typeOfFile == 'VD') {
    myDataForFile <- myData[myData$VDyear == yearToUse & myData$VDcountry == country,]
  } else if (typeOfFile == 'SL') {
    myDataForFile <- myData[myData$SLyear == yearToUse & myData$SLcountry == country,]
  }
  
  RDBESdataForFile[[typeOfFile]]<-myDataForFile
  
  # If we want to remove any invalid data before generating the upload files do this now
  if(cleanData){
    
    rowsBefore <- nrow(myDataForFile)
    print(paste(rowsBefore, ' rows before removing invalid data', sep =""))
    
    # Validate
    myErrors <- validateTables(RDBESdata = RDBESdataForFile, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists, shortOutput = FALSE,framestoValidate = c(typeOfFile))
    
    # Remove any invalid rows 
    myDataForFile <- removeInvalidRows(tableName = typeOfFile,dataToClean = myDataForFile,errorList = myErrors)
    
    rowsAfter <- nrow(myDataForFile)
    print(paste(rowsAfter, ' rows after removing invalid data', sep =""))
    
    if (rowsAfter < rowsBefore){
      missingRows <- rowsBefore - rowsAfter
      warning(paste(missingRows,' invalid rows removed before trying to generate output files', sep = ""))
    }
    
  }
  
  # If we only want a certain number of rows we'll subset the data (normally just used during testing)
  if (!is.null(numberOfRows)){
    if (nrow(myDataForFile) > numberOfRows)
    {
      myDataForFile <- myDataForFile[1:numberOfRows,]
      print(paste("File truncated to ",numberOfRows, " rows",sep=""))
    }
  }
  
  # We now write out the data frame with ids and column names included to make debugging easier
  fwrite(myDataForFile, paste(outputFolder, "debug_", outputFileName,sep="") ,row.names=F,col.names=T,quote=F)
  
  
  # Get rid of the XXid fields from our data - not included in the final output file
  colstoRemove <- names(myDataForFile)[grepl("^..id$",names(myDataForFile))]
  myDataForFile <- select(myDataForFile,-all_of(colstoRemove))
  
  # Replace any NAs with ''
  myDataForFile[is.na(myDataForFile)] <- ''
  
  # Get all the values and list them out
  myFinalData <- do.call('paste',c(myDataForFile,sep=','))
  
  # replace NA with blanks 
  # 20/8/20 - if we get rid of NAs liek this it also removes NAs from the middle of words :-S - we'll get rid of NAs from the data frame instead earlier
  #myFinalData <- gsub('NA','',myFinalData)
  
  # Write out the file
  fwrite(list(myFinalData), paste(outputFolder,outputFileName, sep = "") ,row.names=F,col.names=F,quote=F)
  print(paste("Output file written to ",outputFileName,sep=""))
  
}
