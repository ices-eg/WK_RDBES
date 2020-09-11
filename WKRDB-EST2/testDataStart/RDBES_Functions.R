# Author: https://github.com/davidcurrie2001
# Copied: 20200911


library(RODBC)
library(dplyr)
library(data.table)
library(XML)
library(icesVocab)
library(RCurl)
library(httr)
library(readxl)
library(compare)

# Location for our output files
outputFolder <- "./output/"

#' loadRDBESData
#' This function loads data that is already in the RDBES format from a relational database.
#' 
#' @param connectionString A string specifying the connection string to the database in a formt that odbcDriverConnect can use e.g. 'driver=SQL Server;server=mysqlhost;database=mydbname;trusted_connection=true'
#'
#' @return A named list containing the different RDBES tables
#' @export
#'
#' @examples
#' myRDBESData <- loadRDBESData(readRDS('connectionString.RDS'))
loadRDBESData <- function(connectionString){
  
  # Connect to the database
  channel <- odbcDriverConnect(connectionString)
  
  # Run queries to fetch the data
  myCE <- sqlQuery(channel,"select * from dbo.CE", stringsAsFactors = FALSE)
  myCL <- sqlQuery(channel,"select * from dbo.CL", stringsAsFactors = FALSE)
  
  myDE <- sqlQuery(channel,"select * from dbo.Design", stringsAsFactors = FALSE)
  mySD <- sqlQuery(channel,"select * from dbo.SamplingDetails", stringsAsFactors = FALSE)
  myOS <- sqlQuery(channel,"select * from dbo.OnshoreEvent", stringsAsFactors = FALSE)
  myLE <- sqlQuery(channel,"select * from dbo.LandingEvent", stringsAsFactors = FALSE)
  myVS <- sqlQuery(channel,"select * from dbo.VesselSelection", stringsAsFactors = FALSE)
  myFT <- sqlQuery(channel,"select * from dbo.FishingTrip", stringsAsFactors = FALSE)
  myFO <- sqlQuery(channel,"select * from dbo.FishingOperation", stringsAsFactors = FALSE)
  mySS <- sqlQuery(channel,"select * from dbo.SpeciesSelection", stringsAsFactors = FALSE)
  mySA <- sqlQuery(channel,"select * from dbo.Sample", stringsAsFactors = FALSE)
  myFM <- sqlQuery(channel,"select * from dbo.FrequencyMeasure", stringsAsFactors = FALSE)
  myBV <- sqlQuery(channel,"select * from dbo.BiologicalVariable", stringsAsFactors = FALSE)
  
  mySL <- sqlQuery(channel,"select * from dbo.SpeciesListDetails", stringsAsFactors = FALSE)
  myVD <- sqlQuery(channel,"select * from dbo.VesselDetails", stringsAsFactors = FALSE)
  
  #myLocodes <- sqlQuery(channel,"select * from dbo.PortLocodes", stringsAsFactors = FALSE)
  #myAphiaIds <- sqlQuery(channel,"select * from dbo.SpeciesAphiaIDs", stringsAsFactors = FALSE)
  
  # Close the connection
  close(channel)
  
  # Create a named list to return our data
  myRDBESData <- list(  CE = myCE
                        ,CL = myCL
                        ,DE = myDE
                        ,SD = mySD
                        ,OS = myOS
                        ,LE = myLE
                        ,VS = myVS
                        ,FT = myFT
                        ,FO = myFO
                        ,SS = mySS
                        ,SA = mySA
                        ,FM = myFM
                        ,BV = myBV
                        ,SL = mySL
                        ,VD = myVD
  )
  
  return(myRDBESData)
  
}

#' generateSimpleExchangeFile Generate either a CE, CL, VD, or SL exchange format file for the RDBES
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


#' generateComplexExchangeFile This function creates an RDBES exchange file for CS data
#'
#' @param typeOfFile The hierarchy we want to generate a CS file for e.g. 'H1'
#' @param yearToUse The year we want to generate the CS H5 file for
#' @param country The country to extract data for
#' @param RDBESdata A named list containing our RDBES data
#' @param outputFileName (Optional) The name we wish to give the file we produce - if not supplied a standard pattern will be used
#' @param numberOfSamples (Optional) Limit the output to this number of samples (just used for testing)
#' @param cleanData (Optional) if TRUE then remove any invalid rows from the data before generating the upload files - warning data will potentially be lost from your upload file if you do this!
#' @param RDBESvalidationdata (Optional) If you have selected to cleanData then you need to supply validation data (derived from BaseTypes.xsd)
#' @param RDBEScodeLists (Optional) If you have selected to cleanData then you need to supply reference data (derived from ICES vocabulary server)
#' @param RequiredTables A list of the tables required for each hierachy
#'
#' @return
#' @export
#'
#' @examples generateComplexExchangeFile(typeOfFile = 'H1', yearToUse = 2016, country = 'IE', RDBESdata = myRDBESData)
generateComplexExchangeFile <- function(typeOfFile, yearToUse, country, RDBESdata, outputFileName="", numberOfSamples = NULL, cleanData = FALSE, RDBESvalidationdata = NULL, RDBEScodeLists = NULL, RequiredTables){
  
  # For testing
  # typeOfFile <- 'H1'
  # RDBESdata<-myRDBESData
  # yearToUse <- 2019
  # country <- 'IE'
  # outputFileName <- ""
  # numberOfSamples <- 10
  # cleanData <- TRUE
  # RDBESvalidationdata <- validationData
  # RDBEScodeLists <- allowedValues
  # RequiredTables <- allRequiredTables
  
  ## Step 0 - Check typeOfFile and generate a file name if we need to 
  testedCSfileTypes <- c('H1','H5')
  
  if (!typeOfFile %in% testedCSfileTypes){
    warning(paste("Method not tested for ",typeOfFile, " yet", sep =""))
  }
  
  if (outputFileName == ""){
    outputFileName <- paste(country,yearToUse,paste(typeOfFile,".csv", sep = ""), sep ="_")
  }
  
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(outputFolder)), dir.create(file.path(outputFolder)), FALSE)
  
  # Find which tables we need for this file type
  upperHierarchy <- substr(typeOfFile,2,nchar(typeOfFile))
  requiredTables <- RequiredTables[[typeOfFile]]
  
  ## Step 1 - Filter the data
  myCSData <- filterCSData(RDBESdata = RDBESdata , RequiredTables = requiredTables, YearToFilterBy = yearToUse, CountryToFilterBy = country, UpperHierarchyToFilterBy = upperHierarchy)
  
  
  # If we want to remove any invalid data before generating the upload files do this now
  if(cleanData){
    
    myCSData <- cleanCSData(DataToClean = myCSData, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists, RequiredTables = requiredTables, YearToFilterBy = yearToUse, CountryToFilterBy = country, UpperHierarchyToFilterBy = upperHierarchy)
    
  }
  
  
  # If required, limit the number of samples we will output (normally just used during testing)
  if (!is.null(numberOfSamples)){
    
    myCSData <- limitSamplesInCSData(DataToFilter = myCSData, NumberOfSamples = numberOfSamples, RequiredTables = requiredTables)
    
  }
  
  
  ## Step 2 - I now add a SortOrder field to each of our fitlered data frames
  ## this will allow me to generate the CS file in the correct row order without needing a slow for-loop
  myCSData <- generateSortOrder(RDBESdataToSort = myCSData, RequiredTables = requiredTables)
  
  # Combine our SortOrder values
  # TODO Need to double-check this works correctly
  for (myRequiredTable in requiredTables){
    if (myRequiredTable == 'DE'){
      FileSortOrder <- myCSData[[myRequiredTable]]$SortOrder
    } else {
      FileSortOrder <-c(FileSortOrder,myCSData[[myRequiredTable]]$SortOrder)
    }
  }
  
  
  ## STEP 3) Create a version of the output data for debugging
  
  # Here we create a version of the output data with all the ids and sorting columns in so I can check things are correct
  for (myRequiredTable in requiredTables){
    if (myRequiredTable == 'DE'){
      csForChecking <- do.call('paste',c(myCSData[[myRequiredTable]],sep=','))
    } else {
      csForChecking <- c(csForChecking,do.call('paste',c(myCSData[[myRequiredTable]],sep=',')))
    }
  }
  
  # Sort the output into the correct order
  csForCheckingOrdered <- csForChecking[order(FileSortOrder)]
  
  # Write out a file with the row ids left in - used for debugging and checking the output
  fwrite(list(csForCheckingOrdered), paste(outputFolder,"debug_", outputFileName,sep="") ,row.names=F,col.names=F,quote=F)
  
  ## STEP 4) Create the real version of the output data
  
  # Create the CS data with the sort columns and ids removed - this will then be used to generate the exchange file
  for (myRequiredTable in requiredTables){
    
    # First remove the columns we don't want in the final output (SortOrder and any XXid columns)
    colstoRemove <- c("SortOrder", names(myCSData[[myRequiredTable]])[grepl("^..id$",names(myCSData[[myRequiredTable]]))])
    myData <- select(myCSData[[myRequiredTable]],-all_of(colstoRemove))
    
    # Replace any NAs with ''
    myData[is.na(myData)] <- ''
    
    # Now stick all the lines from each table together with commas seperating the values
    if (myRequiredTable == 'DE'){
      cs <- do.call('paste',c(myData,sep=','))
    } else {
      cs <- c(cs,do.call('paste',c(myData,sep=',')))
    }
  }
  
  # Sort the output into the correct order
  csOrdered <- cs[order(FileSortOrder)]
  
  # replace NA with blanks
  # 20/8/20 - if we get rid of NAs like this it also removes NAs from the middle of words :-S - we'll get rid of NAs from the data frame instead earlier
  #csOrdered <- gsub('NA','',csOrdered)
  
  fwrite(list(csOrdered), paste(outputFolder,outputFileName, sep = "") ,row.names=F,col.names=F,quote=F)
  print(paste("Output file written to ",outputFileName,sep=""))
  
}


#' limitSamplesInCSData
#'
#' @param DataToFilter 
#' @param NumberOfSamples 
#' @param RequiredTables 
#'
#' @return
#' @export
#'
#' @examples
limitSamplesInCSData <- function(DataToFilter, NumberOfSamples, RequiredTables){
  
  # Get our samples (not including sub-samples)
  NotSubSamples <- DataToFilter[['SA']][is.na(DataToFilter[['SA']]$SAparentSequenceNumber),]
  
  #if (nrow(myCSData[['SA']])>numberOfSamples ) {
  if (nrow(NotSubSamples)>NumberOfSamples ) {
    
    #Subset the data to get the SAid values we are interested it
    #SAidsToUse <- myCSData[['SA']][1:numberOfSamples,"SAid"]
    SAidsToUse <- NotSubSamples[1:NumberOfSamples,"SAid"]
    
    # Need Sort out SA and FM first, then we'll deal with the other tables
    
    # SA : Top level samples not including sub-samplea
    DataToFilter[['SA']]<- DataToFilter[['SA']][DataToFilter[['SA']]$SAid %in% SAidsToUse,]
    # Now handle any sub-samples
    mySubSampleData <- DataToFilter[['SA']][!is.na(DataToFilter[['SA']]$SAparentSequenceNumber),]
    
    # If we have any sub-samples see if we need to include them
    if (nrow(mySubSampleData) > 0){
      # Use a recursive function to fetch the top level sequence number of our sub-samples
      mySubSampleData$topLevelSequenceNumber <- sapply(mySubSampleData$SAsequenceNumber,getTopLevelSequenceNumber,SAdata = mySubSampleData)
      # Only include sub-samples if their top level sequence numebr is in our filtered sample data
      mySubSampleData <- mySubSampleData[mySubSampleData$topLevelSequenceNumber %in% DataToFilter[['SA']]$SAsequenceNumber,]
      # Remove the column we added
      mySubSampleData$topLevelSequenceNumber <- NULL
      # Combine our samples and sub-samples together
      DataToFilter[['SA']] <- rbind(DataToFilter[['SA']],mySubSampleData)
    }
    
    # FM
    DataToFilter[['FM']]<- DataToFilter[['FM']][DataToFilter[['FM']]$SAid %in% DataToFilter[['SA']]$SAid,]
    
    # Now deal with all the other tables
    myData <- NULL
    previousRequiredTable <- NULL
    
    # Iterate through the required tables in reverse order and remove any records not assoicated with our selected samples
    for (myRequiredTable in rev(RequiredTables)){
      
      if (myRequiredTable %in% c('SA','FM')){
        # Do nothing - already handled above
      } 
      # Need to check if the BV records are in either the FM or SA tables
      else if (myRequiredTable == 'BV'){
        
        # don't assume FMid and SAid always exist
        myData <- DataToFilter[['BV']][DataToFilter[['BV']]$FMid %in% DataToFilter[['FM']]$FMid | DataToFilter[['BV']]$SAid %in% DataToFilter[['SA']]$SAid,]
        
        if ('FMid' %in% names(DataToFilter[['BV']])){
          data1 <- DataToFilter[['BV']][DataToFilter[['BV']]$FMid %in% DataToFilter[['FM']]$FMid,]
        } else {
          data1 <- NULL
        }
        
        if ('SAid' %in% names(DataToFilter[['BV']])){
          data2 <- DataToFilter[['BV']][DataToFilter[['BV']]$SAid %in% DataToFilter[['SA']]$SAid,]
        } else {
          data2 <- NULL
        }
        
        #DataToFilter[['BV']] <- myData
        DataToFilter[['BV']] <- rbind(data1,data2)
      } 
      # Other tables can follow a general pattern
      else {
        
        previousHierarchyTable <- DataToFilter[[previousRequiredTable]]
        ## Assume the primary key is the first field
        currentPrimaryKey <- names(DataToFilter[[myRequiredTable]])[1]
        myData <- DataToFilter[[myRequiredTable]][DataToFilter[[myRequiredTable]][,currentPrimaryKey] %in% previousHierarchyTable[,currentPrimaryKey],]
        DataToFilter[[myRequiredTable]] = myData
        
      }
      
      previousRequiredTable <- myRequiredTable
      
    }
    
    print(paste("File truncated to data relating to ",NumberOfSamples, " samples",sep=""))
  }
  
  DataToFilter
}

#' cleanCSData
#'
#' @param DataToClean 
#' @param RDBESvalidationdata 
#' @param RDBEScodeLists 
#' @param RequiredTables 
#' @param YearToFilterBy 
#' @param CountryToFilterBy 
#' @param UpperHierarchyToFilterBy 
#'
#' @return
#' @export
#'
#' @examples
cleanCSData <- function(DataToClean,RDBESvalidationdata, RDBEScodeLists, RequiredTables, YearToFilterBy, CountryToFilterBy,UpperHierarchyToFilterBy ){
  
  # For testing
  # typeOfFile <- 'H1'
  # UpperHierarchyToFilterBy <- substr(typeOfFile,2,nchar(typeOfFile))
  # YearToFilterBy <- 2019
  # CountryToFilterBy <- 'IE'
  # outputFileName <- ""
  # RDBESvalidationdata <- validationData
  # RDBEScodeLists <- allowedValues
  # #RequiredTables <- requiredTables
  # requiredTables <- allRequiredTables[[typeOfFile]]
  # DataToClean <- filterCSData(RDBESdata = RDBESdata , RequiredTables = requiredTables, YearToFilterBy = YearToFilterBy, CountryToFilterBy = CountryToFilterBy, UpperHierarchyToFilterBy = UpperHierarchyToFilterBy)
  
  
  rowsBefore <- 0
  for (myRequiredTable in RequiredTables){
    rowsBefore <- rowsBefore + nrow(DataToClean[[myRequiredTable]])
  }
  print(paste(rowsBefore, ' rows before removing invalid data', sep =""))
  
  # Validate 
  myErrors <- validateTables(RDBESdata = DataToClean
                             ,RDBESvalidationdata = RDBESvalidationdata
                             ,RDBEScodeLists = RDBEScodeLists
                             ,shortOutput = FALSE
                             ,framestoValidate = RequiredTables
  )
  
  # Remove any invalid rows 
  for (myRequiredTable in RequiredTables){
    DataToClean[[myRequiredTable]]<- removeInvalidRows(tableName = myRequiredTable,dataToClean = DataToClean[[myRequiredTable]],errorList = myErrors)
  }
  
  # Filter the data again to ensure we don't have any orphan rows in our output
  DataToClean <- filterCSData(RDBESdata = DataToClean , RequiredTables = RequiredTables, YearToFilterBy = YearToFilterBy, CountryToFilterBy = CountryToFilterBy, UpperHierarchyToFilterBy = UpperHierarchyToFilterBy)
  
  rowsAfter <- 0
  for (myRequiredTable in RequiredTables){
    rowsAfter <- rowsAfter + nrow(DataToClean[[myRequiredTable]])
  }
  print(paste(rowsAfter, ' rows after removing invalid data', sep =""))
  
  if (rowsAfter < rowsBefore){
    missingRows <- rowsBefore - rowsAfter
    warning(paste(missingRows,' invalid rows removed before trying to generate output files', sep = ""))
  }
  
  DataToClean
  
}


#' filterCSData Filter the CS data to sub-set by year, country, and hierarchy
#'
#' @param RDBESdata 
#' @param RequiredTables 
#' @param YearToFilterBy 
#' @param CountryToFilterBy 
#' @param UpperHierarchyToFilterBy 
#'
#' @return
#' @export
#'
#' @examples
filterCSData <- function(RDBESdata, RequiredTables, YearToFilterBy, CountryToFilterBy, UpperHierarchyToFilterBy){
  
  # For testing
  # RDBESdata <- myRDBESData
  # RequiredTables <- requiredTables
  # YearToFilterBy <- 2017
  # CountryToFilterBy <- 'IE'
  # UpperHierarchyToFilterBy <- 1
  
  myCSData <- list()
  myData <- NULL
  previousRequiredTable <- NULL
  
  # Get the data for each required table - filter by year, country, and upper hierarchy
  for (myRequiredTable in RequiredTables){
    
    myData <- RDBESdata[[myRequiredTable]]
    
    # Need to filter DE by year and upper hieararchy
    if (myRequiredTable == 'DE'){
      myData <- myData[myData$DEyear == YearToFilterBy & myData$DEhierarchy == UpperHierarchyToFilterBy,]
      myCSData[[myRequiredTable]] = myData
    } 
    # Need to filter SD by country
    else if (myRequiredTable == 'SD'){
      myData <- myData[myData$DEid %in% myCSData[[previousRequiredTable]]$DEid & myData$SDcountry == CountryToFilterBy,]
      myCSData[[myRequiredTable]] = myData
    }
    # Need to handle samples and sub-samples for SA
    else if (myRequiredTable == 'SA'){
      
      #Lets deal with Samples first
      previousHierarchyTable <- myCSData[[previousRequiredTable]]
      ## Assume the primary key is the first field
      previousPrimaryKey <- names(previousHierarchyTable)[1]
      mySampleData <- myData[myData[,previousPrimaryKey] %in% previousHierarchyTable[,previousPrimaryKey],]
      
      # Now handle any sub-samples
      mySubSampleData <- myData[!is.na(myData$SAparentSequenceNumber),]
      
      # Use a recursive function to fetch the top level sequence number of our sub-samples
      mySubSampleData$topLevelSequenceNumber <- sapply(mySubSampleData$SAsequenceNumber,getTopLevelSequenceNumber,SAdata = mySubSampleData)
      
      # Only include sub-samples if their top level sequence numebr is in our filtered sample data
      mySubSampleData <- mySubSampleData[mySubSampleData$topLevelSequenceNumber %in% mySampleData$SAsequenceNumber,]
      
      # Get rid of the column we added earlier
      mySubSampleData$topLevelSequenceNumber <- NULL
      
      # Stick our samples and sub-samples together
      myData <- rbind(mySampleData, mySubSampleData)
      
      myCSData[[myRequiredTable]] = myData
      
    }
    # BVid can either be in FM or SA
    else if (myRequiredTable == 'BV'){
      
      #myData <- myData[myData$FMid %in% myCSData[['FM']]$FMid | myData$SAid %in% myCSData[['SA']]$SAid,]
      # Don't just assume the FMid or SAid columns are present - check first
      if ('FMid' %in% names(myData)){
        myData1 <- myData[myData$FMid %in% myCSData[['FM']]$FMid,]
      } else {
        myData1 <- NULL
      }
      if ('SAid' %in% names(myData)){
        myData2 <- myData[myData$SAid %in% myCSData[['SA']]$SAid,]
      } else {
        myData2 <- NULL
      }
      
      
      #myCSData[[myRequiredTable]] = myData
      myCSData[[myRequiredTable]] = rbind(myData1,myData2)
    } 
    # all other tables can follow a general pattern of matching
    else {
      #previousHierarchyTable <- RDBESdata[[myRequiredTable]]
      previousHierarchyTable <- myCSData[[previousRequiredTable]]
      ## Assume the primary key is the first field
      previousPrimaryKey <- names(previousHierarchyTable)[1]
      myData <- myData[myData[,previousPrimaryKey] %in% previousHierarchyTable[,previousPrimaryKey],]
      myCSData[[myRequiredTable]] = myData
    }
    
    previousRequiredTable <- myRequiredTable
  }
  
  
  myCSData
  
}

#' getTopLevelSequenceNumber Recursive function to get the top level SAsequenceNumber of a series of sameples and sub-samples
#'
#' @param SAdata 
#' @param SAsequenceNumber 
#'
#' @return
#' @export
#'
#' @examples
getTopLevelSequenceNumber <- function(SAdata,SAsequenceNumber ){
  #print(SAsequenceNumber)
  dataToCheck <- SAdata[SAdata$SAsequenceNumber == SAsequenceNumber,]
  
  # If we have mutiple matches we probably don't have unique SAsequenceNumber values
  if (nrow(dataToCheck) > 1){
    warning("There is a problem with non-unique SAsequenceNumber values- check your data")
    # Just use the first match
    dataToCheck <- dataToCheck[1,]
  }
  
  if (nrow(dataToCheck) == 0) {
    return (NA)
  } else if (is.na(dataToCheck$SAparentSequenceNumber)) {
    return (SAsequenceNumber)
  } else {
    return (getTopLevelSequenceNumber(SAdata = SAdata,SAsequenceNumber = dataToCheck$SAparentSequenceNumber))
  }
  
}


#' generateSortOrder Adds a SortOrder field to each frame in our list of data frames.  When the data is sorted by this column it shoudl be in the correct order to generate a CS exchange file.  Even if the data frame is empty I add in a blank SortOrder column - that way I can guarantee it exists
#'
#' @param RDBESdataToSort 
#' @param RequiredTables 
#'
#' @return
#' @export
#'
#' @examples
generateSortOrder <- function(RDBESdataToSort, RequiredTables){
  
  # For testing
  #RDBESdataToSort <- myCSData
  #RequiredTables <- requiredTables
  
  # IMPORTANT - I'm using inner_join from dply so we can maintain the ordering of the first data frame in the join
  # if the ordering isn't maintained then the exchange file will be output in the wrong order
  
  # TODO For our data BV only follows SA not FM - need to check that the sort order will work if there is a mix of lower hierarchies
  
  previousRequiredTable <- NULL
  
  for (myRequiredTable in RequiredTables){
    
    # Check if there are any rows in this table
    if(nrow(RDBESdataToSort[[myRequiredTable]])>0) {
      
      # Need to handle DE differently because the SortOrder doesn't just use the primary key
      if (myRequiredTable == 'DE'){
        
        RDBESdataToSort[[myRequiredTable]]$SortOrder <- paste(RDBESdataToSort[[myRequiredTable]]$DEhierarchy,RDBESdataToSort[[myRequiredTable]]$DEyear,RDBESdataToSort[[myRequiredTable]]$DEstratum,sep="-")
        
      } 
      # Need to handle SA differently because there can be sub-samples
      else if (myRequiredTable == 'SA'){
        
        # We will use SAsequenceNumber in the SortOrder - this shoudl ensure all samples and sub-samples end-up in the correct order
        # TODO this needs checking
        
        previousHierarchyTable <- RDBESdataToSort[[previousRequiredTable]]
        ## Assume the primary key is the first field
        previousPrimaryKey <- names(previousHierarchyTable)[1]
        currentPrimaryKey <- names(RDBESdataToSort[[myRequiredTable]])[1]
        # Create the value for SortOrder based on the value of SortOrder from the previous table, and the current primary key
        RDBESdataToSort[[myRequiredTable]]$SortOrder <- paste( inner_join(RDBESdataToSort[[myRequiredTable]],previousHierarchyTable, by =previousPrimaryKey)[,c("SortOrder")], RDBESdataToSort[[myRequiredTable]][,"SAsequenceNumber"], sep = "-")
        
      }
      # Need to handle BV differently because it can be linked to from either FM or SA
      else if (myRequiredTable == 'BV') {
        
        # Add the SortOrder field first
        # Bit ugly but we'll call it SortOrder_BV to start with to avoid some issues - we'll name it propery in a minute
        RDBESdataToSort[[myRequiredTable]]$SortOrder_BV <- character(nrow(RDBESdataToSort[[myRequiredTable]]))
        currentPrimaryKey <- names(RDBESdataToSort[[myRequiredTable]])[1]
        
        # Add SortOrder where there is a link to FM (rows where FMid is not NA)
        if (nrow( RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$FMid),] )>0){
          
          previousHierarchyTable <- RDBESdataToSort[['FM']]
          previousPrimaryKey <- names(previousHierarchyTable)[1]
          # Create the value for SortOrder based on the value of SortOrder from the previous table, and the current primary key (just for the BV rows that have an FMid)
          RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$FMid),"SortOrder_BV"] <- paste( inner_join(RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$FMid),],previousHierarchyTable, by =previousPrimaryKey)[,c("SortOrder")], RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$FMid),currentPrimaryKey], sep = "-")
        } 
        
        # Add SortOrder where there is a link to SA (rows where SAid is not NA)
        if (nrow( RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$SAid),] )>0){
          
          previousHierarchyTable <- RDBESdataToSort[['SA']]
          previousPrimaryKey <- names(previousHierarchyTable)[1]
          # Create the value for SortOrder based on the value of SortOrder from the previous table, and the current primary key (just for BV rows that have an SAid)
          RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$SAid),"SortOrder_BV"] <- paste( inner_join(RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$SAid),],previousHierarchyTable, by =previousPrimaryKey)[,c("SortOrder")], RDBESdataToSort[[myRequiredTable]][!is.na(RDBESdataToSort[[myRequiredTable]]$SAid),currentPrimaryKey], sep = "-")
          
        }
        
        # Rename SortOrder_BV field to SortOrder
        names(RDBESdataToSort[[myRequiredTable]])[names(RDBESdataToSort[[myRequiredTable]]) == "SortOrder_BV"] <- "SortOrder"
        
      }
      # Else follow the general pattern
      else {
        
        previousHierarchyTable <- RDBESdataToSort[[previousRequiredTable]]
        ## Assume the primary key is the first field
        previousPrimaryKey <- names(previousHierarchyTable)[1]
        currentPrimaryKey <- names(RDBESdataToSort[[myRequiredTable]])[1]
        # Create the value for SortOrder based on the value of SortOrder from the previous table, and the current primary key
        RDBESdataToSort[[myRequiredTable]]$SortOrder <- paste( inner_join(RDBESdataToSort[[myRequiredTable]],previousHierarchyTable, by =previousPrimaryKey)[,c("SortOrder")], RDBESdataToSort[[myRequiredTable]][,currentPrimaryKey], sep = "-")
      }
      # If there's no rows in the table we'll add an emtpy SortOrder column so it definitely exists
    } else  {
      RDBESdataToSort[[myRequiredTable]]$SortOrder <- character(0)
    }
    
    previousRequiredTable <- myRequiredTable
  }
  
  RDBESdataToSort
  
}


#' saveRDataFilesForCS Saves RData files containing the relevent data frames for a CS upper hierarchy.  Each data frame is stored in a seperate RData file.  Data is filtered by country and year 
#'
#' @param typeOfFile 
#' @param yearToUse 
#' @param country 
#' @param RDBESdata 
#' @param RequiredTables 
#'
#' @return
#' @export
#'
#' @examples
saveRDataFilesForCS <- function(typeOfFile, yearToUse, country, RDBESdata, RequiredTables){
  
  # Find which tables we need for this file type
  upperHierarchy <- substr(typeOfFile,2,nchar(typeOfFile))
  requiredTables <- RequiredTables[[typeOfFile]]
  
  ## Step 0
  # Create the output directory if we need do 
  ifelse(!dir.exists(file.path(paste0(outputFolder,typeOfFile))), dir.create(file.path(paste0(outputFolder,typeOfFile))), FALSE)
  
  ## Step 1 - Filter the data
  myCSData <- filterCSData(RDBESdata = RDBESdata , RequiredTables = requiredTables, YearToFilterBy = yearToUse, CountryToFilterBy = country, UpperHierarchyToFilterBy = upperHierarchy)
  
  ## Step 2 Save the data
  for (myRequiredTable in requiredTables){
    frameToSave <- RDBESdata[[myRequiredTable]]
    save(frameToSave, file = paste0(outputFolder,typeOfFile,"/",myRequiredTable, ".RData"))
  }
  
}

#' getFieldNameMapping Get a data frame that maps between database names and the shorter R names
#'
#' @param downloadFromGitHub TRUE if you want to download the latest data model spreadsheets from GitHub
#' @param gitHubDirectory (Optional) Default value is "https://api.github.com/repos/ices-tools-dev/RDBES/contents/Documents"
#' @param fileLocation The location you want to save and read the data model spredsheet from
#'
#' @return
#' @export
#'
#' @examples
getFieldNameMapping <- function(downloadFromGitHub= TRUE, gitHubDirectory = "https://api.github.com/repos/ices-tools-dev/RDBES/contents/Documents",  fileLocation){
  
  # For testing
  #downloadFromGitHub = TRUE
  #fileLocation <- './tableDefs/'
  #gitHubDirectory <- "https://api.github.com/repos/ices-tools-dev/RDBES/contents/Documents"
  
  
  if (downloadFromGitHub){
    
    myDataModelFiles <- NULL
    myResponse <- GET(gitHubDirectory)
    filesOnGitHub <- content(myResponse)
    
    for (myFile in filesOnGitHub){
      myGitHubFile <- data.frame(fileName = myFile$name, downloadURL = myFile$download_url)
      if (is.null(myDataModelFiles)){
        myDataModelFiles <- myGitHubFile 
      } else {
        myDataModelFiles <- rbind(myDataModelFiles,myGitHubFile)
      }
    }
    # Sub-set to the files we are interested in
    myDataModelFiles <- myDataModelFiles[grepl('^.*Data Model.*xlsx$',myDataModelFiles$fileName),]
    
    print(paste("Downloading ",nrow(myDataModelFiles), " files from GitHub", sep =""))
    
    # Download our files
    for (i in 1:nrow(myDataModelFiles)){
      aDataModelFile <- getBinaryURL(myDataModelFiles[i,'downloadURL'])
      # save the file locally
      myFileConnection = file(paste(fileLocation,myDataModelFiles[i,'fileName'], sep = ""), "wb")
      writeBin(aDataModelFile, myFileConnection)
      aDataModelFile <- NA
      close(myFileConnection)
    }
    
    print("Finished downloading")
    
  }
  
  # Now we'll read the spreadsheets
  # (Need to find the names of the files again in case we haven't dowloaded them in this function call)
  
  filesToRead <- list.files(path = fileLocation, pattern = "*.xlsx", recursive = FALSE, full.names = FALSE)
  
  dataModel <- list()
  
  # get the contents of each relevent worksheet in our spreadsheets
  for (myfile in filesToRead){
    
    myFileLocation <- paste(fileLocation,myfile, sep = "")
    myFileSheets <- excel_sheets(myFileLocation)
    
    # CE CL
    if (grepl('^.*CL CE.*xlsx$',myfile)){
      
      print("Loading CL CE names")
      # Add the sheets to the dataModel list
      dataModel[['CE']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*CE.*",myFileSheets)])
      dataModel[['CL']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*CL.*",myFileSheets)])
    } 
    # VD SL
    else if (grepl('^.*VD SL.*xlsx$',myfile)){
      
      print("Loading VD SL names")
      # Add the sheets to the dataModel list
      dataModel[['VD']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*Vessel.*",myFileSheets)])
      dataModel[['SL']] <- read_excel(myFileLocation,sheet = myFileSheets[grepl(".*Species.*",myFileSheets)])
      
    } 
    # CS
    else if (myfile == "RDBES Data Model.xlsx"){
      
      for (aFileSheet in myFileSheets) {
        if (!grepl(".*Model.*",aFileSheet)){
          print(paste("Loading ", aFileSheet, " names", sep = ""))
          myDataModel <- read_excel(myFileLocation,sheet = aFileSheet)
          dataModel[[aFileSheet]] <- myDataModel
        }
        
      }
    }
  }
  
  # Put the field names and R names from each entry in the list into a single data frame
  
  myNameMappings <- NULL
  
  for (i in 1:length(dataModel)){
    
    myDataModelEntry <- dataModel[[i]]
    validColumnNames <- names(myDataModelEntry)[names(myDataModelEntry) %in% c("Field Name","R Name")]
    
    if (length(validColumnNames == 2)) {
      aNameMapping <- myDataModelEntry[,c("Field Name","R Name")]
      
      # Add in the Table Name - based on the first 2 letters of the first entry
      if (nrow(aNameMapping)>0) {
        tableName <- substr(aNameMapping[1,1],1,2)
      } else {
        tableName <- NA
      }
      aNameMapping$TableName <- tableName
      
      myNameMappings <- rbind(myNameMappings,aNameMapping)
    } else {
      print(paste("Not including ",names(dataModel)[i], " in mapping due to invalid column names",sep=""))
    }
    
  }
  
  # Remove any NAs
  if (!is.null(myNameMappings)){
    myNameMappings <- myNameMappings[!is.na(myNameMappings[,"Field Name"]) & !is.na(myNameMappings[,"R Name"]),]
  }
  
  myNameMappings
  
}


#' changeFieldNamesForFrame Change the field names of an RDBES data frame either from database names to R names or vice versa
#'
#' @param frameToRename The RDBES data frame we want to rename
#' @param fieldNameMap The data frame holding our names mappings
#' @param typeOfChange Either RtoDB or DBtoR
#'
#' @return
#' @export
#'
#' @examples changeFieldNames(frameToRename = x, fieldNameMap = list_RDBES_Variables, typeOfChange = "DBtoR")
changeFieldNamesForFrame <- function(frameToRename, fieldNameMap, typeOfChange){
  
  # For testing
  #frameToRename <- myRDBESData[["BV"]]
  #fieldNameMap <- fieldNameMapping
  #typeOfChange <- "DBtoR"
  
  if (!typeOfChange %in% c("RtoDB", "DBtoR")) stop("typeOfChange parameter should be either RtoDB or DBtoR")
  
  # Get the current column names into a data frame
  myDF <- data.frame(name = names(frameToRename), stringsAsFactors = FALSE)
  
  # Assume the first entry is always the primary key of the table and extract the table name
  myTableName <- substr(myDF[1,"name"],1,2)
  
  # IF change DB to R
  if (typeOfChange == "DBtoR"){
    
    # Left join our current names against the replacement names (match to DB names)
    myMapping <- left_join(myDF,fieldNameMap[fieldNameMap$TableName==myTableName,c("Field Name","R Name")], by=c("name" = "Field Name"))
    
    # Make sure we don't have any NAs in the list of names we'll use
    myMapping[,"R Name"] <- ifelse(is.na(myMapping[,"R Name"]),myMapping$name,myMapping[,"R Name"])
    
    # set the new names
    myNewNames <- myMapping[,"R Name"]
    
  } else if (typeOfChange == "RtoDB"){
    
    # Left join our current names against the replacement names (match to R names)
    myMapping <- left_join(myDF,fieldNameMap[fieldNameMap$TableName==myTableName,c("Field Name","R Name")], by=c("name" = "R Name"))
    
    # Make sure we don't have any NAs in the list of names we'll use
    myMapping[,"Field Name"] <- ifelse(is.na(myMapping[,"Field Name"]),myMapping$name,myMapping[,"Field Name"])
    
    # set the new names
    myNewNames <- myMapping[,"Field Name"]
    
  }
  
  # Change the names of our data frame
  names(frameToRename) <- myNewNames
  
  # return the data frame with the new names
  frameToRename
  
}

#' changeFieldNames  Change the field names of our RDBES data either from database names to R names or vice versa
#'
#' @param RDBESdata The RDBES data we want to rename
#' @param fieldNameMap The data frame holding our names mappings
#' @param typeOfChange Either RtoDB or DBtoR
#'
#' @return
#' @export
#'
#' @examples
changeFieldNames <- function(RDBESdata, fieldNameMap, typeOfChange){
  
  if (!typeOfChange %in% c("RtoDB", "DBtoR")) stop("typeOfChange parameter should be either RtoDB or DBtoR")
  
  for (i in 1:length(RDBESdata)){
    
    #myFrame <- RDBESdata[[i]]
    RDBESdata[[i]] <- changeFieldNamesForFrame(frameToRename = RDBESdata[[i]], fieldNameMap = fieldNameMap, typeOfChange = typeOfChange)
    
  }
  
  RDBESdata
  
}

#' loadRDataFiles Load all the RData files from a directory
#'
#' @param directoryToSearch Directory to load files from
#' @param recursive Should we search for files recursivley? Default = FALSE
#'
#' @return
#' @export
#'
#' @examples
loadRDataFiles <- function(directoryToSearch, recursive = FALSE){
  
  # For testing
  #directoryToSearch = "./output/H5"
  #recursive = FALSE
  
  # Get a list of the RData files
  filesToRead <- list.files(path = directoryToSearch, pattern = "*.RData", recursive = recursive, full.names = TRUE)
  
  # Load each file
  for (i in 1:length(filesToRead)){
    load(file=filesToRead[i])
  }
  
  
}

#' getValidationData Gets the base types xsd so we know what data type each field should be and what the code list is
#'
#' @param downloadFromGitHub (Optional) Set to TRUE to download the latest version from GitHub
#' @param gitHubFileLocation (Optional) Default value is "https://raw.githubusercontent.com/ices-tools-dev/RDBES/master/XSD-files/BaseTypes.xsd"
#' @param fileLocation Location of the base types xsd file
#'
#' @return
#' @export
#'
#' @examples validationData <- getValidationData(fileLocation = './tableDefs/BaseTypes.xsd')
getValidationData <- function(downloadFromGitHub = TRUE,gitHubFileLocation = "https://raw.githubusercontent.com/ices-tools-dev/RDBES/master/XSD-files/BaseTypes.xsd", fileLocation){
  
  # For testing
  #downloadFromGitHub = FALSE
  #fileLocation <- './tableDefs/BaseTypes.xsd'
  
  # STEP 1) Get the BaseTypes file (if required)
  if (downloadFromGitHub){
    # get the latest BaseTypes.xsd file from GitHub
    myBaseTypes <- getURL(gitHubFileLocation)
    # save the file locally
    writeLines(myBaseTypes, fileLocation)
  }
  
  # STEP 2) Parse the XML
  doc <- xmlTreeParse(fileLocation,useInternal= TRUE)
  myXML <- xmlToList(doc)
  
  # STEP 3) Get all the field names and their types for each table (stored as "complexType" entries)
  
  # Data frame to hold our validation info
  myValidationDF <- NULL
  
  # Get the infromation we want from the xsd file - hard-code to the current structure...
  for (myElement in myXML[names(myXML) == "complexType"]){
    if(names(myElement)[[1]]=="sequence"){
      for (mySubElement in myElement[[1]]){
        myMin <- mySubElement[[1]]
        myMax <- mySubElement[[2]]
        myName <- mySubElement[[3]]
        myType <- mySubElement[[4]]
        myDF <- data.frame(min=myMin, max=myMax, name=myName, type=myType, stringsAsFactors = FALSE)
        if (is.null(myValidationDF)){
          myValidationDF <- myDF
        } else {
          myValidationDF <- rbind(myValidationDF,myDF)
        }
      }
    }
  }
  
  # STEP 4) Get the vaidation information for the things like decimal ranges (stored as "simpleType")
  
  # Data frame to hold our validation info
  myValidationSimpleTypes <- NULL
  
  # Get the infromation we want from the xsd file - hard-code to the current structure...
  for (myElement in myXML[names(myXML) == "simpleType"]){
    myName <- myElement$.attrs
    myType <- myElement[[1]]$.attrs
    myMin <- myElement[[1]]$minInclusive
    if (is.null(myMin)) {myMin <- NA}
    myMax <- myElement[[1]]$maxInclusive
    if (is.null(myMax)) {myMax <- NA}
    myfractionDigits <- myElement[[1]]$fractionDigits
    if (is.null(myfractionDigits)) {myfractionDigits <- NA}
    myLength <- myElement[[1]]$length
    if (is.null(myLength)) {myLength <- NA}
    myPattern <- myElement[[1]]$pattern
    if (is.null(myPattern)) {myPattern <- NA}
    myDF <- data.frame(name=myName,checkName=myName, description="simpleTypeCheck", minValue=myMin, maxValue=myMax, dataType=myType, fractionDigits = myfractionDigits, length = myLength, pattern = myPattern,  stringsAsFactors = FALSE)
    if (is.null(myValidationSimpleTypes)){
      myValidationSimpleTypes <- myDF
    } else {
      myValidationSimpleTypes <- rbind(myValidationSimpleTypes,myDF)
    }
    
  }
  
  # STEP 5) Join our validation together and return it
  myValidationDF <- left_join(myValidationDF,myValidationSimpleTypes,by=c("type" = "name"))
  
  
  myValidationDF[!is.na(myValidationDF$dataType),"type"] <- myValidationDF[!is.na(myValidationDF$dataType),"dataType"]
  myValidationDF$dataType <- NULL
  
  myValidationDF
  
  
  
}

#' getTablesInHierarchies Used the H* xsd files to define which tables are required in the different hierachies
#'
#' @param downloadFromGitHub (Optional) Set to TRUE if you want to download the lastest xsd files from GitHub
#' @param gitHubFileLocation (Optional) Default value is "https://api.github.com/repos/ices-tools-dev/RDBES/contents/XSD-files"
#' @param fileLocation The folder to read the files from.  If you are downloading from GitHub a copy of the latest files will be saved here.
#'
#' @return
#' @export
#'
#' @examples
getTablesInHierarchies <- function(downloadFromGitHub = TRUE,gitHubFileLocation = "https://api.github.com/repos/ices-tools-dev/RDBES/contents/XSD-files", fileLocation){
  
  # For testing
  #downloadFromGitHub = TRUE
  #fileLocation <- './tableDefs/'
  
  # STEP 1) Get the BaseTypes file (if required)
  if (downloadFromGitHub){
    
    myHierarchyFiles <- NULL
    myResponse <- GET(gitHubFileLocation)
    filesOnGitHub <- content(myResponse)
    
    for (myFile in filesOnGitHub){
      myGitHubFile <- data.frame(fileName = myFile$name, downloadURL = myFile$download_url)
      if (is.null(myHierarchyFiles)){
        myHierarchyFiles <- myGitHubFile 
      } else {
        myHierarchyFiles <- rbind(myHierarchyFiles,myGitHubFile)
      }
    }
    # Sub-set to the files we are interested in
    myHierarchyFiles <- myHierarchyFiles[grepl('^H.*xsd$',myHierarchyFiles$fileName),]
    
    print(paste("Downloading ",nrow(myHierarchyFiles), " files from GitHub", sep =""))
    
    # Download our files
    for (i in 1:nrow(myHierarchyFiles)){
      anHierarchyFile <- getURL(myHierarchyFiles[i,'downloadURL'])
      # save the file locally
      writeLines(anHierarchyFile, paste(fileLocation,myHierarchyFiles[i,'fileName'], sep = ""))
    }
    
  }
  
  # Read all the H.*xsd files
  filesToRead <- list.files(path = fileLocation, pattern = "^H.*xsd$", recursive = FALSE, full.names = FALSE)  
  
  
  myHierarchyTables <- list()
  for(fileToParse in filesToRead){
    
    #fileToParse <-"H1.xsd"
    fileToParse <- paste(fileLocation,fileToParse,sep="")
    
    # STEP 2) Parse the XML
    doc <- xmlTreeParse(fileToParse,useInternal= TRUE)
    myXML <- xmlToList(doc)
    
    myResults <- NULL
    hierachyName <- NULL
    
    for (myElement in myXML[names(myXML) == "complexType"]){
      myAttr <- myElement$.attrs
      names(myAttr) <- NULL
      
      if (grepl('^H.*',myAttr)){
        hierachyName <- myAttr
      }
      if (nchar(myAttr)==2 & !grepl('^H.*',myAttr)){
        #print(myElement$.attrs)
        if (is.null(myResults)){
          myResults <- c(myAttr)
        } else {
          myResults <- c(myResults,myAttr)
        }
      }
    }
    
    # Add to our list of results
    myHierarchyTables[[hierachyName]] <- myResults
  }
  
  myHierarchyTables
  
}



#' logValidationError Internal utility function to log errors into an error data frame
#'
#' @param errorList 
#' @param tableName 
#' @param rowID 
#' @param fieldName 
#' @param problemType 
#' @param problemDescription 
#'
#' @return
#'
#' @examples
logValidationError<- function(errorListToAppendTo = NULL,tableName, rowID, fieldName, problemType, problemDescription){
  
  myErrors <- data.frame(tableName = tableName
                         ,rowID = rowID
                         ,fieldName = fieldName
                         ,problemType = problemType
                         ,problemDescription = problemDescription
                         ,stringsAsFactors = FALSE)
  
  if (is.null(errorListToAppendTo)){
    errorListToReturn <- myErrors
  } else {
    errorListToReturn <- rbind(errorListToAppendTo, myErrors)
  }
  
  errorListToReturn
  
}

#' validateTables This function validates your RDBES tables against the ICES RDBES xsd files
#'
#' @param RDBESdata A named list of RDBES tables
#' @param RDBESvalidationdata The validation data derived from BaseTypes.xsd
#' @param RDBEScodeLists The RDBES code lists
#' @param shortOutput Set to TRUE if you want a summarised error output, set to FALSE if you want the full error output
#'
#' @return
#' @export
#'
#' @examples errors <- validateTables(RDBESdata = myRDBESData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE)
validateTables <- function(RDBESdata, RDBESvalidationdata, RDBEScodeLists, shortOutput = FALSE, framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","TE","VD","VS","CL","CE" )){
  
  # For testing
  # RDBESdata <- myRDBESData
  # RDBESvalidationdata <- validationData
  # RDBEScodeLists <- allowedValues
  # shortOutput <- TRUE
  # #framestoValidate <- c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","TE","VD","VS","CL","CE" )
  # framestoValidate <- c("CE")
  
  # To hold the output
  errorList <- data.frame(tableName=character(0)
                          ,rowID = integer(0)
                          ,fieldName=character(0)
                          ,problemType=character(0)
                          ,problemDescription=character(0), stringsAsFactors = FALSE)
  
  # We'll only validate specific tables
  RDBESdata <- RDBESdata[framestoValidate]
  
  # CHECK 1) are we missing any tables?
  newErrors <- validateMissingTables(RDBESdataToCheck=RDBESdata,framesToCheck=framestoValidate)
  errorList <- rbind(errorList,newErrors)
  
  # Remove any NAs from the list of tables (these are the tables we don't have)
  RDBESdata <- RDBESdata[!is.na(names(RDBESdata))]
  
  # CHECK 2) see if we have any SA rows with inconsistent lower hierachy data
  newErrors <- validateLowerHierarchy(RDBESdataToCheck=RDBESdata)
  errorList <- rbind(errorList,newErrors)
  
  # Now we'll check each data frame in our list
  for (dfToCheck in RDBESdata){
    
    # Validate the CS data frame
    newErrors <- validateCSdataFrame(RDBESdataFrameToCheck=dfToCheck,RDBESvalidationdata=RDBESvalidationdata)
    errorList <- rbind(errorList,newErrors)
    
  } 
  
  # If we want shorter output we won't show every error  - just the first of each type
  if (shortOutput){
    fieldsToKeep <- names(errorList)
    # Sort the errors
    errorList <- errorList[order(errorList$tableName, errorList$fieldName, errorList$problemType, errorList$rowID),]
    # Get the row number within a group
    numberedErrorList <- errorList %>% group_by(errorList$tableName, errorList$fieldName, errorList$problemType) %>% mutate(rowNum = row_number()) %>% mutate(maxRowNum = max(rowNum))
    
    # Generate our extra rows that will show there were more errors present
    myExtraRows <- numberedErrorList[numberedErrorList$rowNum == 2,c(fieldsToKeep,'maxRowNum')]
    if(nrow(myExtraRows)>0) {
      countOfOtherRows <- as.numeric(myExtraRows$maxRowNum)
      countOfOtherRows <- countOfOtherRows - 1
      myExtraRows$rowID <- NA
      myExtraRows$problemDescription <- paste(countOfOtherRows," similar error rows removed for clarity", sep = "")
      # Get rid of our maxRowNum field
      myExtraRows <- myExtraRows[,fieldsToKeep]
    }
    
    # Combine the first row within each group with the extra rows
    shortErrorList <- rbind(numberedErrorList[numberedErrorList$rowNum == 1,fieldsToKeep],myExtraRows)
    shortErrorList <- shortErrorList[order(shortErrorList$tableName, shortErrorList$fieldName, shortErrorList$problemType, shortErrorList$rowID),]
    
    errorList <- shortErrorList
    
  }
  
  # Our list of errors
  errorList
  
}


#' validateMissingTables Internal function used by validateTables
#'
#' @param RDBESdataToCheck 
#' @param framesToCheck 
#'
#' @return
#'
#' @examples
validateMissingTables <- function(RDBESdataToCheck,framesToCheck){
  
  errorsToReturn <- NULL
  
  missingTables <-framesToCheck[!framesToCheck %in% names(RDBESdataToCheck)]
  
  if (length(missingTables)>0){
    errorsToReturn <- logValidationError(errorListToAppendTo = NULL
                                         ,tableName = missingTables
                                         ,rowID = NA
                                         ,fieldName = NA
                                         ,problemType = "Missing table check"
                                         ,problemDescription = paste("The following table is missing from your list: ", missingTables, sep = " "))
  } 
  
  errorsToReturn
  
}

#' validateCSdataFrame Internal function used to validate a CS data drame
#'
#' @param RDBESdataFrameToCheck 
#' @param RDBESvalidationdata 
#'
#' @return
#'
#' @examples
validateCSdataFrame <- function(RDBESdataFrameToCheck,RDBESvalidationdata){
  
  errorsToReturn <- NULL
  
  dfToCheck <- RDBESdataFrameToCheck
  
  # Clear out any errors from any earlier checking
  newErrors <- NULL
  
  # Get the field names as a data frame
  myDF <- data.frame(fieldName = names(dfToCheck), stringsAsFactors = FALSE)
  # Join the field names to the field type data frame
  fieldsAndTypes <- left_join(myDF,RDBESvalidationdata,by=c("fieldName" = "name"))
  # Assume the id is always the first field - might be better to check the field names instead
  myIDField <- names(dfToCheck)[[1]]
  myTableName <- substr(myIDField,1,2)
  print(paste("Validating ",myTableName,sep=""))
  
  # Check 2: Check if we are missing any fields
  newErrors <- validateMissingFields(RDBESdataToCheck=dfToCheck,RDBESvalidationdata=RDBESvalidationdata, tableToCheck=myTableName )
  errorsToReturn <- rbind(errorsToReturn,newErrors)
  
  # CHECK 3: See if the fields are in the right order
  newErrors <- validateFieldOrder(RDBESdataToCheck=dfToCheck,RDBESvalidationdata=RDBESvalidationdata, tableToCheck=myTableName )
  errorsToReturn <- rbind(errorsToReturn,newErrors)
  
  # Now we'll check each field in our current data frame
  for (i in 1:length(names(dfToCheck))) {
    #i <- 15
    
    # Get the current field and its validation infromation
    myFieldName <- names(dfToCheck)[[i]]
    #print(myFieldName)
    myFieldType <- fieldsAndTypes[fieldsAndTypes$fieldName == myFieldName,]
    
    
    #Validate the field
    newErrors <- validateCSdataFrameField(fieldToCheck=myFieldName,dataToCheck=dfToCheck,fieldTypeToCheck=myFieldType)
    errorsToReturn <- rbind(errorsToReturn,newErrors)
    
  }
  
  errorsToReturn
  
}

#' validateCSdataFrameField Internal function used to validate a field within a CS data frame
#'
#' @param fieldToCheck 
#' @param dataToCheck 
#' @param fieldTypeToCheck 
#'
#' @return
#'
#' @examples
validateCSdataFrameField <- function(fieldToCheck,dataToCheck,fieldTypeToCheck){
  
  errorsToReturn <- NULL
  
  myFieldName <- fieldToCheck
  dfToCheck <- dataToCheck
  myFieldType <- fieldTypeToCheck
  
  ## Check 3) NA values
  newErrors <- validateNAvalues(fieldToCheck=myFieldName,dataToCheck=dfToCheck,fieldTypeToCheck=myFieldType)
  errorsToReturn <- rbind(errorsToReturn,newErrors)
  
  # Now that we have passed the NA check point lets get rid of any NA values so 
  # we don't need to worry about them again
  dfToCheckNotNA <- dfToCheck[!is.na(dfToCheck[,myFieldName]),]
  
  # Check 4 Do we know what type this field should be?
  # If not, there's nothing much else we can do so skip to the end and log the error
  
  # Check if we know what types this field should have
  if (!is.na(myFieldType$type)){
    myType <- myFieldType$type
    
    # We only want to carry on with further checks if we actually have some non-NA rows  
    # otherwise we can get validation errors where a field in the data frame is technically the wrong data type 
    # but if there is no data in the field the online RDBES validator won't know or care about that....
    if (nrow(dfToCheckNotNA) >0) {
      
      # IF simple type
      if (length(grep("xs:",myType)) > 0) {
        
        # CHeck 5 For simple data types we'll see if we have the right format data
        newErrors <- validateSimpleTypes(fieldToCheck=myFieldName,dataToCheck=dfToCheckNotNA,fieldTypeToCheck=myFieldType)
        errorsToReturn <- rbind(errorsToReturn,newErrors)
        
        # ELSE code list
      } else {
        
        # Check 6 If we're dealing with code lists we need to see if we have allowed values
        newErrors <- validateAgainstCodeList(fieldToCheck=myFieldName,dataToCheck=dfToCheckNotNA,fieldTypeToCheck=myFieldType, codeLists = allowedValues)
        errorsToReturn <- rbind(errorsToReturn,newErrors)
        
      } # ENDIF simple type / code list 
    } # No non-NA rows
    
    # ELSE could not find validation information on this field so log an error
  } else {
    
    # id and recordType fields don't have validation information so don't bother recording an error for those types of fields
    if (length(grep("^..id$",myFieldName)) == 0 & length(grep("^..recordType$",myFieldName)) == 0){
      newErrors <- logValidationError(errorListToAppendTo = NULL
                                      ,tableName = substr(myFieldName,1,2)
                                      ,rowID = NA
                                      ,fieldName = myFieldName
                                      ,problemType = "Code list missing"
                                      ,problemDescription = paste("Could not find validation information for ", myFieldName, sep = " "))
      errorsToReturn <- rbind(errorsToReturn,newErrors)
    }
  } #EndIF validation infromation exists / does not exist
  
  errorsToReturn
  
}



#' validateMissingFields Internal function used by validateTables
#'
#' @param RDBESdataToCheck 
#' @param RDBESvalidationdata 
#' @param tableToCheck 
#'
#' @return
#'
#' @examples
validateMissingFields <- function(RDBESdataToCheck,RDBESvalidationdata,tableToCheck){
  
  errorsToReturn <- NULL
  
  # Get the field names as a data frame
  myDFNames <- data.frame(fieldName = names(RDBESdataToCheck), stringsAsFactors = FALSE)
  
  missingFieldsCheck <- RDBESvalidationdata
  missingFieldsCheck$table <- substr(missingFieldsCheck$name,1,2)
  missingFieldsCheck <- missingFieldsCheck[missingFieldsCheck$table==tableToCheck,]
  missingFields <- missingFieldsCheck[!missingFieldsCheck$name %in% myDFNames$fieldName,]
  # If we have some missing fields lets log them as an error
  if (nrow(missingFields) > 0){
    #Log the error
    errorsToReturn <- logValidationError(errorListToAppendTo = NULL
                                         ,tableName = tableToCheck
                                         ,rowID = NA
                                         ,fieldName = missingFields$name
                                         ,problemType = "Missing field check"
                                         ,problemDescription = paste("The following field is missing from your data frames: ", missingFields$name, sep = " "))
  } 
  
  errorsToReturn
  
}



#' validateFieldOrder Internal function used by validateTables
#'
#' @param RDBESdataToCheck 
#' @param RDBESvalidationdata 
#' @param tableToCheck 
#'
#' @return
#'
#' @examples
validateFieldOrder <- function(RDBESdataToCheck,RDBESvalidationdata,tableToCheck){
  
  errorsToReturn <- NULL
  
  # Get the field names as a data frame
  myDFNames <- data.frame(fieldName = names(RDBESdataToCheck), stringsAsFactors = FALSE)
  
  missingFieldsCheck <- RDBESvalidationdata
  missingFieldsCheck$table <- substr(missingFieldsCheck$name,1,2)
  missingFieldsCheck <- missingFieldsCheck[missingFieldsCheck$table==tableToCheck,]
  
  # Remove the XXid and XXrecordType fields before checking - these aren't uploaded
  #myDFNamesToCheck <- myDF[!grepl("^..id$",myDF$fieldName) & !grepl("^..recordType$",myDF$fieldName),]
  myDFNamesToCheck <- myDFNames[!grepl("^..id$",myDFNames$fieldName) & !grepl("^..recordType$",myDFNames$fieldName),]
  
  # If the values aren't the same (including the same order) then log an error
  if (!identical(toupper(missingFieldsCheck$name),toupper(myDFNamesToCheck))){
    #Log the error
    errorsToReturn <- logValidationError(errorListToAppendTo = NULL
                                         ,tableName = tableToCheck
                                         ,rowID = NA
                                         ,fieldName = NA
                                         ,problemType = "Field order check"
                                         ,problemDescription = paste("The fields are not in the correct order: ",paste(myDFNamesToCheck,collapse=", "),sep=""))
  } 
  
  errorsToReturn
  
}

#' validateLowerHierarchy Internal function to check if the values of SAlowerHierarchy are consistent with the FM/BV data available
#'
#' @param RDBESdataToCheck 
#'
#' @return
#' @export
#'
#' @examples
validateLowerHierarchy <- function(RDBESdataToCheck){
  
  # For testing
  #RDBESdataToCheck <- myRDBESData
  
  errorsToReturn <- NULL
  
  # We need to check the SA, FM, and BV tables 
  SAdataToCheck <- RDBESdataToCheck[['SA']]
  FMdataToCheck <- RDBESdataToCheck[['FM']]
  BVdataToCheck <- RDBESdataToCheck[['BV']]
  
  if (is.null(SAdataToCheck) | is.null(FMdataToCheck) | is.null(BVdataToCheck)){
    print("Either SA, or FM, or BV don't exist in the data to validate - so we are not running the validateLowerHierarchy check")
  } 
  # Check the lower hierarchy sample
  else {
    
    # Split data by lower hierarchy
    lowerA <- SAdataToCheck[SAdataToCheck$SAlowerHierarchy == 'A',]
    lowerB <- SAdataToCheck[SAdataToCheck$SAlowerHierarchy == 'B',]
    lowerC <- SAdataToCheck[SAdataToCheck$SAlowerHierarchy == 'C',]
    lowerD <- SAdataToCheck[SAdataToCheck$SAlowerHierarchy == 'D',]
    
    # A: Check that samples with lower hierarchy B only have FM rows associated with them
    if (nrow(lowerA)>0){
      # If there are any A SA rows WITH links to BV OR WITHOUT links to FM
      if (nrow(lowerA[lowerA$SAid %in% BVdataToCheck$SAid,])> 0 | nrow(lowerA[!lowerA$SAid %in% FMdataToCheck$SAid,])> 0){
        
        problemSAFM <- lowerA[!lowerA$SAid %in% FMdataToCheck$SAid,]
        problemSABV <- lowerA[lowerA$SAid %in% BVdataToCheck$SAid,]
        problemSA <- rbind(problemSAFM,problemSABV)
        
        someErrors <- logValidationError(errorListToAppendTo = NULL
                                         ,tableName = 'SA'
                                         ,rowID = problemSA[,1]
                                         ,fieldName = 'SAlowerHierarchy'
                                         ,problemType = "Lower hierarchy A check"
                                         ,problemDescription = 'Sample is either incorrectly linked to BV or not linked to FM')
        errorsToReturn <- rbind(errorsToReturn,someErrors)
      }
    }
    
    
    # B: Check that samples with lower hierarchy B only have FM rows associated with them
    if (nrow(lowerB)>0){
      # If there are any B SA rows WITH links to BV OR WITHOUT links to FM
      if (nrow(lowerB[lowerB$SAid %in% BVdataToCheck$SAid,])> 0 | nrow(lowerB[!lowerB$SAid %in% FMdataToCheck$SAid,])> 0){
        
        problemSAFM <- lowerB[!lowerB$SAid %in% FMdataToCheck$SAid,]
        problemSABV <- lowerB[lowerB$SAid %in% BVdataToCheck$SAid,]
        problemSA <- rbind(problemSAFM,problemSABV)
        
        someErrors <- logValidationError(errorListToAppendTo = NULL
                                         ,tableName = 'SA'
                                         ,rowID = problemSA[,1]
                                         ,fieldName = 'SAlowerHierarchy'
                                         ,problemType = "Lower hierarchy B check"
                                         ,problemDescription = 'Sample is either incorrectly linked to BV or not linked to FM')
        errorsToReturn <- rbind(errorsToReturn,someErrors)
      }
    }
    
    
    # C: Check that samples with lower hierarchy C only have BV rows associated with them
    if (nrow(lowerC)>0){
      # If there are any C SA rows WITHOUT links to BV OR WITH links to FM
      if (nrow(lowerC[!lowerC$SAid %in% BVdataToCheck$SAid,])> 0 | nrow(lowerC[lowerC$SAid %in% FMdataToCheck$SAid,])> 0){
        
        problemSAFM <- lowerC[lowerC$SAid %in% FMdataToCheck$SAid,]
        problemSABV <- lowerC[!lowerC$SAid %in% BVdataToCheck$SAid,]
        problemSA <- rbind(problemSAFM,problemSABV)
        
        someErrors <- logValidationError(errorListToAppendTo = NULL
                                         ,tableName = 'SA'
                                         ,rowID = problemSA[,1]
                                         ,fieldName = 'SAlowerHierarchy'
                                         ,problemType = "Lower hierarchy C check"
                                         ,problemDescription = 'Sample is either not linked to BV or incorrectly linked to FM')
        errorsToReturn <- rbind(errorsToReturn,someErrors)
      }
    }
    
    
    # D: Check that samples with lower hierarchy D have no FM or BV rows assoicated with them
    if (nrow(lowerD)>0){
      if (nrow(lowerD[lowerD$SAid %in% BVdataToCheck$SAid,])> 0 | nrow(lowerD[lowerD$SAid %in% FMdataToCheck$SAid,])> 0){
        
        problemSAFM <- lowerD[lowerD$SAid %in% FMdataToCheck$SAid,]
        problemSABV <- lowerD[lowerD$SAid %in% BVdataToCheck$SAid,]
        problemSA <- rbind(problemSAFM,problemSABV)
        
        someErrors <- logValidationError(errorListToAppendTo = NULL
                                         ,tableName = 'SA'
                                         ,rowID = problemSA[,1]
                                         ,fieldName = 'SAlowerHierarchy'
                                         ,problemType = "Lower hierarchy D check"
                                         ,problemDescription = 'Sample is linked to either FM or BV data')
        errorsToReturn <- rbind(errorsToReturn,someErrors)
      }
    }
    
  }
  
  errorsToReturn
  
  
}

#' validateNAvalues Internal function used by validateTables
#'
#' @param fieldToCheck 
#' @param dataToCheck 
#' @param fieldTypeToCheck 
#'
#' @return
#'
#' @examples
validateNAvalues <- function(fieldToCheck,dataToCheck,fieldTypeToCheck){
  
  errorsToReturn <- NULL
  
  #myFieldType <- fieldsAndTypesData[fieldsAndTypesData$fieldName == fieldToCheck,]
  myValuesNA <- dataToCheck[is.na(dataToCheck[,fieldToCheck]),]
  # If we have NA values check if thats ok
  if (nrow(myValuesNA)>0){
    # if we are not allowed to have empty values here then log an error
    if (ifelse(is.na(fieldTypeToCheck$min),0,fieldTypeToCheck$min) >0){
      #Log the error
      errorsToReturn <- logValidationError(errorListToAppendTo = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = myValuesNA[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "Null value check"
                                           ,problemDescription = paste("Null value problem;",paste(substr(fieldToCheck,1,2),"id", sep=""),":", myValuesNA[,1], " ;Column:",fieldToCheck, ";Unallowed value:",myValuesNA[,fieldToCheck], sep = " "))
    }
  } # Endif NA check
  
  errorsToReturn  
}



#' validateSimpleTypes Internal function used by validateTables
#'
#' @param fieldToCheck 
#' @param dataToCheck 
#' @param fieldTypeToCheck 
#'
#' @return
#'
#' @examples
validateSimpleTypes <- function(fieldToCheck,dataToCheck,fieldTypeToCheck){
  
  #fieldToCheck<-myFieldName
  #dataToCheck<-dfToCheckNotNA
  #fieldTypeToCheck<-myFieldType
  
  # TODO - this methd has got a bit long and shoudl be re-factored for clarity
  
  errorsToReturn <- NULL
  
  myTypeToCheck <- fieldTypeToCheck$type
  #print(fieldTypeToCheck)
  
  # Do we have any simpleTypeChecks defined for this field?
  simpleTypeCheckDefined <- FALSE
  if (!is.na(fieldTypeToCheck$description)){
    if (fieldTypeToCheck$description == "simpleTypeCheck"){
      simpleTypeCheckDefined <- TRUE
    } 
  }
  
  # See if we need to do any range checks first
  if (simpleTypeCheckDefined){
    myNumericValues <- dataToCheck[is.numeric(dataToCheck[,fieldToCheck]),]
    myNumericValues <- myNumericValues[!is.na(myNumericValues[,fieldToCheck]),]
    
    # Range checks are only relvent for numeric values
    if (nrow(myNumericValues)>0){
      
      # If min value check
      if(!is.na(fieldTypeToCheck$minValue)){
        minValueNumber <- as.numeric(fieldTypeToCheck$minValue)
        valueTooSmall <- myNumericValues[myNumericValues[,fieldToCheck] < minValueNumber,]
        # If we have soem values that are too small - log an error
        if (nrow(valueTooSmall)){
          #Log the error
          someErrors <- logValidationError(errorListToAppendTo = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = valueTooSmall[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "Min value check"
                                           ,problemDescription = paste("Value is below minimum allowed (",minValueNumber,");",paste(substr(fieldToCheck,1,2),"id", sep=""),":", valueTooSmall[,1], " ;Column:",fieldToCheck, ";Unallowed value:",valueTooSmall[,fieldToCheck], sep = " "))
          errorsToReturn <- rbind(errorsToReturn,someErrors)
        }
      } 
      # Max value check
      if (!is.na(fieldTypeToCheck$maxValue)) {
        maxValueNumber <- as.numeric(fieldTypeToCheck$maxValue)
        valueTooBig <- myNumericValues[myNumericValues[,fieldToCheck] > maxValueNumber,]
        # If we have soem values that are too big - log an error
        if (nrow(valueTooBig)){
          #Log the error
          someErrors <- logValidationError(errorListToAppendTo = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = valueTooBig[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "Max value check"
                                           ,problemDescription = paste("Value is above maximum allowed (",maxValueNumber,");",paste(substr(fieldToCheck,1,2),"id", sep=""),":", valueTooBig[,1], " ;Column:",fieldToCheck, ";Unallowed value:",valueTooBig[,fieldToCheck], sep = " "))
          errorsToReturn <- rbind(errorsToReturn,someErrors)
        }
      }
    }
  }
  
  
  ## Now check the actual data type 
  # Ints
  if (myTypeToCheck == "xs:int"){
    # Check for any non integer values
    #myNonIntValues <- dataToCheck[!is.integer(dataToCheck[,fieldToCheck]),]
    # The above function only checks whether the value is stored as an integer, not whether it is an integer :-S so need to do this instead
    myNonIntValues <- dataToCheck[!as.numeric(dataToCheck[,fieldToCheck])%%1==0,]
    if (nrow(myNonIntValues)>0){
      #Log the error
      someErrors <- logValidationError(errorListToAppendTo = NULL
                                       ,tableName = substr(fieldToCheck,1,2)
                                       ,rowID = myNonIntValues[,1]
                                       ,fieldName = fieldToCheck
                                       ,problemType = "Data type check"
                                       ,problemDescription = paste("Data type problem (int);",paste(substr(fieldToCheck,1,2),"id", sep=""),":", myNonIntValues[,1], " ;Column:",fieldToCheck, ";Unallowed value:",myNonIntValues[,fieldToCheck], sep = " "))
      errorsToReturn <- rbind(errorsToReturn,someErrors)
    }
    # Decimal
  } else if (myTypeToCheck == "xs:decimal" | myTypeToCheck == "xs:long"){
    # Check for any non numeric values
    #myNonDecValues <- dataToCheck[!is.numeric(dataToCheck[,fieldToCheck]),]
    # The above doesn't interpret numbers stored as text as numeric so I have changed it to the following
    myNonDecValues <- dataToCheck[!(!is.na(as.numeric(dataToCheck[,fieldToCheck])) & is.numeric(as.numeric(dataToCheck[,fieldToCheck])) ),] 
    if (nrow(myNonDecValues)>0){
      #Log the error
      someErrors <- logValidationError(errorListToAppendTo = NULL
                                       ,tableName = substr(fieldToCheck,1,2)
                                       ,rowID = myNonDecValues[,1]
                                       ,fieldName = fieldToCheck
                                       ,problemType = "Data type check"
                                       ,problemDescription = paste("Data type problem (decimal/long);",paste(substr(fieldToCheck,1,2),"id", sep=""),":", myNonDecValues[,1], " ;Column:",fieldToCheck, ";Unallowed value:",myNonDecValues[,fieldToCheck], sep = " "))
      errorsToReturn <- rbind(errorsToReturn,someErrors)
    }
    # Now see if we need to do any simpleTypeChecks on our decimal (e.g. precision)
    if (simpleTypeCheckDefined){
      
      # If Precision check
      if(!is.na(fieldTypeToCheck$fractionDigits)){
        
        # Convert values to strings and see which numbers have decimal places
        myDecValues <- dataToCheck[is.numeric(dataToCheck[,fieldToCheck]),]
        dataWithDPs <- myDecValues[regexpr('.', as.character(myDecValues[,fieldToCheck]), fixed = TRUE) > 0,]
        # See how many characters are found after the decimal place
        if (nrow(dataWithDPs)>0){
          dataWithDPs$numberOfDps <- nchar(substr(as.character(dataWithDPs[,fieldToCheck]), regexpr('.', as.character(dataWithDPs[,fieldToCheck]), fixed = TRUE) + 1, nchar(as.character(dataWithDPs[,fieldToCheck]))))
          dataWithDPs <- dataWithDPs[!is.na(dataWithDPs$numberOfDps),]
          dataWithTooManyDps <- dataWithDPs[dataWithDPs$numberOfDps > as.numeric(fieldTypeToCheck$fractionDigits),]
          # If we have too many numbers after the decimal place
          if (nrow(dataWithTooManyDps)>0){
            #Log the error
            someErrors <- logValidationError(errorListToAppendTo = NULL
                                             ,tableName = substr(fieldToCheck,1,2)
                                             ,rowID = dataWithTooManyDps[,1]
                                             ,fieldName = fieldToCheck
                                             ,problemType = "Precision check"
                                             ,problemDescription = paste("Decimal precision problem (",fieldTypeToCheck$fractionDigits,"decimal places allowed);",paste(substr(fieldToCheck,1,2),"id", sep=""),":", dataWithTooManyDps[,1], " ;Column:",fieldToCheck, ";Unallowed value:",dataWithTooManyDps[,fieldToCheck], sep = " "))
            errorsToReturn <- rbind(errorsToReturn,someErrors)
          }
        }
      }
    }  
    
    
    # String
  } else if (myTypeToCheck == "xs:string"){
    
    # We only need to check strings if there simpleTypeChecks defined
    if (simpleTypeCheckDefined){
      # Length check
      if (!is.na(fieldTypeToCheck$length)) {
        
        maxlengthNumber <- as.numeric(fieldTypeToCheck$length)
        myStringDataToCheck <- dataToCheck[!is.na(dataToCheck[,fieldToCheck]),]
        stringTooLong <- myStringDataToCheck[nchar(myStringDataToCheck[,fieldToCheck]) > maxlengthNumber,]
        # If we have soem strings that are too long 
        if (nrow(stringTooLong)){
          #Log the error
          someErrors <- logValidationError(errorListToAppendTo = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = stringTooLong[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "String length check"
                                           ,problemDescription = paste("String is longer than maximum allowed (",maxlengthNumber,");",paste(substr(fieldToCheck,1,2),"id", sep=""),":", stringTooLong[,1], " ;Column:",fieldToCheck, ";Unallowed value:",stringTooLong[,fieldToCheck], sep = " "))
          errorsToReturn <- rbind(errorsToReturn,someErrors)
        }
      }
      
      # String Pattern check
      if (!is.na(fieldTypeToCheck$pattern)) {
        
        myStringDataToCheck <- dataToCheck[!is.na(dataToCheck[,fieldToCheck]),]
        # Check the field agaisnt the pattern using grepl
        patternNotMatched <- myStringDataToCheck[!grepl(fieldTypeToCheck$pattern,myStringDataToCheck[,fieldToCheck]),]
        
        # If we have soem strings that don't match the pattern 
        if (nrow(patternNotMatched)){
          #Log the error
          someErrors <- logValidationError(errorListToAppendTo = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = patternNotMatched[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "String pattern check"
                                           ,problemDescription = paste("String does no match required pattern (",fieldTypeToCheck$pattern,");",paste(substr(fieldToCheck,1,2),"id", sep=""),":", patternNotMatched[,1], " ;Column:",fieldToCheck, ";Unallowed value:",patternNotMatched[,fieldToCheck], sep = " "))
          errorsToReturn <- rbind(errorsToReturn,someErrors)
        }
        
      }
      
    }
  }
  
  errorsToReturn
  
}


#' validateAgainstCodeList Internal function used by validateTables
#'
#' @param fieldToCheck 
#' @param dataToCheck 
#' @param fieldTypeToCheck 
#' @param codeLists 
#'
#' @return
#'
#' @examples
validateAgainstCodeList <- function(fieldToCheck,dataToCheck,fieldTypeToCheck,codeLists){
  
  errorsToReturn <- NULL
  
  myTypeToCheck <- fieldTypeToCheck$type
  
  # see if we can find the correct code list
  myAllowedValues <- codeLists[codeLists$listName == myTypeToCheck,"allowedValues"]
  # If we found which values are allowed then we can check our data against them
  if (length(myAllowedValues)>0){
    # Check if our values are in the allowed list of values
    myResults <- dataToCheck[!dataToCheck[,fieldToCheck] %in% myAllowedValues,]
    # If we have soem values that aren't in the allowed list flag them as errors
    if (nrow(myResults)>0){
      #Log the error
      errorsToReturn <- logValidationError(errorListToAppendTo = NULL
                                           ,tableName = substr(fieldToCheck,1,2)
                                           ,rowID = myResults[,1]
                                           ,fieldName = fieldToCheck
                                           ,problemType = "Code list problem"
                                           ,problemDescription = paste("Code list problem;",paste(substr(fieldToCheck,1,2),"id",sep=""),":", myResults[,1], " ;Column:",fieldToCheck, ";Unallowed value:",myResults[,fieldToCheck], ";Code list name:",myTypeToCheck, sep = " "))
    }
    # ELSE if we didn't find a list of allowed values then log that as an error
  } else {
    #Log the error
    errorsToReturn <- logValidationError(errorListToAppendTo = NULL
                                         ,tableName = substr(fieldToCheck,1,2)
                                         ,rowID = NA
                                         ,fieldName = fieldToCheck
                                         ,problemType = "Missing code list"
                                         ,problemDescription = paste("Could not find code list", myTypeToCheck, " for ", fieldToCheck, sep = " "))
  } # ENDIF find allowed values
  
  errorsToReturn
  
}





#' removeInvalidRows
#'
#' @param tableName The name of the table e.g. 'DE'
#' @param dataToClean The data frame we wish to clean
#' @param errorList The list of errors produced by the validation function
#'
#' @return
#' @export
#'
#' @examples
removeInvalidRows <- function(tableName,dataToClean,errorList ){
  
  # Remove any invalid rows 
  invalidRows <- unique(errorList[errorList$tableName == tableName & !is.na(errorList$rowID),"rowID"])
  # Assume the first column is always the ID field
  dataToClean <- dataToClean[!dataToClean[,1] %in% invalidRows,]
  
  dataToClean
}

#' Load the reference data we need for validation
#'
#' @param downloadFromICES Set to TRUE if you wish to download the latest vocabulary data from ICES, set to FALSE if you just want to use a local file.  When you download the data from ICES it is also saved locally.
#'
#' @return
#' @export
#'
#' @examples
loadReferenceData <- function(downloadFromICES = TRUE, validationData = NULL)
{
  
  if (downloadFromICES){
    
    # Download from the ICES vocabulary server
    codeListsToRefresh <- unique(validationData[grep('xs:*', validationData$type, invert = TRUE),'type'])
    codeListsToRefresh <- sub('.', '', codeListsToRefresh)
    allowedValues <- refreshReferenceDataFromICES(codeListsToRefresh)
    
    # save to file so we don't need to download from ICES every time
    saveRDS(allowedValues, file="referenceData/allowedValues.RDS")
    
  } else {
    # Just load from file
    allowedValues <- readRDS(file="./referenceData/allowedValues.RDS")
  }
  
  allowedValues
}


#' refreshReferenceDataFromICES Downloads the required reference data from ICES
#'
#' @return
#' @export
#'
#' @examples
refreshReferenceDataFromICES <- function(codeListsToRefresh){
  
  #codeListsToRefresh <- unique(validationData[grep('xs:*', validationData$type, invert = TRUE),'type'])
  # We want to remove the first 't' from the list names
  #View(codeListsToRefresh)
  #codeListsToRefresh <- sub('.', '', codeListsToRefresh)
  
  
  # extracts list types from ICES vocabulary server
  codeTypes <- getCodeTypeList()
  
  # pick out the list types that we need
  target_codeTypes <- codeTypes[codeTypes$Key %in% codeListsToRefresh,'Key']
  
  # Create an empty list
  codeLists<-sapply(target_codeTypes,function(x) NULL)
  
  # Get all the reference data we need
  for (i in target_codeTypes){
    print(i)
    codeLists[[i]]<-getCodeList(i)
    codeLists[[i]]$listName <- paste("t", i, sep = "")
    codeLists[[i]]$fileName <- "icesVocab"
    codeLists[[i]]$allowedValues <- codeLists[[i]]$Key
  }
  
  # Put the list entries into a single data frame
  allowedValues <- do.call("rbind", codeLists)
  
  allowedValues
  
}


readComplexExchangeFile <- function(typeOfFile,RDBESvalidationdata,nameOfFile,RequiredTables){
  
  # TODO - this function needs more testing
  
  # For testing
  # typeOfFile <- 'H1'
  # RDBESvalidationdata <- validationData
  # RequiredTables <- requiredTables
  # nameOfFile <- 'output/IE_2019_H1.csv'
  
  testedCSfileTypes <- c('H1','H5')
  
  if (!typeOfFile %in% testedCSfileTypes){
    warning(paste("Method not tested for ",typeOfFile, " yet", sep =""))
  }
  
  # Find which tables we need for this file type
  upperHierarchy <- substr(typeOfFile,2,nchar(typeOfFile))
  myRequiredTables <- RequiredTables[[typeOfFile]]
  
  # Create a list with all the empty tables we need in it
  myDataList <- list()
  # Also create a list to hold all our new data rows 
  myNewDataList <- list()
  
  for (myTable in myRequiredTables){
    myEmptyDf <- createEmptyDataFrameFromValidationData(nameOfTable = myTable, RDBESvalidationdata = RDBESvalidationdata)
    # Add our empty XXid column to the front of the data frame
    myID <- integer(0)
    myEmptyDf <- cbind(myID, myEmptyDf)
    # Name our new id column correctly
    names(myEmptyDf)[1] <- paste(myTable,'id',sep = "")
    # Add the empty data frame to our list
    myDataList[[myTable]] <- myEmptyDf
    # Create an empty list to hold our new data
    myNewDataList[[myTable]] <- list()
  }
  
  # Read our csv file 
  myLines <- readLines(nameOfFile)
  currentRowType <- NA
  previousRowType <- NA
  myNewRowID <- -1
  myPreviousRowID <- -1
  myForeignKeyID <- -1
  myForeignKeyName <- NA
  
  
  
  # Used for tracking and assigning foreign keys 
  foreignKeyTrackingList <- list()
  
  # store the most recent ids for each table type - needed for the foreign key logic
  mostRecentIds <- list()
  for (reqTable in myRequiredTables){
    mostRecentIds[[reqTable]] <- -1
  }
  
  # The approach that follows is to read in each line of the file and store the data and foreign key information seperately - we then add the foreign keys to the data at the end
  
  if (length(myLines)>10000) {
    print(paste("The file is ", length(myLines), " lines long so this might take a few minutes - why not have a cup of tea while you wait...",sep=""))
  }
  
  # For each line in the file
  for (i in 1:length(myLines)){
    
    # Update the user on how far we have got - this slows down the processing so I have removed it
    # if ((i %% 5000)==0){
    #   print(paste("Processing line number ",i,sep=""))
    # }
    
    # Store the previous row details
    previousRowType <- ifelse(is.na(currentRowType),NA,currentRowType)
    myPreviousRowID <- ifelse(myNewRowID == -1,-1,myNewRowID)
    
    # Read the next line
    myRow <- myLines[[i]]
    myRowValues <- strsplit(myRow,split=",")[[1]]
    # Replace empty strings with NAs
    myRowValues[myRowValues == ""] <- NA
    currentRowType <- myRowValues[1]
    # Create a new row id - we'll just use sequential primary key - find out how many rows there are already are and add 1
    myNewRowID <-length(myNewDataList[[currentRowType]])+1
    
    
    # Update the most recent row id for this record type
    mostRecentIds[[currentRowType]] <- myNewRowID
    # We also want to clear out any ids that are saved in this table but that are lower down in the hierarchy - otherwise we might get in a mess, particualrly if there are optional tables in some of the hierarchies
    currentRowTypePositionInVector <- grep(currentRowType,myRequiredTables)
    if (currentRowTypePositionInVector < length(myRequiredTables)){
      mostRecentIds[myRequiredTables[currentRowTypePositionInVector+1:length(myRequiredTables)]]<- -1
    }
    
    
    # Work out what the foreign key shoudl be
    if (currentRowType == 'DE'){
      # No foreign key for DE
      myForeignKeyID <- 1
      myForeignKeyName <- NA
    } else if (currentRowType ==  previousRowType){
      # If we are still on the same row type then we'll just keep the same foreign key detaisl that we already have - no need to do anything here 
      
    } else if (grep(currentRowType,myRequiredTables) > grep(previousRowType,myRequiredTables)) {
      # If we've moved to a row type lower in the hierarchy then we just need to set the new foreign key values
      myForeignKeyID <- myPreviousRowID
      myForeignKeyName <- paste(previousRowType,'id',sep="")
    } else {
      # else we're going back up the hierarchy - trickier...we need to work out which value to use for the foreign key from the mostRecentIds data frame
      
      tableFound <- NA
      
      # Check the tables earlier in the hierarchy and find the lowest one that has an id already recorded - we'll use this for our foreign key
      currentRowTypePositionInVector <- grep(currentRowType,myRequiredTables)
      for (j in (currentRowTypePositionInVector-1):1){
        tableToCheck <- myRequiredTables[j]
        # if this table type exists in the data frame of the most recent keys  and has a value > -1 then we'll use it
        if (!is.null(mostRecentIds[[tableToCheck]] &  mostRecentIds[[tableToCheck]]>-1 )){
          tableFound <- tableToCheck
          break;
        }
      }
      # If we found a mtach we'll use those values for the foreign key
      if (!is.na(tableFound)){
        myForeignKeyID <- mostRecentIds[[tableFound]]
        myForeignKeyName <- paste(tableFound,'id',sep="")
      } else {
        # Soemthing has gone wrong
        # just reset the values for now
        myForeignKeyID <- -1
        myForeignKeyName <- NA
      }
      
    }
    
    # Used for tracking and assigning foreign keys 
    foreignKeyTrackingList[[i]] <- list(currentRowID = myNewRowID, currentRowType = currentRowType,previousRowID = myPreviousRowID, previousRowType = previousRowType ,foreignKeyID = myForeignKeyID, foreignKeyType = myForeignKeyName)
    
    # Our new data
    myNewData <- myRowValues[2:length(myRowValues)]
    
    # Save the names because rbind screws them up
    correctNames <- names(myDataList[[currentRowType]])
    
    # Add the PK to our new data 
    myNewData <- c(myNewRowID,myNewData)
    
    # If the last value in the exchange file row is blank then readLines doesn't pick it up
    # to solve this we'll add a blank value if required
    #if (length(correctNames[!grepl("^..id$",correctNames)])==length(myNewData)+1){
    if (length(correctNames)==length(myNewData)+1){
      #print(correctNames)
      #print(myNewData)
      # Add a blank last value if required
      myNewData <- c(myNewData,NA)
    }
    
    # Fix the names
    names(myNewData) <- correctNames
    
    # Add the row to our data
    myNewDataList[[currentRowType]][[i]] <- myNewData
    
  }
  
  # Now process all the data we have read in
  for (myTable in myRequiredTables){
    # If we have soem new data for this tabel type then deal with it...
    if (length(myNewDataList[[myTable]])){
      # Get the list of data we want
      mynewDataListdf <-  myNewDataList[[myTable]]
      # Get rid of emptry values in the list
      mynewDataListdf <- mynewDataListdf[lapply(mynewDataListdf,length)>0]
      # Combine our list of new data entires and ensure the first column is an int
      mynewDataListdf <- do.call("rbind", mynewDataListdf)
      mynewDataListdf <- as.data.frame(mynewDataListdf,stringsAsFactors = FALSE)
      #mynewDataListdf <- do.call("rbind", myNewDataList[[myTable]])
      mynewDataListdf[,1]<-as.integer(mynewDataListdf[,1])
      myDataList[[myTable]] <- mynewDataListdf
      #myDataList[[myTable]] <- do.call("rbind", myNewDataList[[myTable]])
    }
  }
  
  # Now we have the data but we still need to append the foreign keys
  
  #View(foreignKeyTrackingList)
  # First we need to turn our list of new foreign keys into a data frame
  foreignKeyTracking <- do.call("rbind", foreignKeyTrackingList)
  foreignKeyTracking <- as.data.frame(foreignKeyTracking,stringsAsFactors = FALSE)
  # doing the above leaves all the columns as lists - don't knwo why - didn't seem to be a problem when I did the same to mynewDataListdf' in the code above - any I'll fix this now 
  foreignKeyTracking$currentRowType <- unlist(foreignKeyTracking$currentRowType)
  foreignKeyTracking$previousRowType <- unlist(foreignKeyTracking$previousRowType)
  foreignKeyTracking$foreignKeyType <- unlist(foreignKeyTracking$foreignKeyType)
  
  # Ensure our columns are the correct data types
  foreignKeyTracking$currentRowID <- as.integer(foreignKeyTracking$currentRowID)
  foreignKeyTracking$previousRowID <- as.integer(foreignKeyTracking$previousRowID)
  foreignKeyTracking$foreignKeyID <- as.integer(foreignKeyTracking$foreignKeyID)
  
  # See which FK columns we need to add
  fkColumnsToAdd <- unique(foreignKeyTracking[!is.na(foreignKeyTracking$foreignKeyType),c("currentRowType","foreignKeyType")])
  
  # Add the FK data
  for (i in 1:nrow(fkColumnsToAdd)){
    
    # first we'll add the new columns required
    myColumnToAdd <- fkColumnsToAdd[i,]
    myDataList[[myColumnToAdd$currentRowType]][,myColumnToAdd$foreignKeyType] <- integer(nrow(myDataList[[myColumnToAdd$currentRowType]]))
    # default FK column to NA
    if (nrow(myDataList[[myColumnToAdd$currentRowType]]) >0) {
      myDataList[[myColumnToAdd$currentRowType]][,myColumnToAdd$foreignKeyType] <- NA
    } 
    
    # Now we need to add the actual values for the FKs
    # Get the FK values
    fkValuesToAdd <- foreignKeyTracking[!is.na(foreignKeyTracking$foreignKeyType) &  foreignKeyTracking$currentRowType == myColumnToAdd$currentRowType & foreignKeyTracking$foreignKeyType == myColumnToAdd$foreignKeyType,]
    
    #myDataList[[myColumnToAdd$currentRowType]][,paste(myColumnToAdd$currentRowType,'id',sep="")]
    
    # Left join our data with the FK data - need to use "setNames" to specify the columns to join on because 1 of them is defined using a variable
    mergedFKData <- left_join(myDataList[[myColumnToAdd$currentRowType]],fkValuesToAdd,by=setNames("currentRowID", paste(myColumnToAdd$currentRowType,'id',sep="")))
    
    # Now update the relevent column with the FK value
    mergedFKData[,myColumnToAdd$foreignKeyType] <- mergedFKData$foreignKeyID
    
    # Get rid of the columns we don't need
    colstoRemove <- c("currentRowType","previousRowID","previousRowType","foreignKeyID","foreignKeyType")
    dataWithFK <- select(mergedFKData,-all_of(colstoRemove))
    
    # Update our data list with the modified data
    myDataList[[myColumnToAdd$currentRowType]] <- dataWithFK
  }
  
  # return our data
  myDataList
}

readExchangeFile <- function(RDBESvalidationdata,nameOfFile,RequiredTables=NULL){
  
  # For testing
  #nameOfFile <- 'output/IE_2019_H11.csv'
  
  myNameOfTable <- NA
  
  if (grepl('.*HCE.csv',nameOfFile)){
    myNameOfTable <- 'CE'
  } else if (grepl('.*HCL.csv',nameOfFile)){
    myNameOfTable <- 'CL'
  } else if (grepl('.*HSL.csv',nameOfFile)){
    myNameOfTable <- 'SL'
  } else if (grepl('.*HVD.csv',nameOfFile)){
    myNameOfTable <- 'VD'
  } else if (grepl('.*H[1,2,3,4,5,6,7,8,9].csv',nameOfFile)){
    myNameOfTable <- substring(nameOfFile, nchar(nameOfFile) - 5, nchar(nameOfFile) - 4)
  } else if (grepl('.*H1[0,1,2,3].csv',nameOfFile)){
    myNameOfTable <- substring(nameOfFile, nchar(nameOfFile) - 6, nchar(nameOfFile) - 4)
  }  
  
  if (is.na(myNameOfTable)){
    stop(paste("Not a valid file name ",nameOfFile))
  } else if (grepl('^H.*',myNameOfTable)){
    if (is.null(RequiredTables)) {
      stop(paste("You need to supply a value for RequiredTables if you want to read in file type ",myNameOfTable,sep=""))
    }
    myExchangeFileData <- readComplexExchangeFile(typeOfFile = myNameOfTable,RDBESvalidationdata= RDBESvalidationdata,nameOfFile = nameOfFile, RequiredTables = RequiredTables)
  } else {
    myExchangeFileData <- readSimpleExchangeFile(nameOfTable = myNameOfTable,RDBESvalidationdata= RDBESvalidationdata,nameOfFile = nameOfFile)
  }
  
  print(paste("Data has been imported from file ",nameOfFile,sep=""))
  
  myExchangeFileData
  
}

readSimpleExchangeFile <- function(nameOfTable,RDBESvalidationdata,nameOfFile){
  
  # Create an empty data frame in the right format
  emptyDf <- createEmptyDataFrameFromValidationData(nameOfTable = nameOfTable,RDBESvalidationdata = RDBESvalidationdata)
  
  # These are the classes we want the data to read in to be mapped to
  myColClasses <- c("character",unlist(lapply(emptyDf, class),use.names=F))
  
  # Read our exchange file
  myFileContents <- fread(file=nameOfFile, header = FALSE, stringsAsFactors = FALSE, na.strings="", colClasses = myColClasses)
  # If this is a data table change it to a data frame
  myFileContents <- as.data.frame(myFileContents)
  # Get rid of the first column (this is just the record type)
  myFileContents <- myFileContents[,2:length(names(myFileContents))]
  # Set the correct column names
  names(myFileContents) <- names(emptyDf)
  
  # Combine the empty data frame with the exchange file
  myData <- rbind(emptyDf,myFileContents)
  
  # Add our XXid column at the start with arbiitrary id values
  myID <- 1:nrow(myData)
  myData <- cbind(myID, myData)
  
  # Name our new id column correctly
  names(myData)[1] <- paste(nameOfTable,'id',sep = "")
  
  # return our data in a named list
  dataToReturn <- list()
  dataToReturn[[nameOfTable]] <- myData
  dataToReturn
  
}


createEmptyDataFrameFromValidationData <- function(nameOfTable, RDBESvalidationdata){
  
  # For testing
  #nameOfTable <- 'CE'
  
  # Get all the fields withe correct prefix
  myRegExp <- paste('^',nameOfTable,'.*',sep = "")
  myReleventFields <- validationData[grepl(myRegExp,validationData$name),]
  
  # Get our data frame column names
  myColNames <- myReleventFields$name
  # Work out what data types the columns shoudl be
  myColClasses <- ifelse(myReleventFields$type == 'xs:int','integer',ifelse(myReleventFields$type == 'xs:decimal' | myReleventFields$type == 'xs:float','numeric','character'))
  
  
  # Create our data frame (trick with read.csv taken from https://stackoverflow.com/questions/10689055/create-an-empty-data-frame)
  myNewDataFrame <- read.csv(text=paste(myColNames,collapse = ','), colClasses = myColClasses, stringsAsFactors = FALSE)
  ## If this is a data table change it to a data frame
  myNewDataFrame <- as.data.frame(myNewDataFrame)
  
  myNewDataFrame
  
}

#' compareCSData Function to take 2 lists of CS data and compare the data in them for equality
#'
#' @param dataSet1 
#' @param dataSet2 
#'
#' @return
#' @export
#'
#' @examples
compareCSData <- function(dataSet1, dataSet2, RequiredTables){
  
  # For testing
  #dataSet1 <- dataSet1
  #dataSet2 <- dataSet2
  
  # Our return value
  dataIsEqual <- TRUE
  
  # Which hierarchies do we have in the data?
  upperHierarchiesPresentInDS1 <-  unique(dataSet1[['DE']][,'DEhierarchy'])
  upperHierarchiesPresentInDS2 <-  unique(dataSet2[['DE']][,'DEhierarchy'])
  myTempResult <- compareMyValues(upperHierarchiesPresentInDS1,upperHierarchiesPresentInDS2)
  ifelse(myTempResult,dataIsEqual<-dataIsEqual,dataIsEqual<-FALSE)
  upperHierachiesToCheck <- intersect(upperHierarchiesPresentInDS1,upperHierarchiesPresentInDS2)
  
  # Which years do we have in the data?
  yearsPresentInDS1 <- unique(dataSet1[['DE']][,'DEyear'])
  yearsPresentInDS2 <- unique(dataSet2[['DE']][,'DEyear'])
  myTempResult <-compareMyValues(yearsPresentInDS1,yearsPresentInDS2)
  ifelse(myTempResult,dataIsEqual<-dataIsEqual,dataIsEqual<-FALSE)
  yearsToCheck <- intersect(yearsPresentInDS1,yearsPresentInDS2)
  
  # Which countries do we have in the data?
  countriesPresentInDS1 <- unique(dataSet1[['SD']][,'SDcountry'])
  countriesPresentInDS2 <- unique(dataSet2[['SD']][,'SDcountry'])
  myTempResult <-compareMyValues(countriesPresentInDS1,countriesPresentInDS2)
  ifelse(myTempResult,dataIsEqual<-dataIsEqual,dataIsEqual<-FALSE)
  countriesToCheck <- intersect(countriesPresentInDS1,countriesPresentInDS2)
  
  for (aYear in yearsToCheck){
    
    print(paste("Checking year ",aYear,sep=""))
    
    for (aCountry in countriesToCheck){
      
      print(paste("Checking country ",aCountry,sep=""))
      
      for (aHierarchy in upperHierachiesToCheck){
        
        aHierarchyText <- paste("H",aHierarchy,sep="")
        print(paste("Checking hierarchy ",aHierarchyText,sep=""))
        
        subsetDataSet1 <- filterCSData(RDBESdata = dataSet1 , RequiredTables = RequiredTables[[aHierarchyText]], YearToFilterBy = aYear, CountryToFilterBy = aCountry, UpperHierarchyToFilterBy = aHierarchy)
        
        subsetDataSet2 <- filterCSData(RDBESdata = dataSet2 , RequiredTables = RequiredTables[[aHierarchyText]], YearToFilterBy = aYear, CountryToFilterBy = aCountry, UpperHierarchyToFilterBy = aHierarchy)
        
        tablesInDS1 <- names(subsetDataSet1)
        tablesInDS2 <- names(subsetDataSet2)
        myTempResult <-compareMyValues(tablesInDS1,tablesInDS2)
        ifelse(myTempResult,dataIsEqual<-dataIsEqual,dataIsEqual<-FALSE)
        commonTables <- intersect(tablesInDS1,tablesInDS2)
        
        for (aTable in commonTables){
          
          # Don't check the XXid or XXrecordType columns - they will almost certainly have different values
          dataToCheck1 <- subsetDataSet1[[aTable]][,!(grepl('^..id$',names(subsetDataSet1[[aTable]])) | grepl('^..recordType$',names(subsetDataSet1[[aTable]])))]
          dataToCheck2 <- subsetDataSet2[[aTable]][,!(grepl('^..id$',names(subsetDataSet2[[aTable]])) | grepl('^..recordType$',names(subsetDataSet2[[aTable]])))]
          
          
          # Use the compare package to see if the data is basically the same (not identical but close enough)
          myComparisonResult <- compare(dataToCheck1,dataToCheck2, allowAll = TRUE)
          
          if (myComparisonResult$result){
            print(paste("The data in table ",aTable," matches",sep=""))
          } else {
            dataIsEqual<-FALSE
            print(paste("The data in table ",aTable," does not match",sep=""))
            if (!is.null(myComparisonResult$detailedResult)){
              print("There were differences found in the following columns:")
              print(names(dataToCheck1)[!myComparisonResult$detailedResult])
            }
          }
        }
      }
    }
  }
  
  dataIsEqual
  
}


#' compareMyValues Simple helper function to compare the contents of 2 sets of values and report if there are any differences.
#'
#' @param values1 
#' @param values2 
#'
#' @return
#' @export
#'
#' @examples
compareMyValues <- function(values1, values2){
  
  myReturnValue <- FALSE
  warningText <- list()
  
  values1 <- unique(values1)
  values2 <- unique(values2)
  
  # Check if the values are close enough to be considered equal
  compareValues <- compare(values1,values2,allowAll = TRUE)
  
  if (!compareValues$result){
    warningText[[1]]<- "The same values are not present in both data sets - we will only compare the values that are common (if there are any). "
    if (length(setdiff(values1,values2))>0){
      warningText[[2]] <- "The following values are in the first dataset but not the second: "
      warningText[[3]] <- paste(setdiff(values1,values2),collapse = ",")
    }
    if (length(setdiff(values2,values1))>0){
      warningText[[4]] <- "The following values are in the second dataset but not the first: "
      warningText[[5]] <- paste(setdiff(values2,values1),collapse = ",")
    }
    
  }
  
  # Show the warning text if we need to
  if (length(warningText)>0){
    #warning(unlist(warningText))
    print(unlist(warningText))
  }
  
  myReturnValue <- compareValues$result
  
  myReturnValue
  
}


#' compareSimpleData This function compares CE, CL, SL, or VD data for equality
#'
#' @param dataSet1 
#' @param dataSet2 
#' @param tableType 
#'
#' @return
#' @export
#'
#' @examples
compareSimpleData <- function(dataSet1, dataSet2, tableType){
  
  # For testing
  #tableType <- 'CL'
  #dataSet1 <- dataSet1
  #dataSet2 <- dataSet2
  
  
  # Our return value
  dataIsEqual <- TRUE
  
  yearField <- NA
  countryField <- NA
  
  if (tableType == 'CE'){
    yearField <- 'CEyear'
    countryField <- 'CEvesselFlagCountry'
  } else if (tableType == 'CL'){
    yearField <- 'CLyear'
    countryField <- 'CLvesselFlagCountry'
  } else if (tableType == 'VD'){
    yearField <- 'VDyear'
    countryField <- 'VDcountry'
  } else if (tableType == 'SL'){
    yearField <- 'SLyear'
    countryField <- 'SLcountry'
  }
  
  # Which years do we have in the data?
  yearsPresentInDS1 <- unique(dataSet1[[tableType]][,yearField])
  yearsPresentInDS2 <- unique(dataSet2[[tableType]][,yearField])
  myTempResult <-compareMyValues(yearsPresentInDS1,yearsPresentInDS2)
  ifelse(myTempResult,dataIsEqual<-dataIsEqual,dataIsEqual<-FALSE)
  yearsToCheck <- intersect(yearsPresentInDS1,yearsPresentInDS2)
  
  # Which countries do we have in the data?
  countriesPresentInDS1 <- unique(dataSet1[[tableType]][,countryField])
  countriesPresentInDS2 <- unique(dataSet2[[tableType]][,countryField])
  myTempResult <-compareMyValues(countriesPresentInDS1,countriesPresentInDS2)
  ifelse(myTempResult,dataIsEqual<-dataIsEqual,dataIsEqual<-FALSE)
  countriesToCheck <- intersect(countriesPresentInDS1,countriesPresentInDS2)
  
  for (aYear in yearsToCheck){
    
    print(paste("Checking year ",aYear,sep=""))
    
    for (aCountry in countriesToCheck){
      
      print(paste("Checking country ",aCountry,sep=""))
      
      # Don't check the XXid or XXrecordType columns - they will almost certainly have different values
      dataToCheck1 <- dataSet1[[tableType]][,!(grepl('^..id$',names(dataSet1[[tableType]])) | grepl('^..recordType$',names(dataSet1[[tableType]])))]
      dataToCheck2 <- dataSet2[[tableType]][,!(grepl('^..id$',names(dataSet2[[tableType]])) | grepl('^..recordType$',names(dataSet2[[tableType]])))]
      
      # Filter our datasets by year and country
      dataToCheck1 <- dataToCheck1[dataToCheck1[,countryField] == aCountry,]
      dataToCheck1 <- dataToCheck1[dataToCheck1[,yearField] == aYear,]
      
      dataToCheck2 <- dataToCheck2[dataToCheck2[,countryField] == aCountry,]
      dataToCheck2 <- dataToCheck2[dataToCheck2[,yearField] == aYear,]
      
      # Use the compare package to see if the data is basically the same (not identical but close enough)
      myComparisonResult <- compare(dataToCheck1,dataToCheck2, allowAll = TRUE)
      
      if (myComparisonResult$result){
        print(paste("The data in table ",tableType," matches",sep=""))
      } else {
        dataIsEqual<-FALSE
        print(paste("The data in table ",tableType," does not match",sep=""))
        if (!is.null(myComparisonResult$detailedResult)){
          print("There were differences found in the following columns:")
          print(names(dataToCheck1)[!myComparisonResult$detailedResult])
        }
      }
    }
  }
  
  dataIsEqual
  
}



#' createTestData
#'
#' @param HierarchyToGenerate 
#' @param LowerHierarchyToGenerate 
#' @param RDBESvalidationdata 
#' @param RDBEScodeLists 
#' @param RequiredTables 
#' @param NumberOfStrata 
#' @param NumberSampled 
#' @param NumberTotal 
#' @param SelectionMethods 
#'
#' @return
#' @export
#'
#' @examples
createTestData <- function(HierarchyToGenerate,LowerHierarchyToGenerate, RDBESvalidationdata, RDBEScodeLists, RequiredTables, NumberOfStrata, NumberSampled, NumberTotal, SelectionMethods ){
  
  myRequiredTables <- RequiredTables[[HierarchyToGenerate]]
  myAuxTables <- c('VD','SL')
  
  # We might need to remove some tables, depending on the lower hierarchy
  if (LowerHierarchyToGenerate == 'A'){
    # No need to do anything
  } else if (LowerHierarchyToGenerate == 'B'){
    # Remove BV
    myRequiredTables <- myRequiredTables[myRequiredTables != 'BV']
  } else if (LowerHierarchyToGenerate == 'C'){
    # Remove FM
    myRequiredTables <- myRequiredTables[myRequiredTables != 'FM']
  } else if (LowerHierarchyToGenerate == 'D'){
    # Remove FM and BV
    myRequiredTables <- myRequiredTables[myRequiredTables != 'FM']
    myRequiredTables <- myRequiredTables[myRequiredTables != 'BV']
  }
  
  # First create a list with all the empty tables we need in it (the CS hierarchy tables and VD, SL)
  myDataList <- list()
  
  for (myTable in c(myRequiredTables,myAuxTables)){
    myEmptyDf <- createEmptyDataFrameFromValidationData(nameOfTable = myTable, RDBESvalidationdata = RDBESvalidationdata)
    # Add our empty XXid column to the front of the data frame
    myID <- integer(0)
    # Create the record type column
    myRecType <- character(0)
    myEmptyDf <- cbind(myID,myRecType,myEmptyDf)
    # Name our new columns correctly
    names(myEmptyDf)[1] <- paste(myTable,'id',sep = "")
    names(myEmptyDf)[2] <- paste(myTable,'recordType',sep = "")
    # Add the empty data frame to our list
    myDataList[[myTable]] <- myEmptyDf
    
    # Check if we have been given values for some aspects of the data we want to generate - if not we set defaults
    
    if (!myTable %in% names(NumberOfStrata)){
      NumberOfStrata[[myTable]] <- 1
    }
    
    if (!myTable %in% names(NumberSampled)){
      NumberSampled[[myTable]] <- 1
    }
    
    if (!myTable %in% names(NumberTotal)){
      NumberTotal[[myTable]] <- NumberSampled[[myTable]] +1
    }
    
    if (!myTable %in% names(SelectionMethods)){
      SelectionMethods[[myTable]] <- 'SRSWR'
    }
    
    myDF <- myDataList[[myTable]]
    myNumberOfStrata <- NumberOfStrata[[myTable]]
    myNumberSampled <- NumberSampled[[myTable]]
    myNumberTotal <- NumberTotal[[myTable]]
    myMethod <- SelectionMethods[[myTable]]
    
    # Generate test data for each required table
    myNewDF <- createNewTestDataFrame(HierarchyToGenerate = HierarchyToGenerate,LowerHierarchyToGenerate = LowerHierarchyToGenerate, TableType = myTable, DataFrameToUpdate = myDF, NumberOfStrata = myNumberOfStrata, NumberSampled = myNumberSampled, NumberTotal = myNumberTotal, SelectionMethod = myMethod, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists)
    
    myDataList[[myTable]] <- myNewDF
    
    
  }
  
  #myDataList
  
  # At the moment we have a lot of indepdent test data - we'll mutiple all these records together so that we have our data linked together
  myMultipliedTestData <- list()
  
  currentTable <- NA
  previousTable <- NA
  
  for (myTable in myRequiredTables){
    
    previousTable <- currentTable
    currentTable <- myTable
    #print(currentTable)
    #print(previousTable)
    myCurrentData <-  myDataList[[currentTable]]
    
    # If we're not at the top level table lets start mutiplying data
    if (!is.na(previousTable)){
      myPreviousData <- myMultipliedTestData[[previousTable]]
      
      FKcolumnName <- paste(previousTable,'id',sep="")
      
      # Add our foreign key column to the data
      myCurrentData[,FKcolumnName]<-integer(nrow(myCurrentData))
      # Default the value to NA if we need to
      if (nrow(myCurrentData)>0){
        myCurrentData[,FKcolumnName] <- NA
      }
      
      # For each unique value of the foreign key in the previous table we will mutiply the data in the current table
      for (myFK in unique(myPreviousData[,FKcolumnName])){
        newBlockOfData <- myCurrentData
        newBlockOfData[,FKcolumnName] <- myFK
        myMultipliedTestData[[currentTable]] <- rbind(myMultipliedTestData[[currentTable]],newBlockOfData)
      }
      
      # Re-generate the PK for our mutiplied data because they will have been duplicated
      myMultipliedTestData[[currentTable]][,paste(currentTable,'id',sep="")] <- 1:nrow(myMultipliedTestData[[currentTable]])
      
      # If we're at the first table we'll just take those values without trying to multiply them
    } else {
      myMultipliedTestData[[currentTable]] <- myCurrentData
    }
    
  }
  
  # Just add the auxillary tables into the output without doing anything to them
  for (myTable in myAuxTables){
    myMultipliedTestData[[myTable]] <- myDataList[[myTable]]
  }
  
  # Ok we now have soem fairly meaningless random data - lets try and make it a bit more useful
  myMultipliedTestData
  
  
}




#' createNewTestDataFrame
#'
#' @param HierarchyToGenerate 
#' @param LowerHierarchyToGenerate 
#' @param TableType 
#' @param DataFrameToUpdate 
#' @param NumberOfStrata 
#' @param NumberSampled 
#' @param NumberTotal 
#' @param SelectionMethod 
#' @param RDBESvalidationdata 
#' @param RDBEScodeLists 
#'
#' @return
#' @export
#'
#' @examples
createNewTestDataFrame <- function(HierarchyToGenerate,LowerHierarchyToGenerate, TableType, DataFrameToUpdate, NumberOfStrata, NumberSampled, NumberTotal,SelectionMethod, RDBESvalidationdata, RDBEScodeLists){
  
  # Generate our strata names
  if (NumberOfStrata <= 1){
    myStratumNames <- c('U')
  } else {
    myStratumNames <- sapply(1:NumberOfStrata, function(x) paste(TableType,'_stratum',x,sep=""))
  }
  
  # For each stratum
  for (aStratum in myStratumNames){
    # for each of our sampled units
    for (i in 1:NumberSampled){
      
      # Create a new row id
      newRowID <- nrow(DataFrameToUpdate) +1
      
      # Create a new row
      myNewRowValues <- createNewTestDataRow(HierarchyToGenerate = HierarchyToGenerate,LowerHierarchyToGenerate = LowerHierarchyToGenerate, TableType = TableType, RowID = newRowID, ColumnNames = names(DataFrameToUpdate), StratumName = aStratum, NumberSampled = NumberSampled, NumberTotal = NumberTotal,SelectionMethod = SelectionMethod, RDBESvalidationdata = RDBESvalidationdata, RDBEScodeLists = RDBEScodeLists)
      
      # Add the new row to our data frame
      myNewRow <- as.data.frame(myNewRowValues, stringsAsFactors = FALSE)
      # Slow to use Rbind in a loop :-(
      DataFrameToUpdate <- rbind(DataFrameToUpdate,myNewRow)  
    }
  }
  
  DataFrameToUpdate
  
  
}



#' createNewTestDataRow
#'
#' @param HierarchyToGenerate 
#' @param LowerHierarchyToGenerate 
#' @param TableType 
#' @param RowID 
#' @param ColumnNames 
#' @param StratumName 
#' @param NumberSampled 
#' @param NumberTotal 
#' @param SelectionMethod 
#' @param RDBESvalidationdata 
#' @param RDBEScodeLists 
#'
#' @return
#' @export
#'
#' @examples
createNewTestDataRow <- function(HierarchyToGenerate,LowerHierarchyToGenerate, TableType, RowID, ColumnNames, StratumName, NumberSampled, NumberTotal,SelectionMethod, RDBESvalidationdata, RDBEScodeLists){
  
  # Empty list to hold our new row values
  myNewRowValues <- list()
  
  # For each column in our data frame
  for (myColName in ColumnNames){
    
    # Default the new value to NA
    myNewValue <- NA
    
    # We'll deal with the 'special' columns first (most specific first), then deal with all the others
    
    # SPECIAL COLUMN - DEhierarchy
    if (myColName == 'DEhierarchy') {
      myNewValue <- substr(HierarchyToGenerate,2,nchar(HierarchyToGenerate))
      # SPECIAL COLUMN - DEhierarchyCorrect
    } else if (myColName == 'DEhierarchyCorrect') {
      myNewValue <- 'Y'
      # SPECIAL COLUMN - SAlowerHierarchy
    } else if (myColName == 'SAlowerHierarchy') {
      myNewValue <- LowerHierarchyToGenerate
      # SPECIAL COLUMN - BVfishID
    } else if (myColName == 'BVfishId') {
      myNewValue <- RowID
      # SPECIAL COLUMN - XXid
    } else if (myColName == paste(TableType,'id',sep="")) {
      myNewValue <- RowID
      # SPECIAL COLUMN - XXrecordType
    } else if (myColName == paste(TableType,'recordType',sep="")) {
      myNewValue <- TableType
      # SPECIAL COLUMN - XXsequenceNumber
    } else if (grepl('^..sequenceNumber$',myColName)) {
      myNewValue <- RowID
      # SPECIAL COLUMN - XXunitName
    } else if (grepl('^..unitName$',myColName)) {
      myNewValue <- paste(TableType,'_unit_', RowID,sep = "")
      # SPECIAL COLUMN - XXstratification
    } else if (grepl('^..stratification$',myColName)) {
      if (StratumName =='U'){
        #myNewValue <-if_else(myColName == 'BVstratification','N','No')
        myNewValue <- 'N'
      } else {
        #myNewValue <-if_else(myColName == 'BVstratification','Y','Yes')
        myNewValue <- 'Y'
      }
      # SPECIAL COLUMN - XXstratumName
    } else if (grepl('^..stratumName$',myColName)) {
      myNewValue <- StratumName
      # SPECIAL COLUMN - XXnumberSampled
    } else if (grepl('^..numberSampled$',myColName)) {
      myNewValue <- NumberSampled
      # SPECIAL COLUMN - XXnumberTotal
    } else if (grepl('^..numberTotal$',myColName)) {
      myNewValue <- NumberTotal
      # SPECIAL COLUMN - XXselectionMethod
    } else if (grepl('^..selectionMethod$',myColName)) {
      myNewValue <- SelectionMethod
      # SPECIAL COLUMN - XXsampled
    } else if (grepl('^..sampled$',myColName)) {
      myNewValue <- 'Y'
      # SPECIAL COLUMN - XXclustering
    } else if (grepl('^..clustering$',myColName)) {
      myNewValue <- 'N'
      # SPECIAL COLUMN - XXclusterName
    } else if (grepl('^..clusterName$',myColName)) {
      myNewValue <- 'U'
      # SPECIAL COLUMN - XXgsaSubarea
    } else if (grepl('^..gsaSubarea$',myColName)) {
      myNewValue <- 'NotApplicable'
      # SPECIAL COLUMN - XXArea
    } else if (grepl('^..area$',myColName)) {
      myNewValue <- '27'
      # NOT SPECIAL :-(
    } else {
      # Else this column is not special :-( - just put some random data in it....
      
      # Try finding the validation info for this column
      myValidationInfo <- RDBESvalidationdata[RDBESvalidationdata$name == myColName,]
      
      # VALIDATION INFROMATION FOUND
      if (nrow(myValidationInfo)==1){
        #print(myColName)
        # MANDATORY
        if (myValidationInfo$min >0){
          # Mandatory so we need to do soemthing :-)
          # What we do will depden on what type of column it is e.g. code list, int, double etc
          
          # CODE LIST
          if (grepl('^t.*',myValidationInfo$type) & is.na(myValidationInfo$description)){
            # if its a code list we'll get the first entry from the code list
            requiredCodeListName <- myValidationInfo$type
            #print(requiredCodeListName)
            firstEntry <- RDBEScodeLists[RDBEScodeLists$listName == requiredCodeListName,'Key' ][1]
            myNewValue <- firstEntry
            # SIMPLETYPECHECK
          } else if (!is.na(myValidationInfo$description) & myValidationInfo$description == 'simpleTypeCheck'){
            # simpleTypeCheck - string with pattern
            if (!is.na(myValidationInfo$pattern)){
              # string with pattern - I assume this can only be dates or times - might not hold true in the future
              if (myValidationInfo$checkName == 'tDate'){
                myNewValue <- '1999-01-01'
              } else if (myValidationInfo$checkName == 'tDate'){
                myNewValue <- '12:34'
              } else {
                print(paste("There was a pattern check that I didn't deal with in column ", myColName, sep =""))
              }
              # simpleTypeCheck - min value
            } else if (!is.na(myValidationInfo$minValue)){
              myNewValue <- myValidationInfo$minValue
              # simpleTypeCheck - max value (but no min)
            } else if (!is.na(myValidationInfo$maxValue)){
              myNewValue <- myValidationInfo$maxValue
              # simpleTypeCheck - specified max decimal places
            } else if (!is.na(myValidationInfo$fractionDigits)){
              myNewValue <- round(1.12345678912345678912345,as.integer(myValidationInfo$fractionDigits))
            } else {
              print(paste("There was a simpleTypeCheck that I didn't deal with in column ", myColName, sep =""))
            }
            # INT, LONG, OR DECIMAL
          } else if (myValidationInfo$type %in% c('xs:int','xs:long','xs:decimal')){
            myNewValue <- 10
            # STRING
          } else if (myValidationInfo$type == 'xs:string'){
            myNewValue <- 'abc'
            # DIDN'T MATCH - TELL THE USER
          } else {
            print(paste("I didn't deal with the validation information in column ", myColName, sep =""))
          }
          # OPTIONAL
        } else {
          # If the field is optional- don't bother to do anything
        }
        # NO VALIDATION INFORMATION FOUND
      } else {
        print(paste("Could not find information for ",myColName,sep=""))
      }
    }
    # Add our new value to the list of new values
    myNewRowValues[[myColName]] <- myNewValue
  }
  
  myNewRowValues
  
}

#' makeTestDataMoreRealistic
#'
#' @param DataToUse 
#' @param CountryToUse 
#' @param YearToUse 
#' @param MetierList 
#' @param SpeciesList 
#' @param RDBEScodeLists 
#'
#' @return
#' @export
#'
#' @examples
makeTestDataMoreRealistic <- function(DataToUse,CountryToUse,YearToUse,MetierList,SpeciesList,RDBEScodeLists, catchFractionToUse = 'Lan', landingCategoryToUse = 'HuC'){
  
  # For testing
  #DataToUse <- myTestData
  #RDBEScodeLists <- allowedValues
  #CountryToUse <- 'IE'
  #SpeciesList <- c(126436)
  #MetierList <- c('OTB_DEF_100-119_0_0')
  
  
  # VESSEL DETAILS
  DataToUse[['VD']][,'VDcountry'] <- CountryToUse
  DataToUse[['VD']][,'VDflagCountry'] <- CountryToUse
  DataToUse[['VD']][,'VDyear'] <- YearToUse
  # Lets pick some random vessel lengths
  myVesselLengths <- RDBEScodeLists[RDBEScodeLists$listName == 'tRS_VesselLengthCategory','Key']
  DataToUse[['VD']][,'VDlengthCategory'] <- sample(myVesselLengths,nrow(DataToUse[['VD']]),replace = TRUE)
  DataToUse[['VD']][,'VDencryptedVesselCode'] <- paste('VDcode_',DataToUse[['VD']][,'VDid'],sep="")
  
  # SPECIES LIST DETAILS
  DataToUse[['SL']][,'SLcountry'] <- CountryToUse
  DataToUse[['SL']][,'SLyear'] <- YearToUse
  DataToUse[['SL']][,'SLspeciesListName'] <- paste(CountryToUse,'_',YearToUse,'_SpeciesList',sep="")
  mySpeciesCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tSpecWoRMS','Key']
  mySpeciesCodes <- c(SpeciesList,mySpeciesCodes)
  mySpeciesCodes <- unique(mySpeciesCodes)
  mySpeciesCodes <- mySpeciesCodes[1:nrow(DataToUse[['SL']])]
  DataToUse[['SL']][,'SLcommercialTaxon'] <- mySpeciesCodes
  DataToUse[['SL']][,'SLspeciesCode'] <- mySpeciesCodes
  DataToUse[['SL']][,'SLcatchFraction'] <- catchFractionToUse
  
  # CS TABLES
  
  # COUNTRY
  # Sort out the country and location code fields
  DataToUse[['SD']][,'SDcountry'] <- CountryToUse
  if ('LE' %in% names(DataToUse)){
    DataToUse[['LE']][,'LEcountry'] <- CountryToUse
  }
  
  # LOCODE
  # For fields that use locodes we'll pick a random locode from the country we are interested in
  myCountryLocodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tHarbour_LOCODE','Key']
  myCountryLocodes <- myCountryLocodes[grepl(paste('^',CountryToUse,'.*$',sep=''), myCountryLocodes)]
  if ('FT' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['FT']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['FT']][,'FTdepartureLocation'] <- myRandomValues
    DataToUse[['FT']][,'FTarrivalLocation'] <- myRandomValues
  }
  if ('OS' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['OS']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['OS']][,'OSlocode'] <- myRandomValues
  }
  if ('LE' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['LE']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['LE']][,'LElocation'] <- myRandomValues
  }
  if ('LO' %in% names(DataToUse)){
    if (length(myCountryLocodes) > 1){
      myRandomValues <- sample(myCountryLocodes,nrow(DataToUse[['LO']]),replace = TRUE)
    } else {
      myRandomValues <- myCountryLocodes
    }
    DataToUse[['LO']][,'LOlocode'] <- myRandomValues
  }
  
  # YEAR
  DataToUse[['DE']][,'DEyear'] <- YearToUse
  
  # DATES
  # For dates we'll pick a date within the year we are using
  if ('FT' %in% names(DataToUse)){
    myRandomValues <- sample(seq(as.Date(paste(YearToUse,'/01/01',sep="")), as.Date(paste(YearToUse,'/12/01',sep="")), by="day"), nrow(DataToUse[['FT']]),replace = TRUE)
    myRandomTripLength <- sample(1:30,nrow(DataToUse[['FT']]),replace = TRUE)
    
    DataToUse[['FT']][,'FTdepartureDate'] <- format(myRandomValues,'%Y-%m-%d')
    DataToUse[['FT']][,'FTarrivalDate'] <- format(myRandomValues + myRandomTripLength,'%Y-%m-%d')
  }
  # for FO we'll use the arrival date of the trip for all our fishing acitvity
  if ('FO' %in% names(DataToUse) & 'FT' %in% names(DataToUse)){
    FTDates <- DataToUse[['FT']][,c('FTid','FTdepartureDate','FTarrivalDate')]
    myJoin <- inner_join(DataToUse[['FO']],FTDates, by ='FTid')
    DataToUse[['FO']][,'FOendDate'] <- myJoin[,'FTarrivalDate']
  }
  if ('OS' %in% names(DataToUse)){
    myRandomValues <- sample(seq(as.Date(paste(YearToUse,'/01/01',sep="")), as.Date(paste(YearToUse,'/12/01',sep="")), by="day"), nrow(DataToUse[['OS']]),replace = TRUE)
    DataToUse[['OS']][,'OSsamplingDate'] <- format(myRandomValues,'%Y-%m-%d')
  }
  if ('LE' %in% names(DataToUse)){
    myRandomValues <- sample(seq(as.Date(paste(YearToUse,'/01/01',sep="")), as.Date(paste(YearToUse,'/12/01',sep="")), by="day"), nrow(DataToUse[['LE']]),replace = TRUE)
    DataToUse[['LE']][,'LEdate'] <- format(myRandomValues,'%Y-%m-%d')
  }
  
  # SPECIES LIST NAME
  DataToUse[['SS']][,'SSspeciesListName'] <- paste(CountryToUse,'_',YearToUse,'_SpeciesList',sep="")
  DataToUse[['SS']][,'SScatchFraction'] <- catchFractionToUse
  
  # VESSELS
  # Ensure we are only referring to vessel that appear in our Vessel Details - this gets a bit complex because we can have the encryptedVesselCode used in a few tables, which can be linked to each other
  myVesselCodes <- DataToUse[['VD']][,'VDencryptedVesselCode']
  
  # If we have VS data - pick a random vessel for each VS row, and then ensure that any FT or LE rows connected to it use the same value
  if ('VS' %in% names(DataToUse)){
    
    if (length(myVesselCodes) > 1){
      myRandomValues <- sample(myVesselCodes,nrow(DataToUse[['VS']]),replace = TRUE)
    } else {
      myRandomValues <- myVesselCodes
    }
    
    DataToUse[['VS']][,'VSencryptedVesselCode'] <- myRandomValues
    DataToUse[['VS']][,'VSunitName'] <- myRandomValues
  }
  
  # IF we have FT data we need to see whether it is connected to an VS record - if so, use the same vessel code, else pick a random one
  if ('FT' %in% names(DataToUse)){
    # Clear out any existing values first
    DataToUse[['FT']][,'FTencryptedVesselCode'] <- NA
    
    # If we have a VS table then we'll use the vessel code form that record in the FT record
    if ('VS' %in% names(DataToUse) & 'VSid' %in% names(DataToUse[['FT']])){
      VSvessel <- DataToUse[['VS']][,c('VSid','VSencryptedVesselCode')]
      myJoin <- inner_join(DataToUse[['FT']],VSvessel, by ='VSid')
      DataToUse[['FT']][,'FTencryptedVesselCode'] <- myJoin[,'VSencryptedVesselCode']
    } 
    
    # Now use random vessels for any remaining NA vessel codes
    if (length(myVesselCodes) > 1){
      myRandomValues <- sample(myVesselCodes,nrow(DataToUse[['FT']][is.na(DataToUse[['FT']][,'FTencryptedVesselCode']),]),replace = TRUE)
    } else {
      myRandomValues <- myVesselCodes
    }
    if (nrow(DataToUse[['FT']][is.na(DataToUse[['FT']][,'FTencryptedVesselCode']),])>0){
      DataToUse[['FT']][is.na(DataToUse[['FT']][,'FTencryptedVesselCode']),'FTencryptedVesselCode'] <- myRandomValues
    }
    
    DataToUse[['FT']][,'FTsequenceNumber'] <- DataToUse[['FT']][,'FTid']
    
  }
  
  # If we have LE data it can be directly linked to either a VS or FT record - need to use the same vessel code if it is - else pick a random one
  if ('LE' %in% names(DataToUse)){
    
    # Clear out any existing values first
    DataToUse[['LE']][,'LEencryptedVesselCode'] <- NA
    
    # If we have a VS table then we'll use the vessel code form that record in the LE record
    if ('VS' %in% names(DataToUse) & 'VSid' %in% names(DataToUse[['LE']])  ){
      VSvessel <- DataToUse[['VS']][,c('VSid','VSencryptedVesselCode')]
      myJoin <- inner_join(DataToUse[['LE']],VSvessel, by ='VSid')
      DataToUse[['LE']][,'LEencryptedVesselCode'] <- myJoin[,'VSencryptedVesselCode']
    } 
    
    # If we have a FT table then we'll use the vessel code form that record in the LE record
    if ('FT' %in% names(DataToUse) & 'FTid' %in% names(DataToUse[['LE']])){
      FTvessel <- DataToUse[['FT']][,c('FTid','FTencryptedVesselCode')]
      myJoin <- inner_join(DataToUse[['LE']],FTvessel, by ='FTid')
      DataToUse[['LE']][,'LEencryptedVesselCode'] <- myJoin[,'FTencryptedVesselCode']
    } 
    
    # Now use random vessels for any remaining NA vessel codes
    if (length(myVesselCodes) > 1){
      myRandomValues <- sample(myVesselCodes,nrow(DataToUse[['LE']][is.na(DataToUse[['LE']][,'LEencryptedVesselCode']),]),replace = TRUE)
    } else {
      myRandomValues <- myVesselCodes
    }
    if (nrow(DataToUse[['LE']][is.na(DataToUse[['LE']][,'LEencryptedVesselCode']),])>0){
      DataToUse[['LE']][is.na(DataToUse[['LE']][,'LEencryptedVesselCode']),'LEencryptedVesselCode'] <- myRandomValues
    }
    
  }
  
  # SPECIES
  
  # Ensure we are only sampling species that appear in our Species Details
  mySpeciesCodes <- DataToUse[['SL']][,'SLspeciesCode']
  if (length(mySpeciesCodes) > 1){
    myRandomValues <- sample(mySpeciesCodes,nrow(DataToUse[['SA']]),replace = TRUE)
  } else {
    myRandomValues <- mySpeciesCodes
  }
  DataToUse[['SA']][,'SAspeciesCode'] <- myRandomValues
  
  # METIERS
  
  # If we have been given a list of metiers then we'll just use them 
  if (!is.null(MetierList)){
    myMetiersCodes <- unique(MetierList)
    # Otherwise we'll generate a list of metiers from the code list
  } else {
    myMetiersCodes <- RDBEScodeLists[RDBEScodeLists$listName == 'tMetier6_FishingActivity','Key']
    # Pick the maximum number of metiers for our list based on whatever is the biggest number from i) the number of SA records, ii) the number 1 (to cover the very rare case when we don't have any samples)
    numerOfMetiersToUse <- max(nrow(DataToUse[['SA']]), 1 )
    myMetiersCodes <- myMetiersCodes[1:numerOfMetiersToUse]
  }
  
  # If we have FO data pick a random metier and gear code
  if ('FO' %in% names(DataToUse)){
    # use random metier
    if (length(myMetiersCodes) > 1){
      myRandomValues <- sample(myMetiersCodes,nrow(DataToUse[['FO']]),replace = TRUE)
    } else {
      myRandomValues <- myMetiersCodes
    }
    DataToUse[['FO']][,'FOmetier6'] <- myRandomValues
    DataToUse[['FO']][,'FOgear'] <- substring(myRandomValues,1,3)
  }
  
  # If we have LE data pick a random metier and gear code
  if ('LE' %in% names(DataToUse)){
    # use random metier
    if (length(myMetiersCodes) > 1){
      myRandomValues <- sample(myMetiersCodes,nrow(DataToUse[['LE']]),replace = TRUE)
    } else {
      myRandomValues <- myMetiersCodes
    }
    DataToUse[['LE']][,'LEmetier6'] <- myRandomValues
    DataToUse[['LE']][,'LEgear'] <- substring(myRandomValues,1,3)
    
    DataToUse[['LE']][,'LEsequenceNumber'] <- DataToUse[['LE']][,'LEid']
    
  }
  
  # If we have SA data pick a random metier and gear code
  if ('SA' %in% names(DataToUse)){
    # use random metier
    if (length(myMetiersCodes) > 1){
      myRandomValues <- sample(myMetiersCodes,nrow(DataToUse[['SA']]),replace = TRUE)
    } else {
      myRandomValues <- myMetiersCodes
    }
    DataToUse[['SA']][,'SAmetier6'] <- myRandomValues
    DataToUse[['SA']][,'SAgear'] <- substring(myRandomValues,1,3)
    
    # Ensure SAsequence number is unique
    DataToUse[['SA']][,'SAsequenceNumber'] <- DataToUse[['SA']][,'SAid']
    
    # Sort out catch and landing categories
    DataToUse[['SA']][,'SAcatchCategory'] <- catchFractionToUse
    DataToUse[['SA']][,'SAlandingCategory'] <- landingCategoryToUse
    
  }
  
  # Temporal
  if ('TE' %in% names(DataToUse)){
    DataToUse[['TE']][,'TEsequenceNumber'] <- DataToUse[['TE']][,'TEid']
    
  }
  
  # FREQUENCY MEASURE
  
  if ('FM' %in% names(DataToUse)){
    
    # For each species sampled we'll randomly pick a minimum length and then create soem normal length data in the FM data
    speciesSampled <- unique(DataToUse[['SA']][,'SAspeciesCode'])
    minLength <- data.frame( SAspeciesCode = speciesSampled, minLength = sample(10:40,length(speciesSampled), replace = TRUE), stringsAsFactors = FALSE)
    myJoin <- inner_join(DataToUse[['SA']],minLength, by ='SAspeciesCode')
    myJoin2 <- inner_join(DataToUse[['FM']],myJoin,by='SAid')
    # Set all the values of FMclass to be the minium length to start with - we'll change this in a bit
    DataToUse[['FM']][,'FMclass'] <- myJoin2[,'minLength'] * 10
    
    # For each sample, create increasing values for FMclass, with a normal distribution of fish counts
    # TODO - shoudl be a better way to do this
    for (mySAid in unique(DataToUse[['FM']][,'SAid'])){
      
      # The FM data associated with this SAid
      myFMDataForASample <- DataToUse[['FM']][DataToUse[['FM']][,'SAid'] == mySAid,]
      
      # First we'll generate soem nromal data that we will use for the number of fish at each length
      myNumberOfLengthClasses <- nrow(myFMDataForASample)
      myMinLengthClass <- min(myFMDataForASample[,'FMclass'])
      # Set a standard deviation
      mySD <- 5
      # Assume the mean is half way along the number of length classes
      myMean <- myMinLengthClass + (myNumberOfLengthClasses/2)
      # Generate a normal distribution of lengths
      myLengthSequence <- seq(myMinLengthClass, myMinLengthClass + myNumberOfLengthClasses, by = 1)
      myNormalFishCounts <- dnorm(myLengthSequence, mean = myMean, sd = mySD)
      # Let's fix the max number of fish at a length class as 10 and scale everything accordingly
      myMultiplyFactor <- 10/max(myNormalFishCounts)
      myNormalFishCounts <- round(myNormalFishCounts*myMultiplyFactor)
      
      # Now lets generate our lengths and the number of fish at each length
      currentLength <- NA
      FMCount <- 0
      for (myFMid in myFMDataForASample[,'FMid']){
        FMCount <- FMCount + 1
        if (is.na(currentLength)){
          currentLength <- myFMDataForASample[myFMDataForASample[,'FMid']==myFMid,'FMclass']
        } else {
          # Add 10mm to previous length
          currentLength <- currentLength + 10
        }
        myFMDataForASample[myFMDataForASample[,'FMid'] == myFMid,'FMclass'] <- currentLength
        # Make a random number of fish at that length
        #myFMDataForASample[myFMDataForASample[,'FMid'] == myFMid,'FMnumberAtUnit'] <- sample(1:10,1)
        # Use our nomral distribution of fish counts
        myFMDataForASample[myFMDataForASample[,'FMid'] == myFMid,'FMnumberAtUnit'] <- myNormalFishCounts[FMCount]
        
      }
      DataToUse[['FM']][DataToUse[['FM']][,'SAid'] == mySAid,] <- myFMDataForASample
    }
  }
  
  # BIOLOGICAL VARIABLES
  if ('BV' %in% names(DataToUse)){
    
    # Sort out BVfishId so that we don't have duplicates
    DataToUse[['BV']][,'BVfishId'] <- DataToUse[['BV']][,'BVid']
    
    # Ages
    myBVages <- DataToUse[['BV']][DataToUse[['BV']][,'BVtype'] == 'Age',]
    
    # If we have soem age data
    if (nrow(myBVages)>0){
      
      # Set the ages to NA to start with 
      DataToUse[['BV']][DataToUse[['BV']][,'BVtype'] == 'Age','BVvalue'] <- NA
      
      # If the BV is linked to a length class use that to generate an age
      if( 'FMid' %in% names(myBVages)){
        myJoin <- inner_join(myBVages,DataToUse[['FM']][,c('FMid','FMclass')], by ='FMid')
        if (nrow(myJoin)>0){
          # Assume the age is length / 50
          myJoin[,'BVvalue'] <- round(myJoin[,'FMclass']/50.0)
          # Update the age values in our data
          # TODO - this is not a good way of doing things in R
          for(myBV in myJoin[,'BVid'] ){
            DataToUse[['BV']][DataToUse[['BV']][,'BVid'] == myBV,'BVvalue'] <- myJoin[myJoin[,'BVid']==myBV,'BVvalue']
          }
          
        }
        
      } 
      
      # For any remaining NAs we'll pick a random age
      DataToUse[['BV']][DataToUse[['BV']][,'BVtype'] == 'Age' & is.na(DataToUse[['BV']][,'BVvalue']),'BVvalue'] <- as.character(sample(1:10, nrow(myBVages),replace = TRUE))
      
      # Set the unit as Year
      DataToUse[['BV']][DataToUse[['BV']][,'BVtype'] == 'Age','BVvalueType'] <- 'Year'
    }
  }
  
  
  # Return our data
  DataToUse
  
}


