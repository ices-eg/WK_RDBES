# Author: https://github.com/davidcurrie2001
# Copied: 20200911

###############################################################################
# TODO
# (kibi - 20200911): It would be nice if RDBES_Functions.R was sourced directly 
#     from https://github.com/davidcurrie2001, but I can figure out how
#
##############################################################################

# Local paths to .xsd files

baseType_path <- "Q:/mynd/RDB/RDBES/XSD-files/"
hierarchies_path <- "Q:/mynd/RDB/RDBES/XSD-files/"

# Load our functions
source("./WKRDB-EST2/testData/RDBES_Functions.R")

# This file shows how to generate test data for the RDBES

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = paste0(baseType_path, "BaseTypes.xsd"))
#validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# 11/9/2020 Temp fix because the validation fields aren't up to date :-(
validationData[validationData$type == 'tRS_Stratification','type'] <- 'tYesNoFields'

# Load the reference data: either refresh from ICES or just use a local copy
# Use line below for WKRDB-EST2
allowedValues <- readRDS(file="./WKRDB-EST2/testData/referenceData/allowedValues.RDS")

# Get icesVocab for R version 4.0.2
# This is not needed for WKRDB-EST values are already downloaded
# 
# library(devtools)
# install_github("ices-tools-prod/icesVocab")
#allowedValues <- loadReferenceData(downloadFromICES = T)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = hierarchies_path)
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

## STEP 2) GENERATE TEST DATA

# Can use a loop to generate test data for all hierarchies if you want to 
for (i in 1:1){
  myHierarchyToGenerate <- paste('H',i,sep="")
  
  # Define some parameters for our test data
  #myHierarchyToGenerate <- 'H1'
  print(myHierarchyToGenerate)
  myLowerHierarchyToGenerate <- 'A'
  myYear <- 2015
  myCountry <- 'IE'
  # Number of strata in different tables - if no value if given for a table then it is assumed to be unstratified
  myStrata <- list(DE = 2, VS = 2)
  # Number of things sampled in different tables - if no value is given for a table then it is assumed to be 1
  mySampled <- list(VS=5,FO=3,SS=1,SA=2, FM=20,BV=2, VD=10, SL=20)
  # Total number of things in different tables - if no value is given for a table then it is assumed to be equal to the number sampled + 1
  myTotal <- list(VS=30,FO=10,SS=4, FM=20, BV=2)
  # Select methods used in different tables - if no value is given for a table then it is assumed to be simple random sampling SRSWR
  myMethods <- list()
  
  # Generate some random data
  myTestData <- createTestData(HierarchyToGenerate = myHierarchyToGenerate, LowerHierarchyToGenerate = myLowerHierarchyToGenerate, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables, NumberOfStrata = myStrata, NumberSampled = mySampled, NumberTotal = myTotal, SelectionMethods = myMethods)
  
  # The data we just geenerated is too random and won't pass validation or upload check - lets fix that now
  myNewTestData <- makeTestDataMoreRealistic(DataToUse = myTestData,CountryToUse=myCountry,YearToUse=myYear,MetierList= NULL,SpeciesList= NULL,RDBEScodeLists=allowedValues)
  
  # Lets validate our data (Optional)
  #errorsTestData <- validateTables(RDBESdata = myNewTestData, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, shortOutput = TRUE,framestoValidate = c("BV","DE","FM","FO","FT","LE","LO","OS","SA","SD","SL","SS","TE","VD","VS"))
  
  # Create a VD output file
  generateSimpleExchangeFile(typeOfFile = 'VD', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a SL output file
  generateSimpleExchangeFile(typeOfFile = 'SL', yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData,cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues)
  
  # Create a complex exchange file (Hx)
  generateComplexExchangeFile(typeOfFile = myHierarchyToGenerate, yearToUse = myYear, country = myCountry, RDBESdata = myNewTestData, cleanData = TRUE, RDBESvalidationdata = validationData, RDBEScodeLists = allowedValues, RequiredTables = allRequiredTables)
  
}
