### re-issue H1 testData to test 0 generation from SS/SL/SA based on David Currie script  
# MI_RDBES_ExchangeFiles/CreateTestData.R 
# Kasia Krakówka NMFRI

# Load our functions
source("D:/WK_RDBES/WKRDB-EST2/subgroup4/personal/Kasia/RDBES_Functions.R")

# This file shows how to generate test data for the RDBES

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
#validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# 11/9/2020 Temp fix because the validation fields aren't up to date :-(
validationData[validationData$type == 'tRS_Stratification','type'] <- 'tYesNoFields'

# Load the reference data: either refresh from ICES or just use a local copy
allowedValues <- loadReferenceData(downloadFromICES = FALSE)
#allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

## STEP 2) GENERATE TEST DATA

  # Define some parameters for our test data
  myHierarchyToGenerate <- 'H1'
  print(myHierarchyToGenerate)
  myLowerHierarchyToGenerate <- 'A'
  myYear <- 2015
  myCountry <- 'IE'
  # Number of strata in different tables - if no value if given for a table then it is assumed to be unstratified
  myStrata <- list(DE = 2, VS = 2)
  # Number of things sampled in different tables - if no value is given for a table then it is assumed to be 1
  mySampled <- list(VS=1,FO=4,SS=2,SA=2, FM=0,BV=0, VD=1, SL=6)
  # Total number of things in different tables - if no value is given for a table then it is assumed to be equal to the number sampled + 1
  myTotal <- list(VS=1,FO=4,SS=2, FM=0, BV=0)
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