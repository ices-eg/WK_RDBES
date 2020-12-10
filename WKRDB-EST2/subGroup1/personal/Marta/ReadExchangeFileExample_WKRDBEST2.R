#Coppied from https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles
# with some changes by M. Suska

# The scripts takes Hx.csv and produces a set of csv's (TR.csv, DE.csv, ...)

## Example of how to import exchange files - assumes exchange files are saved in
## the folder ./output/uploaded

# Load our functions
source("RDBES_Functions.R")

# IMPORTANT: Hack to stop write.csv changing numbers to scientific notation
options(scipen=500) # big number of digits

## STEP 1) LOAD OUR DATA

# Load the validation data
#validationData <- getValidationData(downloadFromGitHub = FALSE, fileLocation = './tableDefs/BaseTypes.xsd')
validationData <- getValidationData(downloadFromGitHub = TRUE, fileLocation = './tableDefs/BaseTypes.xsd')

# 11/9/2020 Temp fix because the validation fields aren't up to date :-(
validationData[validationData$type == 'tRS_Stratification','type'] <- 'tYesNoFields'

# Load the reference data: either refresh from ICES or just use a local copy
#allowedValues <- loadReferenceData(downloadFromICES = FALSE)
allowedValues <- loadReferenceData(downloadFromICES = TRUE, validationData=validationData)

# Load the lists of tables required for each hierarchy: either refresh from ICES or just use a local copy
#allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = './tableDefs/')
allRequiredTables <- getTablesInHierarchies(downloadFromGitHub = TRUE, fileLocation = './tableDefs/')

myExchangeFileVD <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_HVD.csv' )
myExchangeFileSL <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_HSL.csv' )
myExchangeFileH1 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H1.csv',RequiredTables = allRequiredTables )
myExchangeFileH2 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H2.csv',RequiredTables = allRequiredTables )
myExchangeFileH3 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H3.csv',RequiredTables = allRequiredTables )
myExchangeFileH4 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H4.csv',RequiredTables = allRequiredTables )
myExchangeFileH5 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H5.csv',RequiredTables = allRequiredTables )
myExchangeFileH6 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H6.csv',RequiredTables = allRequiredTables )
myExchangeFileH7 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H7.csv',RequiredTables = allRequiredTables )
myExchangeFileH8 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H8.csv',RequiredTables = allRequiredTables )
myExchangeFileH9 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H9.csv',RequiredTables = allRequiredTables )
myExchangeFileH10 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H10.csv',RequiredTables = allRequiredTables )
myExchangeFileH11 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H11.csv',RequiredTables = allRequiredTables )
myExchangeFileH12 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H12.csv',RequiredTables = allRequiredTables )
myExchangeFileH13 <- readExchangeFile(RDBESvalidationdata = validationData, nameOfFile = 'D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/output/uploaded/DK_1966_H13.csv',RequiredTables = allRequiredTables )

# Update 15092020 M.Suska
for (Hierarchy in paste('H', c(1:13), sep = '')){



dir.create(file.path(paste('D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/subGroup1/personal/Marta/RDBESextract/DK1966ESP-AZTI_DCF_Onboard_Sampling', Hierarchy, sep = '')), showWarnings = FALSE)

write.csv(myExchangeFileVD$VD,
          file = paste('D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/subGroup1/personal/Marta/RDBESextract/DK1966ESP-AZTI_DCF_Onboard_Sampling',Hierarchy,'/VD.csv', sep = ''),
          row.names =  FALSE)
write.csv(myExchangeFileSL$SL,
          file = paste('D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/subGroup1/personal/Marta/RDBESextract/DK1966ESP-AZTI_DCF_Onboard_Sampling',Hierarchy,'/SL.csv', sep = ''),
          row.names =  FALSE)

for (i in names(eval(parse(text = paste('myExchangeFile', Hierarchy, sep = ''))))){
  write.csv(eval(parse(text = paste('myExchangeFile', Hierarchy, sep = '')))[[i]],
            file = paste('D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/subGroup1/personal/Marta/RDBESextract/DK1966ESP-AZTI_DCF_Onboard_Sampling',Hierarchy,'/',i,'.csv', sep = ''),
            row.names =  FALSE)
}

}
# Some checks if the rows are being linked up correctly
