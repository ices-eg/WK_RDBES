library(openxlsx)

rdbesDataModel <- "data-raw\\RDBES Data Model.xlsx"
rdbesDataModelVDSL <- "data-raw\\RDBES Data Model VD SL.xlsx"
rdbesDataModelCLCE <- "data-raw\\RDBES Data Model CL CE.xlsx"

outFile <- "data\\mapColNamesFieldR.RData"

mapColNames <-NULL

for (i in 2:14) {

  dat_0 <- read.xlsx(rdbesDataModel, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_0[1,"Field.Name"],1,2)
  }
  dat_0$Table.Prefix <- tablePrefix

  mapColNames <- rbind(mapColNames, dat_0[,c("Table.Prefix","Field.Name", "R.Name")])
}


for (i in 1:2) {
  dat_1 <- read.xlsx(rdbesDataModelVDSL, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_1[1,"Field.Name"],1,2)
  }
  dat_1$Table.Prefix <- tablePrefix

  mapColNames <- rbind(mapColNames, dat_1[,c("Table.Prefix","Field.Name", "R.Name")])
}

for (i in 1:2) {
  dat_2 <- read.xlsx(rdbesDataModelCLCE, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_2[1,"Field.Name"],1,2)
  }
  dat_2$Table.Prefix <- tablePrefix

  mapColNames <- rbind(mapColNames, dat_2[,c("Table.Prefix","Field.Name", "R.Name")])
}

# Get rid of NA field names
mapColNamesFieldR <- mapColNames[!is.na(mapColNames$Field.Name),]

# Fix for one issue
mapColNamesFieldR[mapColNamesFieldR$R.Name == "Clid","Field.Name"] <- "CLid"
mapColNamesFieldR[mapColNamesFieldR$R.Name == "Clid","R.Name"] <- "CLid"

# Save the data
save(mapColNamesFieldR,file=outFile)
