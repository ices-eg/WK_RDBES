
library(openxlsx)

data_model <- "Q:/mynd/RDB/RDBES/Documents/RDBES Data Model.xlsx"
data_model_vd_sl <- "Q:/mynd/RDB/RDBES/Documents/RDBES Data Model VD SL.xlsx"
out_path <- "Q:/mynd/RDB/WKRDB-EST/WK_RDBES/WKRDB-EST2/testData/referenceData/"

mapColNames <- c()

for (i in 2:14) {
  dat_0 <- read.xlsx(data_model, sheet = i)[, c("Field.Name", "R.Name")]
  
  mapColNames <- rbind(mapColNames, dat_0)
}

for (i in 1:2) {
  dat_0 <- read.xlsx(data_model_vd_sl, sheet = i)[, c("Field.Name", "R.Name")]
  
  mapColNames <- rbind(mapColNames, dat_0)
}

mapColNamesUniq <- unique(mapColNames[c("Field.Name", "R.Name")])

# Test
mapColNamesUniq_1 <- unique(mapColNames[c("Field.Name")])
mapColNamesUniq_2 <- unique(mapColNames[c("R.Name")])

saveRDS(mapColNames, paste0(out_path, "mapColNamesFieldR.rds"))

