# Script that reads all the csv's per table and sets up DBErawObj

# to do:
# all hierarchies

library(devtools)
source_url(
  "https://raw.githubusercontent.com/davidcurrie2001/MI_RDBES_ExchangeFiles/master/RDBES_Functions.R"
)
hierarchies_path <- "D:/Projekty/RDBES/RDBES/XSD-files/"
allRequiredTables <-
  getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = hierarchies_path)

# temp variables
# Year = 1966
# Country = 'DK'
# SamplingScheme = 'ESP-AZTI_DCF_Onboard_Sampling'
# Hierarchy = 'H3'

doDBErawObj = function(Year,
                       Country,
                       SamplingScheme,
                       Hierarchy,
                       RDBESextractPath,
                       DBErawPath) {
  
  # set the directory folders
  data_dir = paste(RDBESextractPath,
                   '/',
                   Country,
                   Year,
                   SamplingScheme,
                   Hierarchy,
                   sep = '')
  DBErawPath_dir = paste(DBErawPath,
                         '/',
                         Country,
                         Year,
                         SamplingScheme,
                         Hierarchy,
                         sep = '')
  
  # to do: check if the proper tables are in the specified directory
  list_files <- grep('csv', list.files(data_dir), value = TRUE)
  allRequiredTables[[Hierarchy]]
  
  for (i in list_files) {
    name =  gsub(".csv", "", i)
    assign(name, read.csv(paste(data_dir, '/', i, sep = '')))
    
  }
  
  # to do: check all the columns
  # mapColNames...
  
  # to do: should we implement any other checks?
  
  # set up DBErawObj
  DBErawObj = list()
  for (i in 1:length(allRequiredTables[[Hierarchy]])) {
    name = allRequiredTables[[Hierarchy]][i]
    DBErawObj[[name]] = eval(parse(text = allRequiredTables[[Hierarchy]][i]))
  }
  
  # At the end add SL and VD
  DBErawObj[['SL']] = SL
  DBErawObj[['VD']] = VD
  
  
  dir.create(file.path(DBErawPath_dir), showWarnings = FALSE)
  saveRDS(DBErawObj, file = paste(DBErawPath_dir, '/DBErawObj.rds', sep = ''))
}

# doDBErawObj(
#   1966,
#   'DK',
#   'ESP-AZTI_DCF_Onboard_Sampling',
#   'H5',
#   RDBESextractPath = './WKRDB-EST2/subGroup1/personal/Marta/RDBESextract',
#   DBErawPath = './WKRDB-EST2/subGroup1/personal/Marta/DBEraw'
# )
