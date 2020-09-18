library(devtools)



#################################################################################################################
# load all the functions and datasets that are needed
#################################################################################################################

# script with getTablesInHierarchies function among others
source_url(
  "https://raw.githubusercontent.com/davidcurrie2001/MI_RDBES_ExchangeFiles/master/RDBES_Functions.R"
)
# and leave only the function that we need
rm(list=setdiff(ls(), "getTablesInHierarchies"))

# data model defined in the xsd files
hierarchies_path <- "D:/Projekty/RDBES/RDBES/XSD-files/"
allRequiredTables <-
  getTablesInHierarchies(downloadFromGitHub = FALSE, fileLocation = hierarchies_path)

# mapping of Field Names to R names
mapColNamesFieldR <- readRDS("D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/testData/referenceData/mapColNamesFieldR.rds")
#################################################################################################################
#################################################################################################################




#' doDBErawObj
#' Function that reads all the csv's per table and sets up DBErawObj
#' based on the scripts from the repo  https://raw.githubusercontent.com/davidcurrie2001/MI_RDBES_ExchangeFiles
#' 
#' @param RDBESextractPath Path where the folder with the csvs exported from the RDBES are being stored. If the user does not specify it,
#' an interactive window will be launched, so that the user can choose the directory
#' @param DBErawPath Path where the function is supposed to write the DBErawObjs in. If not defined, it will be stored in the parent 
#' of RDBESextractPath
#'
#' @return DBErawObj - the list containing all the tables that create the given hierarchy
#' @export
#'
#' @examples
#' doDBErawObj()
doDBErawObj = function(RDBESextractPath = NA,
                       DBErawPath = NA) {
  
  #######################################################################################################################
  # the directories
  #######################################################################################################################
  
  # set the directory where the csv downloaded from the RDBES are being stored
  # if it is not defined in the parameters, then open the interactive window to enable the user to choose the dir
  RDBESextractPath = ifelse(is.na(RDBESextractPath), choose.dir(caption = 'Select folder with the csv files' ), RDBESextractPath)
  ifelse(!dir.exists(RDBESextractPath), 'The directory not found', FALSE)
  
  # set the directory for DBErawObj
  # if it is nof defined in the parameters, save in the parent of the parent of the RDBESextractPath
  # and print out the message on where the user can find the results
  DBErawPath = ifelse(is.na(DBErawPath), paste(dirname(dirname(RDBESextractPath)), '/DBEraw', sep = ''), DBErawPath)
  ifelse(!dir.exists(DBErawPath), dir.create(DBErawPath), FALSE)
  
  # check if the proper tables are in the specified directory
  list_files <- grep('csv', list.files(RDBESextractPath), value = TRUE)
  list_files_names = gsub(".csv", "", list_files)
  
  # DE and SD must be in the given directory
  mustHaves = c('DE', 'SD', 'SL', 'VD')
  
  if(all(mustHaves %in% list_files_names)){
    
    DE = read.csv(paste(RDBESextractPath, '/DE.csv', sep = ''), stringsAsFactors = FALSE)
    SD = read.csv(paste(RDBESextractPath, '/SD.csv', sep = ''), stringsAsFactors = FALSE)
    Country = unique(SD$SDcountry)
    Year = unique(DE$DEyear)
    Hierarchy = unique(DE$DEhierarchy)
    SamplingScheme = unique(DE$DEsamplingScheme)
    
    if(length(c(Country, Year, Hierarchy, SamplingScheme)) > 4){
      stop('The function will not work yet, when in the DE, there are different Sampling designs or Years or Hierarchies or Countries defined.')
    }
    
  } else{
    
    mustHavesMissing = setdiff(mustHaves, list_files_names)
    stop(paste(paste0(mustHavesMissing, collapse = ' '),'missing from the directory: ', RDBESextractPath,'. Please check if the given directory is correct.', sep = ''))
    
  }
  
  requiredTables = allRequiredTables[[paste('H',Hierarchy, sep = '')]]
  
  # TO DO:
  # as required tables depend on the chosen lower hierarchy, the checks should be carried out only on the upper tables
  # and then depending on the lower hierarchy defined - additional check if the proper tables are included
  # requiredTablesUpp = setdiff(requiredTables, c('FM', 'BV')) 
  
  if(!all(requiredTables %in% list_files_names)){
    
    missings = setdiff(requiredTables, list_files_names)
    stop(paste(
      'For the hierarchy ', Hierarchy, ' which has beed specified in the DE.csv, teh following tables are required: ', paste0(requiredTables, collapse = ', '), '.\n',
      'The following tables: ', missings, ' are missing from the directory: ', RDBESextractPath,'. Please check if the given directory is correct.', sep = ''))
    
  }
  
  #######################################################################################################################
  # load the data
  #######################################################################################################################  
  
  for (i in list_files_names) {
    assign(i, read.csv(paste(RDBESextractPath, '/', i, '.csv', sep = ''), stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM", na.strings = c("NA","NULL")))
    
  }
  
  #######################################################################################################################
  # check the data
  #######################################################################################################################  
  
  # TO DO
  # check all the columns
  # mapColNames...
  
  # TO DO:
  # should we implement any other checks?
  
  #######################################################################################################################
  # create the DBErawObj
  #######################################################################################################################  
  
  # set up DBErawObj
  DBErawObj = list()
  for (i in 1:length(requiredTables)) {
    name = requiredTables[i]
    DBErawObj[[name]] = eval(parse(text = requiredTables[i]))
  }
  
  # At the end add SL and VD
  DBErawObj[['SL']] = SL
  DBErawObj[['VD']] = VD
  
  # rename colnames from fiels named to r names
  for (i in names(DBErawObj)) {
    eval(parse(
      text = paste0(
        "names(DBErawObj$",
        i,
        ") <- mapColNamesFieldR$R.Name[match(tolower(names(DBErawObj$",
        i,
        ")), tolower(mapColNamesFieldR$Field.Name))]"
      )
    ))
    
  }
  
  #######################################################################################################################
  # save the data
  #######################################################################################################################  
  
  param_string = paste(Country, Year, paste('H',Hierarchy, sep = ''), sep = '_')
  # save each file in a separate subfolder or keep all the files in a one folder calles DBEraw
  #DBErawPathSubfolder = paste(DBErawPath, param_string, sep = '/')
  #ifelse(!dir.exists(DBErawPathSubfolder), dir.create(DBErawPathSubfolder), FALSE)
  #saveRDS(DBErawObj, file = paste(DBErawPathSubfolder, '/DBErawObj_', param_string, '.rds', sep = ''))
  saveRDS(DBErawObj, file = paste(DBErawPath, '/DBErawObj_', param_string, '.rds', sep = ''))
  print(paste('DBErawObj was saved here ->', DBErawPath))
}

#doDBErawObj()
