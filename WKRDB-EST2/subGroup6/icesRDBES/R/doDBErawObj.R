#' doDBErawObj
#' Function that reads all the csv's per table and sets up DBErawObj
#' based on the scripts from the repo
#' https://raw.githubusercontent.com/davidcurrie2001/MI_RDBES_ExchangeFiles
#'
#' @param rdbesExtractPath Path where the folder with the csvs exported from
#' the RDBES are being stored. If the user does not specify it,
#' an interactive window will be launched, so that the user can choose the
#' directory
#' @param dbeRawPath Path where the function is supposed to write the
#' DBErawObjs in. If not defined, it will be stored in the parent
#' of rdbesExtractPath
#'
#' @return DBErawObj - the list containing all the tables that create the
#' given hierarchy
#' @export
#'
doDBErawObj <- function(rdbesExtractPath = NA,
                       dbeRawPath = NA) {

  ##############################################################################
  # the directories
  ##############################################################################

  # Get allRequiredTables data from file in data folder
  allRequiredTables <- tablesInRDBESHierarchies


  # set the directory where the csv downloaded from the RDBES are being stored
  # if it is not defined in the parameters, then open the interactive window
  # to enable the user to choose the dir
  rdbesExtractPath <-
    ifelse(
      is.na(rdbesExtractPath),
      choose.dir(caption = "Select folder with the csv files"),
      rdbesExtractPath
    )
  ifelse(!dir.exists(rdbesExtractPath), "The directory not found", FALSE)

  # set the directory for DBErawObj
  # if it is nof defined in the parameters, save in the parent of the parent
  # of the RDBESextractPath
  # and print out the message on where the user can find the results
  dbeRawPath <-
    ifelse(is.na(dbeRawPath),
           paste(dirname(dirname(rdbesExtractPath)), "/DBEraw", sep = ""),
           dbeRawPath)
  ifelse(!dir.exists(dbeRawPath), dir.create(dbeRawPath), FALSE)

  # check if the proper tables are in the specified directory
  listFiles <-
    grep("csv", list.files(rdbesExtractPath), value = TRUE)
  listFilesNames <- gsub(".csv", "", listFiles)

  # DE and SD must be in the given directory
  mustHaves <- c("DE", "SD", "SL", "VD")

  if (all(mustHaves %in% listFilesNames)) {
    de <-
      read.csv(paste(rdbesExtractPath, "/DE.csv", sep = ""),
               stringsAsFactors = FALSE)
    sd <-
      read.csv(paste(rdbesExtractPath, "/SD.csv", sep = ""),
               stringsAsFactors = FALSE)
    #Country <- unique(sd$SDcountry)
    year <- unique(de$DEyear)
    hierarchy <- unique(de$DEhierarchy)
    samplingScheme <- unique(de$DEsamplingScheme)

    if (length(c(year, hierarchy, samplingScheme)) > 4) {
      stop(
        "The function will not work yet, when in the DE, there are different
        Sampling designs or Years or Hierarchies or Countries defined."
      )
    }
  } else {
    mustHavesMissing <- setdiff(mustHaves, listFilesNames)
    stop(
      paste(
        paste0(mustHavesMissing, collapse = " "),
        "missing from the directory: ",
        rdbesExtractPath,
        ". Please check if the given directory is correct.",
        sep = ""
      )
    )
  }

  requiredTables <-
    allRequiredTables[[paste("H", hierarchy, sep = "")]]

  # TO DO:
  # as required tables depend on the chosen lower hierarchy, the checks
  # should be carried out only on the upper tables
  # and then depending on the lower hierarchy defined - additional check if
  # the proper tables are included
  # requiredTablesUpp = setdiff(requiredTables, c("FM", "BV"))

  if (!all(requiredTables %in% listFilesNames)) {
    missings <- setdiff(requiredTables, listFilesNames)
    stop(
      paste(
        "For the hierarchy ",
        hierarchy,
        " which has beed specified in the DE.csv, teh following tables
        are required: ",
        paste0(requiredTables, collapse = ", "),
        ".\n",
        "The following tables: ",
        missings,
        " are missing from the directory: ",
        rdbesExtractPath,
        ". Please check if the given directory is correct.",
        sep = ""
      )
    )
  }

  ##############################################################################
  # load the data
  ##############################################################################

  for (i in listFilesNames) {
    assign(
      i,
      read.csv(
        paste(rdbesExtractPath, "/", i, ".csv", sep = ""),
        stringsAsFactors = FALSE,
        fileEncoding = "UTF-8-BOM",
        na.strings = c("NA", "NULL")
      )
    )
  }

  ##############################################################################
  # check the data
  ##############################################################################

  # TO DO
  # check all the columns
  # mapColNames...

  # TO DO:
  # should we implement any other checks?

  ##############################################################################
  # create the DBErawObj
  ##############################################################################

  # set up DBErawObj
  dbeRawObj <- list()
  for (i in 1:length(requiredTables)) {
    name <- requiredTables[i]
    dbeRawObj[[name]] <- eval(parse(text = requiredTables[i]))
  }

  # At the end add SL and VD
  dbeRawObj[["SL"]] <- SL
  dbeRawObj[["VD"]] <- VD

  #rename colnames from fiels named to r names
  for (i in names(dbeRawObj)) {
    eval(parse(
      text = paste0(
        "names(dbeRawObj$",
        i,
        ") <- mapColNamesFieldR$R.Name[match(tolower(names(dbeRawObj$",
        i,
        ")), tolower(mapColNamesFieldR$Field.Name))]"
      )
    ))

  }

  ##############################################################################
  # save the data
  ##############################################################################

  paramString <- paste(year, paste("H", hierarchy, sep = ""), sep = "_")
  # save each file in a separate subfolder or keep all the files in a one
  # folder calles DBEraw
  #dbeRawPathSubfolder = paste(dbeRawPath, paramString, sep = "/")
  #ifelse(!dir.exists(dbeRawPathSubfolder), dir.create(dbeRawPathSubfolder)
  # , FALSE)
  #saveRDS(dbeRawObj, file = paste(dbeRawPathSubfolder, "/dbeRawObj_",
  # paramString, ".rds", sep = ""))
  saveRDS(dbeRawObj,
          file = paste(dbeRawPath,
                       "/dbeRawObj_", paramString, ".rds", sep = ""))
  print(paste("dbeRawObj was saved here ->", dbeRawPath))
}

#doDBErawObj()
