#' Generates the DBE estimation object for the upper hierarchy tables
#'
#' @param inputList All the data tables in a named list. Name should be equal
#' to the short table names e.g. DE, SD, TE, FO. An example can be found at
#' the share point:
#' https://community.ices.dk/ExpertGroups/WKRDB/2019\%20Meetings/
#' WKRDB-EST\%202019/06.\%20Data/Kirsten/H1/H1_upper.RData
#'
#' @export
#' @return The upper hierarchy tables in the DBE estimation object
#' (DBEestimantionObjUpp)
#'
#' @examples
#' \dontrun{
#' H1 <-
#' readRDS("./WKRDB-EST2/testData/output/DBErawObj/DBErawObj_DK_1966_H1.rds")
#' H1out <- doDBEestimantionObjUpp(H1)
#' }


doDBEestimantionObjUpp <-
  function(inputList) {

    hierarchy <- unique(inputList$DE$DEhierarchy)
    if (is.null(hierarchy))
      stop("Cannot identify the hierarchy from the DE table.
           Is it missing or empty?")
    if (length(hierarchy) > 1)
      stop("There is more than one hierarchy in the DE table. I cant cope!")

    # kibi - function stops when inputting these hierarchies
    if (hierarchy %in% c(5, 8, 12, 13))
      stop(paste("Stop: The function can not handle hierarchy 5, 8, 12 & 13"))

    # kibi - warnings for the ones where the link to the middle may be wrong
    if (hierarchy %in% c(6, 7, 11))
      warning(
        paste("Warning: The function may not handle the link
              to the middle hierarchies correctly"))

    # Variable names for the output
    varNames <- c(
      "idAbove",
      "id",
      "hierarchy", # kibi - corrected spelling all over the script
      "su",
      "recType",
      "unitName",
      "stratification",
      "stratumName",
      "clustering",
      "clusterName",
      "numTotal",
      "numSamp",
      "selProb",
      "incProb",
      "selectMeth",
      "selectMethCluster",
      "numTotalClusters",
      "numSampClusters",
      "selProbCluster",
      "incProbCluster"
    )

    # check variable names against the input
    a <-
      lapply(seq_along(inputList), function(i)
        data.frame(
          TableName = names(inputList)[i],
          FieldName = names(inputList[[i]])
        ))
    a <-
      lapply(a, function(x) {
        x$fieldNameStripped <-
          sub(x$TableName[1], "", x$FieldName)
        return(x)
      })
    lut <- do.call("rbind", a)
    varNamesNotInTables <-
      varNames[!varNames %in% lut$fieldNameStripped]

    # idAbove and su are not in the tables, dont want to give a warning
    varNamesNoWarning <- c("idAbove", "su")
    varNamesNotInTables <-
      varNamesNotInTables[!varNamesNotInTables %in% varNamesNoWarning]
    if (length(varNamesNotInTables) > 0)
      warning(
        paste(
          "Some unexpected var_names hardcoded in this function: ",
          paste(varNamesNotInTables, collapse =
                  ", "),
          "\nMaybe wrong data model version or a bug."
        )
      )


    # createing a list with expected tables for each hierarchy
    expectedTables <- list(
      H1 = data.frame(
        table_names = c("DE", "SD", "VS", "FT", "FO"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H2 = data.frame(
        table_names = c("DE", "SD", "FT", "FO"),
        su_level = c("NA", "NA", "su1", "su2")
      ),
      H3 = data.frame(
        table_names = c("DE", "SD", "TE", "VS", "FT", "FO"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
      ),
      H4 = data.frame(
        table_names = c("DE", "SD", "OS", "FT", "LE"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H5 = data.frame(
        table_names = c("DE", "SD", "OS", "LE"),
        su_level = c("NA", "NA", "su1", "su2")
      ),
      H6 = data.frame(
        table_names = c("DE", "SD", "OS", "FT"),
        su_level = c("NA", "NA", "su1", "su2")
      ),
      H7 = data.frame(
        table_names = c("DE", "SD", "OS"),
        su_level = c("NA", "NA", "su1")
      ),
      H8 = data.frame(
        table_names = c("DE", "SD", "TE", "VS", "LE"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H9 = data.frame(
        table_names = c("DE", "SD", "LO", "TE"),
        su_level = c("NA", "NA", "su1", "su2")
      ),
      H10 = data.frame(
        table_names = c("DE", "SD", "VS", "TE", "FT", "FO"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
      ),
      H11 = data.frame(
        table_names = c("DE", "SD", "LO", "TE", "FT"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H12 = data.frame(
        table_names = c("DE", "SD", "LO", "TE", "LE"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H13 = data.frame(
        table_names = c("DE", "SD", "FO"),
        su_level = c("NA", "NA", "su1")
      )
    )


    ### Need to include the DE - not as a SU, but I do have
    # info about stratification
    de <- inputList$DE

    ### Do we need to include the SD - that the link between the DE and PSU
    # - rename DEid to idAbove
    sd <- inputList$SD
    names(sd) <-
      sub("SD", "", names(sd))

    expectedTablesHere <-
      eval(parse(text = paste0("expectedTables$H", hierarchy)))

    out <-
      list(
        expectedTables = data.frame(hierarchy = hierarchy, expectedTablesHere),
        de = de,
        sd = sd
      )

    ### Importing the SU tables

    # check that the inputList has all the expected tables
    missingTables <-
      expectedTablesHere$table_names[
        !expectedTablesHere$table_names %in% names(inputList)
        ]
    if (length(missingTables) > 0)
      stop(paste(
        "Not all expected tables are in the inputList:",
        paste(missingTables, collapse =
                " ,")
      ))

    # CC has some code to do this better, not assume to start at 3
    for (i in c(3:length(expectedTablesHere$table_names))) {
      su <-
        eval(parse(text = paste0(
          "inputList$", expectedTablesHere$table_names[[i]]
        )))

      names(su) <-
        sub(unique(expectedTablesHere$table_names[[i]]), "", names(su))

      su$su <- expectedTablesHere$su_level[[i]]
      su$hierarchy <- hierarchy
      h <- i - 1
      su$idAbove <-
        eval(parse(text = paste0(
          "su$", expectedTablesHere$table_names[[h]], "id"
        )))

      # select relevant columns
      j <- which(names(su) %in% varNames)
      
      eval(parse(
        text = paste0(
          expectedTablesHere$su_level[[i]],
          "_done",
          "<- su[,j]"
        )
      ))


      # Create list with the design variables
      eval(parse(
        text = paste0(
          "out$",
          expectedTablesHere$su_level[[i]],
          " = ",
          expectedTablesHere$su_level[[i]],
          "_done"
        )
      ))

      # Create list with the inclusion probabilities

      # Create list with the selection probabilities

      # Create list with combined inclusion probabilities


    }

    return(out)
  }
