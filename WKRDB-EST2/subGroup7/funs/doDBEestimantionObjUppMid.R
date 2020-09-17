#' Generates the DBE estimation object for the upper hierarchy tables
#'
#' @param input_list All the data tables in a named list. Name should be equal 
#' to the short table names e.g. DE, SD, TE, FO. An example can be found at the share point: 
#' https://community.ices.dk/ExpertGroups/WKRDB/2019%20Meetings/WKRDB-EST%202019/06.%20Data/Kirsten/H1/H1_upper.RData
#' 
#'
#' @return The upper hierarchy tables in the DBE estimation object (DBEestimantionObjUpp)
#'
#' @examples
#' \dontrun{
#' H1 <- readRDS('./WKRDB-EST2/testData/output/DBErawObj/DBErawObj_DK_1966_H1.rds')
#' H1out <- doDBEestimantionObjUpp(H1)
#' }


doDBEestimantionObjUpp <-
  function(input_list) { 

    library(dplyr)
    
    # hg
    # we dont have to specify the hierarchy in the function
    # note: i kept the typo in here for now
    hierarchy <- unique(input_list$DE$DEhierarchy)
    if (is.null(hierarchy))
      stop('Cannot identify the hierarchy from the DE table. Is it missing or empty?')
    if (length(hierarchy) > 1)
      stop('There is more than one hierarchy in the DE table. I cant cope!')
    
    # kibi - function stops when inputting these hierarchies
    if (hierarchy %in% c(5, 8, 12, 13))
      stop(paste("Stop: The function can not handle hierarchy 5, 8, 12 & 13"))
    
    # kibi - warnings for the ones where the link to the middle may be wrong
    if (hierarchy %in% c(6, 7, 11))
      warning(paste("Warning: The function may not handle the link to the middle hierarchies correctly"))
    
    # Variable names for the output
    var_names <- c(
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
    
    # hg
    # check variable names
    
    a <-
      lapply(seq_along(input_list), function(i)
        data.frame(
          TableName = names(input_list)[i],
          FieldName = names(input_list[[i]])
        ))
    a <-
      lapply(a, function(x) {
        x$FieldNameStripped <-
          sub(x$TableName[1], "", x$FieldName)
        return(x)
      })
    lut <- do.call('rbind', a)
    var_names_not_in_tables <-
      var_names[!var_names %in% lut$FieldNameStripped]
    
    # idAbove and su are not in the tables, dont want to give a warning
    varnames_no_warning <- c('idAbove', 'su')
    var_names_not_in_tables <-
      var_names_not_in_tables[!var_names_not_in_tables %in% varnames_no_warning]
    if (length(var_names_not_in_tables) > 0)
      warning(
        paste(
          'Some unexpected var_names hardcoded in this function: ',
          paste(var_names_not_in_tables, collapse =
                  ', '),
          '\nMaybe wrong data model version or a bug.'
        )
      )
    
    
    # createing a list with expected tables for each hierarchy
    # 20200917 kibi Added SS & SA to all hierarchies
    expected_tables <- list(
      H1 = data.frame(
        table_names = c("DE", "SD", "VS", "FT", "FO", "SS", "SA"), 
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5")
      ),
      H2 = data.frame(
        table_names = c("DE", "SD", "FT", "FO", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
      ),
      H3 = data.frame(
        table_names = c("DE", "SD", "TE", "VS", "FT", "FO", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5", "su6")
      ),
      H4 = data.frame(
        table_names = c("DE", "SD", "OS", "FT", "LE", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5")
      ),
      H5 = data.frame(
        table_names = c("DE", "SD", "OS", "LE", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
      ),
      H6 = data.frame(
        table_names = c("DE", "SD", "OS", "FT", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
      ),
      H7 = data.frame(
        table_names = c("DE", "SD", "OS", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H8 = data.frame(
        table_names = c("DE", "SD", "TE", "VS", "LE", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5")
      ),
      H9 = data.frame(
        table_names = c("DE", "SD", "LO", "TE", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
      ),
      H10 = data.frame(
        table_names = c("DE", "SD", "VS", "TE", "FT", "FO", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5", "su6")
      ),
      H11 = data.frame(
        table_names = c("DE", "SD", "LO", "TE", "FT", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5")
      ),
      H12 = data.frame(
        table_names = c("DE", "SD", "LO", "TE", "LE", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5")
      ),
      H13 = data.frame(
        table_names = c("DE", "SD", "FO", "SS", "SA"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      )
    )
    
    
    out <- list()
    
    ### Some general info from the DE to go into a description - for now just a copy
    # desc <- input_list$DE
    
    ### Need to include the DE - not as a SU, but I do have info about stratification
    de <- input_list$DE
    
    ### Do we need to include the SD - that the link between the DE and PSU - rename DEid to idAbove
    
    sd <- input_list$SD
    names(sd) <-
      sub("SD", "", names(sd))
    
    # kibi - moved these lines up
    expected_tables_here <-
      eval(parse(text = paste0("expected_tables$H", hierarchy))) 
    
    # hg   
    #out <- list(expected_tables = expected_tables, de = de, sd = sd)
    out <-
      list(
        expected_tables = data.frame(hierarchy = hierarchy, expected_tables_here),
        de = de,
        sd = sd
      )

    ### Importing the SU tables

    
    # hg
    # check that the input_list has all the expected tables
    missing_tables <-
      expected_tables_here$table_names[!expected_tables_here$table_names %in% names(input_list)]
    if (length(missing_tables) > 0)
      stop(paste(
        'Not all expected tables are in the input_list:',
        paste(missing_tables, collapse =
                ' ,')
      ))
    
    # CC has some code to do this better, not assume to start at 3    
    for (i in c(3:length(expected_tables_here$table_names))) {
      su <-
        eval(parse(text = paste0(
          "input_list$", expected_tables_here$table_names[[i]]
        )))
      
      names(su) <-
        sub(unique(expected_tables_here$table_names[[i]]), "", names(su))
      
      su$su <- expected_tables_here$su_level[[i]]
      su$hierarchy <- hierarchy
      h <- i - 1
      su$idAbove <-
        eval(parse(text = paste0(
          "su$", expected_tables_here$table_names[[h]], "id"
        )))
      
      eval(parse(
        text = paste0(
          expected_tables_here$su_level[[i]],
          "_done",
          "<- select(su, one_of(var_names))"
        )
      ))
      
      # names(su1_done) <-
      # c(c(paste0(unique(su1_done$recType), "id")),c(names(su1_done[, c(3:ncol(su1_done))])))
      # 
      # # Create list with the table name
      # eval(parse(
      #   text = paste0(
      #     "out$",
      #     expected_tables_here$su_level[[i]],
      #     "$name",
      #     " = ",
      #     "'",
      #     unique(su$recType),
      #     "'"
      #   )
      # ))
      
      # Create list with the design variables
      eval(parse(
        text = paste0(
          "out$",
          expected_tables_here$su_level[[i]],
          " = ",
          expected_tables_here$su_level[[i]],
          "_done"
        )
      ))
      
      # Create list with the inclusion probabilities
      
      # Create list with the selection probabilities
      
      # Create list with combined inclusion probabilities
      
      
    }
    
    return(out)
  }
