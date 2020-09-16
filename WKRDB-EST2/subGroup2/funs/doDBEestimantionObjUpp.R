#' generic_su_object_upper_hie
#'
#' @param input_list All the data tables in a named list. Name should be equal 
#' to the short table names e.g. DE, SD, TE, FO. An example can be found at the share point: 
#' https://community.ices.dk/ExpertGroups/WKRDB/2019%20Meetings/WKRDB-EST%202019/06.%20Data/Kirsten/H1/H1_upper.RData
#' @param hierachy The number of the hierachy you are inputting - 1 to 13
#' 
#'
#' @return
#' @export
#'
#' @examples
#' 


generic_su_object_upper_hie <-
  function(input_list = H1_upper,
           hierachy = 1) {
    library(dplyr)
    
    
    # Varibale names for the output
    var_names <- c(
      "idAbove",
      "id",
      "hierachy",
      "su",
      "recType",
      "stratification",
      "stratum",
      "clustering",
      "clusterName",
      "total",
      "sampled",
      "prob",
      "selectMeth",
      "selectMethCluster",
      "totalClusters",
      "sampledClusters",
      "probCluster"
    )
    
    # createing a list with expected tables for each hierachy
    expected_tables <- list(
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
    
    
    out <- list()
    
    ### Some general info from the DE to go into a description - for now just a copy
    # desc <- input_list$DE
    
    ### Need to include the DE - not as a SU, but I do have info about stratification
    de <- input_list$DE
    
        ### Do we need to include the SD - that the link between the DE and PSU - rename DEid to idAbove
    
    sd <- input_list$SD
    names(sd) <-
      sub("SD", "", names(sd))
    
    out <- list(de = de, sd = sd)
  
    
    ### Importing the SU tables
    
    expected_tables_here <-
      eval(parse(text = paste0("expected_tables$H", hierachy)))
    
    for (i in c(3:length(expected_tables_here$table_names))) {
      su <-
        eval(parse(text = paste0(
          "input_list$", expected_tables_here$table_names[[i]]
        )))
      
      names(su) <-
        sub(unique(expected_tables_here$table_names[[i]]), "", names(su))
      
      su$su <- expected_tables_here$su_level[[i]]
      su$hierachy <- hierachy
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
