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

generic_su_object_upper_hie <- function(input_list = H1_upper, hierachy = 1) {
  
  library(dplyr)
  
  
  # Varibale names for the output ----

  var_names <- c("hierachy", "su", "recType", "idAbove", "id",
                 "stratification", "stratum", "clustering", "clusterName", 
                 "total", "sampled", "prob", "selectMeth", "selectMethCluster", "totalClusters", 
                 "sampledClusters", "probCluster")
  
  # createing a list with expected tables for each hierachy ----
  expected_tables <- list(
    H1 = data.frame(
      table_names = c("DE", "SD", "VS", "FT", "FO","SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
    ),
    H2 = data.frame(
      table_names = c("DE", "SD", "FT", "FO","SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3")
    ),
    H3 = data.frame(
      table_names = c("DE", "SD", "TE", "VS", "FT", "FO","SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5")
    ),
    H4 = data.frame(
      table_names = c("DE", "SD", "OS", "FT", "LE","SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
    ),
    H5 = data.frame(
      table_names = c("DE", "SD", "OS", "LE","SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3")
    ),
    H6 = data.frame(
      table_names = c("DE", "SD", "OS", "FT", "SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3")
    ),
    H7 = data.frame(
      table_names = c("DE", "SD", "OS","SA"),
      su_level = c("NA", "NA", "su1", "su2")
    ),
    H8 = data.frame(
      table_names = c("DE", "SD", "TE", "VS", "LE","SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
    ),
    H9 = data.frame(
      table_names = c("DE", "SD", "LO", "TE", "SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3")
    ),
    H10 = data.frame(
      table_names = c("DE", "SD", "VS", "TE", "FT", "FO","SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3", "su4", "su5")
    ),
    H11 = data.frame(
      table_names = c("DE", "SD", "LO", "TE", "FT", "SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
    ),
    H12 = data.frame(
      table_names = c("DE", "SD", "LO", "TE", "LE","SA"),
      su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
    ),
    H13 = data.frame(
      table_names = c("DE", "SD", "FO","SA"),
      su_level = c("NA", "NA", "su1", "su2")
    )
  )
  
  

  out <- list()


  expected_tables_here <- eval(parse(text = paste0('expected_tables$H', hierachy)))
  
  # Creating the su data.frames -----
  
  for (i in c(3:length(expected_tables_here$table_names))) {
  
  su <- eval(parse(text = paste0('input_list$', expected_tables_here$table_names[[i]])))
    
  names(su) <- sub(unique(expected_tables_here$table_names[[i]]), "", names(su))
  
  su$su <- as.character(expected_tables_here$su_level[[i]])
  su$hierachy <- hierachy
  h <- i - 1
  su$idAbove <- eval(parse(text = paste0('su$', expected_tables_here$table_names[[h]], "id")))
  
  eval(parse(text = paste0(expected_tables_here$su_level[[i]], "_done", "<- select(su, one_of(var_names))")))
  
  
  eval(parse(text = paste0("out$", expected_tables_here$su_level[[i]], "$designTable", " = ", expected_tables_here$su_level[[i]], "_done")))
  
  }
  
  
  
  return(out)

  
}
