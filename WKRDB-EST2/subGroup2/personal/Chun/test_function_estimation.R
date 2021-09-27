## function to estimate parameter

## return the corresponding hierachy tables from the hierachy index
define_hierachy <- function(hierachy_ind) {
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
      table_names = c("DE", "SD", "OS", "FT", "FO"),
      su_level = c("NA", "NA", "su1", "su2", "su3")
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
  
  ## get the hierachy table
  expected_tables_here <-
    eval(parse(text = paste0("expected_tables$H", hierachy_ind)))
  
  return(expected_tables_here)
}


##?? param = "Haul duration": is this parameter unique among tables? 
## if i want to estimate haul duration, the observed measurements are in FO table only
## if I want to estimate mean length, the observed measurements are in SA
myfun <- function(param = "Haul duration", 
                  level = "trip"
                  estimator = "HT",
                  input_list = H1_upper) {
  
  
  ## step 1: find the hierachy tables of input data
  ## find the hierachy
  myhierachy_ind <- unique(input_list[["DE"]]$DEhierarchy)
  mytable        <- define_hierachy(myhierachy_ind)
  
  ## step 2: find the table (corresponds to level) where the estimation is up to
  ## translate level name into table name
  mylevel        <- switch(level, "vessel" = "VS", "trip" = "FT", "haul" = "FO")
  mylevel        <- switch(level, "trip" = mytable$su_level[mytable$table_names == "FT"])
  
  ## step 3: find the table and variable name for parameter:  
  ## it is either observed in higher level tables or estimated from lower level tables
  ## translate parameter name into variable name in the table
  mypara         <- switch(param, "Haul duration" = "FOdur", "length" = "FO_est_length")
  
  
  
  
  
  
}