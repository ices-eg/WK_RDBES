
#' @param input_list All the data tables in a named list. Name should be equal 
#' to the short table names e.g. DE, SD, TE, FO. An example can be found at the share point: 
#' https://community.ices.dk/ExpertGroups/WKRDB/2019%20Meetings/WKRDB-EST%202019/06.%20Data/Kirsten/H1/H1_upper.RData
#' @param parameter what you want to estimate (e.g. 'SAtotalWtLive', has to be a valid fieldname (R name i think)
#' @param level at what level do you want to estimate it (e.g. average and total live weight by trip ('TR') by main strata ('DE') etc)


DBEestimObjUpp <- (input_list,parameter,level,estimator) {

  hierachy <- input_list$DE$DEhierarchy[1] 
  # stop if there is more than 1 hierarchy
  
  de <- input_list$DE
  sd <- input_list$SE
  
  ## first add in the aux variables
  vs <- merge(input_list$VS,input_list$VD)
  ft <- merge(input_list$FT,input_list$VD)
  ss <- merge(input_list$SS,input_list$SL)
  # etc
  # is any of this hierarchy specific? probably some.
  
  # make a new input list (with aux variables)
  input_list_aux <- list(de,sd,vs,ft,ss) #etc
  
  ## find which table 'parameter' is in
  ## first generate a lookup table, which parameters are in which tables?
  a <- lapply(seq_along(input_list_aux), function(i) data.frame(TableName=names(input_list_aux)[i],FieldName=names(input_list_aux[[i]])))
  lut <- do.call('rbind',a)
  
  # find the paraTable and some warnings if it goes wrong
  paraTable <- lut$TableName[which(lut$FieldName==parameter)]
  paraTable <- as.character(paraTable)
  if(length(paraTable)==0) stop('parameter is not a valid field name; it has to be a name in one of the tables of the input_list')
  if(length(paraTable)>1) stop('parameter is not a unique field name, it occurs in: ',paste(paraTable, collapse=' and '))
  
  
  # specify parameters that may need to be used for estimation  
  est_var <- c(
    "selectionProb",
    "inclusionProb",
    "selectionMethod",
    "unitName",
    "selectMethCluster",
    "numTotalClusters",
    "numSampClusters",
    "selProbCluster",
    "incProbCluster",
    "samp"
    )

  expected_tables <- list(
    H1 = data.frame(
      table_names = c("DE", "SD", "VS",  "FT",  "FO"),
      su_level =    c("NA", "NA", "su1", "su2", "su3")
    )
    #etc
  )
  # now rename ONLY estvar eg from VSselectionProb to SU1selectionProb, the rest can stay VS (i think)
  
  # merge the whole lot (maybe also with aux tables)
  # make this generic with eval(parse())
  # only merge up to the level of paraTable
  
#  numberOfSu <- how many levels of SU are there? 1,2,etc
  
  supertable <- merge(merge(merge(DE,SD),VS),FT)FO)

  # estimation function to be defined
  out <- estimationfun(supertable,parameter,level,estimator,numberOfSU)

  return(out)
}