# Example use generateProbs


dataset<-readRDS("Inputs/DBErawObj_DK_1966_H1.rds")
load("Inputs/RDBES_DataModel.RData")

# isolates columns for estimation (estimNames)

# isolate DVs
target_table<-"VS"
var_table<-dataModel[dataModel$Table==target_table,]
dv_vars<-var_table[grepl(var_table$Required, pat="DV"),"R.Name"]
id_vars<-var_table[grepl(var_table$Field.Name, pat="id"),"R.Name"]
vars_keep<-c(id_vars, dv_vars)

y<-dataset$VS

colnames(y)[colnames(y) %in% dv_vars]<-gsub(target_table, "", colnames(y)[colnames(y) %in% dv_vars])

source("funs/generateProbs.R")
generateProbs (x=y, probType = "selection")
generateProbs (x=y, probType = "inclusion")