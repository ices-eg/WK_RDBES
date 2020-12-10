
dataset<-readRDS("Inputs/DBErawObj_DK_1966_H1.rds")
dataset<-readRDS("Inputs/DBErawObj_DK_1966_H3.rds")
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
# example
generateProbs (x=y, probType = "selection")
generateProbs (x=y, probType = "inclusion")


y$value<-rnorm(nrow(y))



# HT estimation

ls1<-split(y, y$SDid)
ls2<-lapply(ls1, function(x) {
a <- unique(x$stratification)

# checks on stratification var
	if(sum(is.na(a))>0) stop ("missing values in stratification")
	if(!any(a %in% c("Y","N"))) stop ("stratification not defined - must be Y or N")

# procedure non-stratified
	if( a == "N" )
		{
			wi <- 1/generateProbs(x, probType="inclusion")
			HTi <- wi*x$value
			HTi
		}
	
# procedure stratified	
	if( a == "Y" )
		{
		ls1<-split(x, x$stratum)
		ls2<-lapply(ls1, function(x) { 
				wi <- 1/generateProbs(x, probType="inclusion")
				HTi <- wi*x$value
				HTi
				})
		 HTi<-do.call("rbind",ls2)		
		}
HTi	
})


if(all(y$selectMeth %in% c("SRSWOR","UPSWOR"))) HT <- sum(ls2[[1]]); HT	
if(all(y$selectMeth %in% c("SRSWR","UPSWR"))) HT <- sum(ls2[[1]][!duplicated(y$TEnationalCode)])	# requires identification of what is unique TEnationalCode
	
sum(y$value)*1.5



# select and isolate id_var, parent_id_var, and design_vars to a table






TEstratification, TEstratum, TEclustering TEclusterName, TEtotal TEsampled TEsampProb, TEselectionMethod



VStotal/VSsampled





# identify stratification
grepl(c1,pat="stratification")


selectionMethod
c1<-colnames(x)
table(x[,grepl(c1,pat="selectionMethod")])





# selection probs

	# SRSWR
		1/VS$VStotal
	
	# SRSWOR
		# order dependent - needs to be specified (but selection!) or assumed order of observations e order in draw	
	
	# UPSWR
		# needs to be specified (but selection!)

	# UPSWOR
		# Order and history dependent - needs to be specified (but selection!)

# inclusion probs
	
	# SRSWR
		1-(1-1/VS$VStotal)^VS$VSsampled
	
	# SRSWOR
		VS$VSsampled / VS$VStotal
	
	# UPSWR
		# (1-(1-pi)^n) where pi is selection prob
		VS$VSsampProb

	# UPSWOR
		# order dependent (difficult to calculate - involves pairwise Joint Inclusion Probabilities)
		VS$VSsampProb


# Multi-Stage designs
		
		# one selection method
		
			# SRSWR [slide 15]
				# selection
					# multiplication of selection probs at different stages
				# inclusion
					# multiplication of inclusion probs at different stages
			 
			# SRSWOR [slide 16]
				# selection
					# order dependent - not generally used in estimation 
				# inclusion
					# multiplication of the inclusion probabilities
		
		# combination of selection methods
			# UPSWR (PSU) + SRSWR (SSU and TSU)	[slide 18, 20]
				# selection
					# multiplication of selection probs at different stages - pi for UPSWR needs to be specified
				# inclusion
					# multiplication of selection probs at different stages - pi for UPSWR needs to be specified
					
			# UPSWR (PSU) + SRSWOR (SSU and TSU) [slide 19, 20]					
				# selection
					# order dependent - not generally used in estimation
				# inclusion				
					# multiplication of inclusion probs at different stages - pi for UPSWOR needs to be specified
					
					
					
