
load("001_Inputs/H8/TE.RData")
load("001_Inputs/H8/VS.RData")
load("List_RDBES_Variables_v1.17.RData")

# isolates columns for estimation (estimNames)

# isolate DVs
var_table<-list_RDBES_Variables[list_RDBES_Variables$Table=="TE",]

dv_vars<-var_table[grepl(var_table$Required, pat="DV"),"R.Name"]

id_vars<-var_table[grepl(var_table$Field.Name, pat="id"),"R.Name"]

vars_keep<-c(id_vars, dv_vars)

y<-TE

colnames(y)[colnames(y) %in% dv_vars]<-var_table$estimName[match(colnames(y)[colnames(y) %in% dv_vars], var_table$R.Name)]


generate_probs <- function(x, type){
	
	a <- as.character(unique(x$selectMeth))
	
	if(sum(is.na(a))>0) stop ("cannot proceed: NAs in selectMeth")
	if(length(a)>1) stop ("two different selection methods")
	
	vec_n <- x$sampledUnits
	vec_N <- x$totalUnits

	if (type == "selection")
		{
		vec_prob <- x$selProbUnit # not defined
		
		print(a)
		if( a %in% c("SRSWR" , "SRSWOR") )
			{
			if( a == "SRSWR") vec_prob <-  1/vec_N
			if( a == "SRSWOR") stop ("depends on order")
			}
		if( a %in% c("UPSWR" , "UPSWOR"))
			{
			if(sum(is.na(vec_prob))>0) stop ("cannot proceed: NAs in sampProb")
			vec_prob <-  vec_prob
			}
		}

	
	if (type == "inclusion")
		{
		vec_prob <- x$incProbUnit

		if(length(a)>1) { 
				stop ("two different selection methods")
				} else {
					print(a)
					if( a %in% c("SRSWR" , "SRSWOR") )
						{
						if(sum(is.na(vec_N))>0) stop ("cannot proceed: NAs in total")
						if(sum(is.na(vec_n))>0) stop ("cannot proceed: NAs in sampled")
						if( a == "SRSWR") vec_prob <-  1-(1-1/vec_N)^vec_n
						if( a == "SRSWOR") vec_prob <-  vec_n/vec_N
						}
					if( a %in% c("UPSWR" , "UPSWOR"))
						{
						if(sum(is.na(vec_prob))>0) stop ("cannot proceed: NAs in sampProb")
						vec_prob <-  vec_prob
						}
					}

		}

			vec_prob
		
	}
	
generate_probs(x=y, type="inclusion")


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
			wi <- 1/generate_probs(x, type="inclusion")
			HTi <- wi*x$value
			HTi
		}
	
# procedure stratified	
	if( a == "Y" )
		{
		ls1<-split(x, x$stratum)
		ls2<-lapply(ls1, function(x) { 
				wi <- 1/generate_probs(x, type="inclusion")
				HTi <- wi*x$value
				HTi
				})
		 do.call("rbind",ls2)		
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
					
					
					
