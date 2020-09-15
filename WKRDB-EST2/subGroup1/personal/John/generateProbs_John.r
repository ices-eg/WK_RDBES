generateProbs <- function(x, probType){

	# generates selection and inclusion probs from RDBES N and n
	# x is RDBES data.frame
	# probType is string. Value == "selection" | "inclusion" , for selection or inclusion probabilities respectively
	
	
	a <- unique(x[grepl("selectMeth",names(x))==T])
	a <- as.character(unique(a[grepl("selectMethCluster",names(a))==F]))
	
	if(sum(is.na(a))>0) stop ("cannot proceed: NAs in SSselectMeth")
	if(length(a)>1) stop ("two different selection methods")
	
	vec_n <- x[grepl("numSamp",names(x))==T]
	vec_n <- vec_n[grepl("SampCluster",names(vec_n))==F]     
	vec_N <- x[grepl("numTotal",names(x))==T]
	vec_N <- vec_N[grepl("TotalCluster",names(vec_N))==F]
	
	if (probType == "selection")
		{
		vec_prob <- a[grepl("selProbUnit",names(a))==T] # not defined
		
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

	
	if (probType == "inclusion")
		{
		vec_prob <- a[grepl("incProbUnit",names(a))==T]
		
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
	
# generate_probs(x=y, probType="inclusion")
