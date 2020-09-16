generateProbs <- function(x, probType){
	
	# generates selection and inclusion probs from RDBES N and n
	# x is RDBES data.frame
	# probType is string. Value == "selection" | "inclusion" , for selection or inclusion probabilities respectively
	
	
	a <- as.character(unique(x$selectMeth))
	
	if(sum(is.na(a))>0) stop ("cannot proceed: NAs in selectMeth")
	if(length(a)>1) stop ("two different selection methods")
	
	vec_n <- x$numSamp
	vec_N <- x$numTotal

	if (probType == "selection")
		{
		vec_prob <- x$selProb # not defined
		
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
		vec_prob <- x$incProb

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
