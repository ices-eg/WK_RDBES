#Script to load up the RDS and fill out useing the prob functin

check <- read.csv("WKRDB-EST2/subGroup1/personal/John/DK_1965_ESP-AZTI_DCF_Onboard_Sampling_1/")


##gets files

#Input <- "WKRDB-EST2/subGroup1/personal/Marta/DBEraw/"
#Input <- "C:\\Git Repos\\WK_RDBES\\WKRDB-EST2\\subGroup1\\personal\\Marta\\DBEraw\\DK1966ESP-AZTI_DCF_Onboard_SamplingH1\\"
Output <- "WKRDB-EST2/subGroup1/personal/John/PreparedOutputs/"
Input <- "WKRDB-EST2/testData/output/DBErawObj/"

# CreateDBEPrepObj(Input = Input,Output = Output)

 CreateDBEPrepObj <- function(Input,Output){
  
#get file list
InputFiles <- list.files(Input, recursive = TRUE)
#ignroe readme
InputFiles <- InputFiles[!InputFiles == "README.rmd" &  grepl(".rds",InputFiles)==T]
  



#Loads data in
for(i in InputFiles){
  assign(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],readRDS(paste(Input,InputFiles[InputFiles==i],sep="")))
  
}


#function
source("WKRDB-EST2/subGroup1/personal/John/generateProbs_John.r")
source("WKRDB-EST2/subGroup1/personal/John/generateClusterProbs_John.r")
#DBErawObj_DK_1966_H1$VS
#runs function and assigns 
for(i in InputFiles){
    print(i)
  
  #should assign Raw to a prepaed DBE
  assign(paste("DBEpreparedObj_",paste(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[c(2,3,4)],sep="",collapse = "_"),sep = "",collapse = "_"),
         get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1]))
  
  
  table_names <- names(get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1]))
  
  #Removes FT from loop where needed
  if(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[4] %in% c("H5","H8","H12","H13")==T){
    table_names <- table_names[!table_names %in% c("SD","DE","FM","VD","SL","FT")]
  }
  #Removes LE from loop where needed
  if(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[4] %in% c("H6","H7","H9","H11")==T){
    table_names <- table_names[!table_names %in% c("SD","DE","FM","VD","SL","LE")]
  }
  #everything else
  if(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[4] %in% c("H5","H8","H12","H13","H6","H7","H9","H11")==F){
  table_names <- table_names[!table_names %in% c("SD","DE","FM","VD","SL")]
  }
  
  for(k in table_names){
  a <- eval(parse(text=paste0(paste("DBEpreparedObj_",paste(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[c(2,3,4)],sep="",collapse = "_"),sep = "",collapse = "_"),"$",k,sep="")))
  
  #Separate Na from non NA values 
  print("Selection")
  a_NA <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] ,sep=""))))==T,]
  a <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] ,sep=""))))==F,] 
  ###
  ###
  if(nrow(a)>0){  
  #Check on number of methods
  if( length( unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMeth",names(a))==T & grepl("selectMethCluster",names(a))==F],sep="")))))>=2){
    stop("More than one selection method")
    print(unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMeth",names(a))==T & grepl("selectMethCluster",names(a))==F],sep="")))))
  }
  
  ## calculates values for a and checks agianst submitted in values
  
  CalcValues <- generateProbs(a, "selection" )
  names(CalcValues) <- "CalcValues"
  if(all(a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T){
    print("Submitted and calculated values match")
  }else{
    PrintOut<- cbind(a[grepl("id",names(a))==T],a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F],CalcValues,Equal=c((a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T))
    print(PrintOut[PrintOut$Equal=="FALSE",])
    rm(PrintOut)
    switch(menu(c("Yes", "No"), title="Submitted values do not match calculated calculated do you want to overwrite submitted data?"),a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] <- CalcValues,print("submitted values used"))  
  }
  rm(CalcValues)
  }
  ###
  ###
  if(nrow(a_NA)>0){
  #Check on number of methods
   if( length( unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMeth",names(a_NA))==T & grepl("selectMethCluster",names(a_NA))==F],sep="")))))>=2){
  stop("More than one selection method")
  print(unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMeth",names(a_NA))==T & grepl("selectMethCluster",names(a_NA))==F],sep="")))))
  }
  #applies function
  a_NA[grepl("selProb",names(a_NA))==T & grepl("selProbCluster",names(a_NA))==F] <- generateProbs(a_NA, "selection" )  
  #Rbind data 
  a <- rbind(a,a_NA)
  rm(a_NA)
  }
  
  
  #Now for inclusion
  print("Inclusion")
  #Separate Na from non NA values 
  a_NA <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("incProb",names(a))==T & grepl("incProbCluster",names(a))==F] ,sep=""))))==T,] 
  a <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("incProb",names(a))==T & grepl("incProbCluster",names(a))==F] ,sep=""))))==F,] 
  ###
  ###
  
  if(nrow(a)>0){
  #Check on number of methods for inclusion
  if( length( unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMeth",names(a))==T & grepl("selectMethCluster",names(a))==F],sep="")))))>=2){
    stop("More than one selection method")
    print(unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMeth",names(a))==T & grepl("selectMethCluster",names(a))==F],sep="")))))
  }
  
  ## calculates values for a and checks agianst submitted in values
  
  CalcValues <- generateProbs(a, "inclusion" )
  names(CalcValues) <- "CalcValue"
  if(all(a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T){
    print("Submitted and calculated values match")
  }else{
    PrintOut<- cbind(a[grepl("id",names(a))==T],a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F],CalcValues,Equal=c((a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T))
    print(PrintOut[PrintOut$Equal=="FALSE",])
    rm(PrintOut)
    switch(menu(c("Yes", "No"), title="Submitted values do not match calculated calculated do you want to overwrite submitted data?"),a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] <- CalcValues,print("submitted values used"))  
  }
  rm(CalcValues)
  }
  
  ###
  ###
  if(nrow(a_NA)>0){
  #Check on number of methods
  if( length( unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMeth",names(a_NA))==T & grepl("selectMethCluster",names(a_NA))==F],sep="")))))>=2){
    stop("More than one inclusion method")
    print(unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMeth",names(a_NA))==T & grepl("selectMethCluster",names(a_NA))==F],sep="")))))
  }
  #applies function
  a_NA[grepl("incProb",names(a_NA))==T & grepl("incProbCluster",names(a_NA))==F] <- generateProbs(a_NA, "inclusion" )
  #Rbind data 
  a <- rbind(a,a_NA)
  ###
  ###
  
  if(all(is.na(a[grepl("numTotalClusters|numTotalCluster",names(a))==T])==F)){
  ###Now does Cluster seleciton
  print("Cluster selection")
  a_NA <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProbCluster",names(a))==T]))))==T,]
  a <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProbCluster",names(a))==T] ))))==F,]
  ###
  ###
  if(nrow(a)>0){  
    #Check on number of methods
    if( length( unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMethCluster",names(a))==T],sep="")))))>=2){
      stop("More than one selection method")
      print(unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMethCluster",names(a))==T],sep="")))))
    }
    
    ## calculates values for a and checks agianst submitted in values
    
    CalcValues <- generateClusterProbs(a, "selection" )
    names(CalcValues) <- "CalcValues"
    if(all(a[grepl("selProbCluster",names(a))==T]==CalcValues$CalcValues)==T){
      print("Submitted and calculated values match")
    }else{
      PrintOut<- cbind(a[grepl("id",names(a))==T],a[grepl("selProbCluster",names(a))==T],CalcValues$CalcValues,Equal=c((a[grepl("selProbCluster",names(a))==T]==CalcValues$CalcValues)==T))
      print(PrintOut[PrintOut$Equal=="FALSE",])
      rm(PrintOut)
      switch(menu(c("Yes", "No"), title="Submitted values do not match calculated calculated do you want to overwrite submitted data?"),a[grepl("selProbCluster",names(a))==T] <- CalcValues$CalcValues,print("submitted values used"))  
    }
    rm(CalcValues)
  }
  ###
  ###
  if(nrow(a_NA)>0){
    #Check on number of methods
    if( length( unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMethCluster",names(a_NA))==T ],sep="")))))>=2){
      stop("More than one selection method")
      print(unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMethCluster",names(a_NA))==T ],sep="")))))
    }
    #applies function
    CalcValues <- generateClusterProbs(a_NA, "selection" )
    
    a_NA[grepl("selProbCluster",names(a_NA))==T] <- CalcValues$CalcValues
    #Rbind data 
    a <- rbind(a,a_NA)
    rm(a_NA,CalcValues)
  }
  ###
  ###
  #And again for inclusion
  print("Cluster inclusion")
  a_NA <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProbCluster",names(a))==T]))))==T,]
  a <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProbCluster",names(a))==T] ))))==F,]
  ###
  ###
  if(nrow(a)>0){  
    #Check on number of methods
    if( length( unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMethCluster",names(a))==T],sep="")))))>=2){
      stop("More than one selection method")
      print(unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMethCluster",names(a))==T],sep="")))))
    }
    
    ## calculates values for a and checks agianst submitted in values
    
    CalcValues <- generateClusterProbs(a, "inclusion" )
    names(CalcValues) <- "CalcValues"
    if(all(a[grepl("selProbCluster",names(a))==T]==CalcValues$CalcValues)==T){
      print("Submitted and calculated values match")
    }else{
      PrintOut<- cbind(a[grepl("id",names(a))==T],a[grepl("selProbCluster",names(a))==T],CalcValues$CalcValues,Equal=c((a[grepl("selProbCluster",names(a))==T]==CalcValues$CalcValues)==T))
      print(PrintOut[PrintOut$Equal=="FALSE",])
      rm(PrintOut)
      switch(menu(c("Yes", "No"), title="Submitted values do not match calculated calculated do you want to overwrite submitted data?"),a[grepl("selProbCluster",names(a))==T] <- CalcValues$CalcValues,print("submitted values used"))  
    }
    rm(CalcValues)
  }
  ###
  ###
  if(nrow(a_NA)>0){
    #Check on number of methods
    if( length( unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMethCluster",names(a_NA))==T ],sep="")))))>=2){
      stop("More than one selection method")
      print(unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMethCluster",names(a_NA))==T ],sep="")))))
    }
    #applies function
    CalcValues <- generateClusterProbs(a_NA, "inclusion" )
    
    a_NA[grepl("selProbCluster",names(a_NA))==T] <- CalcValues$CalcValues
    #Rbind data 
    a <- rbind(a,a_NA)
    rm(a_NA,CalcValues)
  }
  ###
  ###
  }else{
    print("No values in numTotalClusters")
  }
  
  ###
  ###
  ### 
  ###
  
  #assigns the moddifed table back to the input object
  eval(parse(text=paste0(paste0(paste("DBEpreparedObj_",paste(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[c(2,3,4)],sep="",collapse = "_"),sep = "",collapse = "_"),"$",k,sep=""), "<-", "a")))
  
  rm(a,a_NA)
  gc()
  print(k)
  }
  
}
  rm(table_names)
  gc()
}
# Asslist <- get(ls()[grepl("DBEpreparedObj_",x=ls())==T])
Asslist <-list()
for(i in ls()[grepl("DBEpreparedObj_",x=ls())==T]){
  print(i)
  Asslist[length(Asslist)+1] <- list(get(ls()[ls()==i]))
}
names(Asslist) <- ls()[grepl("DBEpreparedObj_",x=ls())==T]
### get asslist back to ge
list2env(Asslist,envir = .GlobalEnv)

#wite out data

for(i in InputFiles){
  saveRDS(get(paste("DBEpreparedObj_",paste(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[c(2,3,4)],sep="",collapse = "_"),sep = "",collapse = "_")),file=paste(Output,paste("DBEpreparedObj_",paste(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[c(2,3,4)],sep="",collapse = "_"),sep = "",collapse = "_"),".Rds",sep=""))
}

}

# Bellow this line is the scrapbook for code snippets ---------------------


# paste("DBEprepared",paste(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[c(2,3,4)],sep="",collapse = "_"),sep = "",collapse = "_")

# 
# names(get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1]))
# 
# get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1])
# 
# paste0("$",table_names[1],sep="")

# eval(parse(text=paste0(paste0(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],"$",table_names[1],sep=""))))
# a[grepl("numSamp",names(a))==T]
# 
# for(k in table_names){
#   
#   print(k)
# 
# }
# 
# generateProbs(a,"selection")
# 
# c <- unique(a[grepl("selectMeth",names(a))==T])
# unique(c[grepl("selectMethCluster",names(c))==F])
# 
# 
# a[grepl("selProb",names(a))==T ]
# 
# a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F ]
# a[grepl("incProb",names(a))==T & grepl("incProbCluster",names(a))==F]
# selProbCluster
# 
# 
# 
# if(is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] ,sep=""))))==T){
#   
#   
# }
# 
# 
# a_NA[grepl("selProb",names(a_NA))==T & grepl("selProbCluster",names(a_NA))==F] <- generateProbs(a_NA, "selection" ) 
# 
# CalcValues <- generateProbs(a, "selection" )
# if(all(a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T){
#   print("Submitted and calculated values match")
# }else{
#   
#   PrintOut<- cbind(a[grepl("id",names(a))==T],a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F],CalcValues,Equal=c((a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T))
#   print(PrintOut[PrintOut$Equal=="FALSE",])
#   rm(PrintOut)
#   switch(menu(c("Yes", "No"), title="Submitted values do not match calculated calculated do you want to overwrite submitted data?"),a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] <- CalcValues,print("submitted values used"))  
# }
# 
# 
# 
# menu(c("Yes", "No"), title="Do you want this?")
# 
# a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues
# 
# all(a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)
