#'Load raw object and create prepared object Function relies on the data being
#'correctly named following established hierarchy
#'@param Input a string pointing towards the input folder
#'@param Output a string pointing towards the output folder
#'@export .Rdata files
#'@examples
#'
#'Input <- "WKRDB-EST2/testData/output/DBErawObj/"
#'Output <- "WKRDB-EST2/subGroup1/personal/John/PreparedOutputs/"
#'
#'CreateDBEPrepObj(Input = Input,Output = Output)

CreateDBEPrepObj <- function(Input,Output){
  
  #get file list
  InputFiles <- list.files(Input, recursive = TRUE)
  #ignroe readme
  InputFiles <- InputFiles[!InputFiles == "README.rmd"]
  
  
  
  
  #Loads data in
  for(i in InputFiles){
    assign(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],readRDS(paste(Input,InputFiles[InputFiles==i],sep="")))
    
  }
  
  
  #function
  source("WKRDB-EST2/subGroup1/personal/John/generateProbs_John.r")
  
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
      #assigns k df from Input list obj to a 
      a <- eval(parse(text=paste0(paste("DBEpreparedObj_",paste(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[c(2,3,4)],sep="",collapse = "_"),sep = "",collapse = "_"),"$",k,sep="")))
      
      #Separate Na from non NA values 
      print("Selection")
      a_NA <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] ,sep=""))))==T,]
      a <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] ,sep=""))))==F,] 
      
      #Check on number of methods
      if( length( unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMeth",names(a))==T & grepl("selectMethCluster",names(a))==F],sep="")))))>=2){
        stop("More than one selection method")
        print(unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMeth",names(a))==T & grepl("selectMethCluster",names(a))==F],sep="")))))
      }
      
      ## calculates values for a and checks agianst submitted in values
      CalcValues <- generateProbs(a, "selection" )
      if(all(a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T){
        print("Submitted and calculated values match")
      }else{
        PrintOut<- cbind(a[grepl("id",names(a))==T],a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F],CalcValues,Equal=c((a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T))
        print(PrintOut[PrintOut$Equal=="FALSE",])
        rm(PrintOut)
        switch(menu(c("Yes", "No"), title="Submitted values do not match calculated calculated do you want to overwrite submitted data?"),a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] <- CalcValues,print("submitted values used"))  
      }
      rm(CalcValues)
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
      #Now for inclusion
      print("Inclusion")
      #Separate Na from non NA values 
      a_NA <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("incProb",names(a))==T & grepl("incProbCluster",names(a))==F] ,sep=""))))==T,] 
      a <- a[is.na(eval(parse(text = paste0("a$",names(a)[grepl("incProb",names(a))==T & grepl("incProbCluster",names(a))==F] ,sep=""))))==F,] 
      
      
      #Check on number of methods for inclusion
      if( length( unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMeth",names(a))==T & grepl("selectMethCluster",names(a))==F],sep="")))))>=2){
        stop("More than one selection method")
        print(unique(eval(parse(text=paste0(paste("a"),"$",names(a)[grepl("selectMeth",names(a))==T & grepl("selectMethCluster",names(a))==F],sep="")))))
      }
      
      ## calculates values for a and checks agianst submitted in values
      CalcValues <- generateProbs(a, "inclusion" )
      if(all(a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T){
        print("Submitted and calculated values match")
      }else{
        PrintOut<- cbind(a[grepl("id",names(a))==T],a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F],CalcValues,Equal=c((a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F]==CalcValues)==T))
        print(PrintOut[PrintOut$Equal=="FALSE",])
        rm(PrintOut)
        switch(menu(c("Yes", "No"), title="Submitted values do not match calculated calculated do you want to overwrite submitted data?"),a[grepl("selProb",names(a))==T & grepl("selProbCluster",names(a))==F] <- CalcValues,print("submitted values used"))  
      }
      rm(CalcValues)
      
      #Check on number of methods
      if( length( unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMeth",names(a_NA))==T & grepl("selectMethCluster",names(a_NA))==F],sep="")))))>=2){
        stop("More than one inclusion method")
        print(unique(eval(parse(text=paste0(paste("a_NA"),"$",names(a_NA)[grepl("selectMeth",names(a_NA))==T & grepl("selectMethCluster",names(a_NA))==F],sep="")))))
      }
      #applies function
      a_NA[grepl("incProb",names(a_NA))==T & grepl("incProbCluster",names(a_NA))==F] <- generateProbs(a_NA, "inclusion" )
      #Rbind data 
      a <- rbind(a,a_NA)
      #assigns the moddifed table back to the input object
      eval(parse(text=paste0(paste0(paste("DBEpreparedObj_",paste(unlist(strsplit(InputFiles[InputFiles==i],"\\_|\\."))[c(2,3,4)],sep="",collapse = "_"),sep = "",collapse = "_"),"$",k,sep=""), "<-", "a")))
      
      rm(a,a_NA)
      gc()
      print(k)
    }
    rm(table_names)
    gc()
  }
  # Asslist for sending back to GE
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

