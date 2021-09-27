#' Load raw object and create prepared object Function relies on the data being
#' correctly named following established hierarchy
#' @param Input a string pointing towards the input folder
#' @param Output a string pointing towards the output folder
#' @param Return True or False to determine if objects are returned to the GE deafult is True
#' @param CreateDir Ture or False to determien if you would like to create output dir
#' @export .Rdata files
#' @examples
#'
# Input <- "D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/subGroup1/inputs/DBEraw/"
# Output <- "D:/Projekty/RDBES/WK_RDBES/WKRDB-EST2/subGroup1/outputs/"
# Return <- T
# CreateDBEPrepObj(Input = Input, Output = Output)
CreateDBEPrepObj <- function(Input = NA, Output = NA, Return = T, CreateDir = F) {
  
  options(error=NULL)
  if (is.na(Input) == T) {
    stop("Missing Input")
  }
  
  
  if (is.na(Output) == T) {
    warning("No output folder, objects will not be written out")
  }
  
  if (CreateDir == T & is.na(Output) == T) {
    stop("No Output path given and cannot create path")
  }
  
  if (dir.exists(Input) == F) {
    stop("Input folder does not exist")
  }

  if(is.na(Output) == F ){
  if (dir.exists(Output) == F & CreateDir == F) {
    stop("Output directory does not exist")
  }
}
  
  if(is.na(Output)==F){
    if (dir.exists(Output) == F & CreateDir == T) {
      warning("Output directory path created")
      dir.create(paste(Output), recursive = T)
    }
  }
  
  
  
  if (is.na(Input) == F) {
  # get file list
  InputFiles <- list.files(Input)
  # ignroe readme
  InputFiles <- InputFiles[!InputFiles == "README.rmd" & grepl(".rds", InputFiles) == T]




  # Loads data in
  for (i in InputFiles) {
    assign(unlist(strsplit(InputFiles[InputFiles == i], "\\."))[1], readRDS(paste(Input, InputFiles[InputFiles == i], sep = "")))
  }


  # function
  source("WKRDB-EST2/subGroup1/personal/John/generateProbs_John.r")

  # DBErawObj_DK_1966_H1$VS
  # runs function and assigns
  for (i in InputFiles) {
    print(i)

    # should assign Raw to a prepaed DBE
    assign(
      paste("DBEpreparedObj_", paste(unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[c(2, 3, 4)], sep = "", collapse = "_"), sep = "", collapse = "_"),
      get(unlist(strsplit(InputFiles[InputFiles == i], "\\."))[1])
    )


    tableNames <- names(get(unlist(strsplit(InputFiles[InputFiles == i], "\\."))[1]))

    # Removes FT from loop where needed
    if (unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[4] %in% c("H5", "H8", "H12", "H13") == T) {
      tableNames <- tableNames[!tableNames %in% c("SD", "DE", "FM", "VD", "SL", "FT")]
    }
    # Removes LE from loop where needed
    if (unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[4] %in% c("H6", "H7", "H9", "H11") == T) {
      tableNames <- tableNames[!tableNames %in% c("SD", "DE", "FM", "VD", "SL", "LE")]
    }
    # everything else
    if (unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[4] %in% c("H5", "H8", "H12", "H13", "H6", "H7", "H9", "H11") == F) {
      tableNames <- tableNames[!tableNames %in% c("SD", "DE", "FM", "VD", "SL")]
    }

    for (k in tableNames) {
      a <- eval(parse(text = paste0(paste("DBEpreparedObj_", paste(unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[c(2, 3, 4)], sep = "", collapse = "_"), sep = "", collapse = "_"), "$", k, sep = "")))

      # Separate Na from non NA values
      print("Selection")
      aNA <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("selProb", names(a)) == T & grepl("selProbCluster", names(a)) == F], sep = "")))) == T, ]
      a <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("selProb", names(a)) == T & grepl("selProbCluster", names(a)) == F], sep = "")))) == F, ]
      ###
      ###
      if (nrow(a) > 0) {
        
        # Check on number of methods
        # if (length(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = ""))))) >= 2) {
        #   stop("More than one selection method")
        #   print(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = "")))))
        # }
        listSelectMeth = split(a, a[grepl("selectMeth", names(a)) == T& grepl("selectMethCluster", names(a)) == F])
        for(j in listSelectMeth){
          
          if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) == 'CENSUS'){
            
            CalcValues = data.frame(CalcValue = rep(1, nrow(j)))
            j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F] <- CalcValues
           
            rm(CalcValues)
            
          }else if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) %in% c('SRSWR', 'UPSWR')){
            
            CalcValues <- generateProbs(j, "selection")
            names(CalcValues) <- "CalcValues"
            ## calculates values for a and checks agianst submitted in values
            
            if (all(j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F] == CalcValues) == T) {
              print("Submitted and calculated values match")
            } else {
              PrintOut <- cbind(j[grepl("id", names(j)) == T], j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F], CalcValues, Equal = c((j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F] == CalcValues) == T))
              print(PrintOut[PrintOut$Equal == "FALSE", ])
              rm(PrintOut)
              switch(menu(c("Yes", "No"), title = "Submitted values do not match calculated calculated do you want to overwrite submitted data?"), j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F] <- CalcValues, print("submitted values used"))
            }
            rm(CalcValues)
            
          }else if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) %in% c('NotSam','NPEJ', 'NPAH')){
            print('Probabilites not calculated because the selection method is not probabilistic')
          }else{
            
            # CalcValues = data.frame(CalcValue = rep(0, nrow(j)))
            # j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F] <- CalcValues
            # 
            # rm(CalcValues)
            print('This selection method is not ready yet. Original values left')
          }
          
          
          

         
          if(names(listSelectMeth)[1]==unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F])) {
            aProb = j
          }else{
            aProb = rbind(aProb, j)
          }
        }

      }else{
        aProb = data.frame()
      }
      ###
      ###
      if (nrow(aNA) > 0) {
        
        # Check on number of methods
        # if (length(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = ""))))) >= 2) {
        #   stop("More than one selection method")
        #   print(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = "")))))
        # }
        listSelectMeth = split(aNA, aNA[grepl("selectMeth", names(aNA)) == T& grepl("selectMethCluster", names(aNA)) == F])
        for(j in listSelectMeth){
          
          if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) == 'CENSUS'){
            
            CalcValues = data.frame(CalcValue = rep(1, nrow(j)))
            j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F] <- CalcValues
            
            rm(CalcValues)
            
          }else if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) %in% c('SRSWR', 'UPSWR')){
            
            j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F] <- generateProbs(j, "selection")
            
            
          }else if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) %in% c('NotSam','NPEJ', 'NPAH')){
            print('Probabilites not calculated because the selection method is not probabilistic') #SYSS, SRSWOR, UPSWOR
          }else{
            
            # CalcValues = data.frame(CalcValue = rep(0, nrow(j)))
            # j[grepl("selProb", names(j)) == T & grepl("selProbCluster", names(j)) == F] <- CalcValues
            # 
            # rm(CalcValues)
            print('This selection method is not ready yet. Original values left')
          }
          
          
          
          if(names(listSelectMeth)[1]==unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F])) {
            aNAProb = j
          }else{
            aNAProb = rbind(aNAProb, j)
          }
        }
        
        
      }else{
        aNAProb = data.frame()
      }
      
      if(nrow(aProb)>0 & nrow(aNAProb)>0){
      a=rbind(aProb, aNAProb)
      }else if(nrow(aProb)>0){
        a = aProb
      }else{
        a = aNAProb
      }

      print("Inclusion")
      aNA <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("incProb", names(a)) == T & grepl("incProbCluster", names(a)) == F], sep = "")))) == T, ]
      a <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("incProb", names(a)) == T & grepl("incProbCluster", names(a)) == F], sep = "")))) == F, ]
      ###
      ###
      if (nrow(a) > 0) {
        
        listSelectMeth = split(a, a[grepl("selectMeth", names(a)) == T& grepl("selectMethCluster", names(a)) == F])
        for(j in listSelectMeth){
          
          if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) == 'CENSUS'){
            
            CalcValues = data.frame(CalcValue = rep(1, nrow(j)))
            j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F] <- CalcValues
            
            rm(CalcValues)
            
          }else if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) %in% c('SRSWR', 'UPSWR')){
            
            CalcValues <- generateProbs(j, "inclusion")
            names(CalcValues) <- "CalcValues"
            ## calculates values for a and checks agianst submitted in values
            
            if (all(j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F] == CalcValues) == T) {
              print("Submitted and calculated values match")
            } else {
              PrintOut <- cbind(j[grepl("id", names(j)) == T], j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F], CalcValues, Equal = c((j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F] == CalcValues) == T))
              print(PrintOut[PrintOut$Equal == "FALSE", ])
              rm(PrintOut)
              switch(menu(c("Yes", "No"), title = "Submitted values do not match calculated calculated do you want to overwrite submitted data?"), j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F] <- CalcValues, print("submitted values used"))
            }
            rm(CalcValues)
            
          }else if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) %in% c('NotSam','NPEJ', 'NPAH')){
            print('Probabilites not calculated because the selection method is not probabilistic')
          }else{
            
            # CalcValues = data.frame(CalcValue = rep(0, nrow(j)))
            # j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F] <- CalcValues
            # 
            # rm(CalcValues)
            print('This selection method is not ready yet. Original values left')
          }
          
          
          
          
          
          if(names(listSelectMeth)[1]==unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F])) {
            aProb = j
          }else{
            aProb = rbind(aProb, j)
          }
        }
        
      }else{
        aProb = data.frame()
      }
      ###
      ###
      if (nrow(aNA) > 0) {
        
        # Check on number of methods
        # if (length(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = ""))))) >= 2) {
        #   stop("More than one selection method")
        #   print(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = "")))))
        # }
        listSelectMeth = split(aNA, aNA[grepl("selectMeth", names(aNA)) == T& grepl("selectMethCluster", names(aNA)) == F])
        for(j in listSelectMeth){
          
          if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) == 'CENSUS'){
            
            CalcValues = data.frame(CalcValue = rep(1, nrow(j)))
            j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F] <- CalcValues
            
            rm(CalcValues)
            
          }else if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) %in% c('SRSWR', 'UPSWR')){
            
            j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F] <- generateProbs(j, "inclusion")
            
            
          }else if(unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F]) %in% c('NotSam','NPEJ', 'NPAH')){
            print('Probabilites not calculated because the selection method is not probabilistic') #SYSS, SRSWOR, UPSWOR
          }else{
            
            # CalcValues = data.frame(CalcValue = rep(0, nrow(j)))
            # j[grepl("incProb", names(j)) == T & grepl("incProbCluster", names(j)) == F] <- CalcValues
            # 
            # rm(CalcValues)
            print('This selection method is not ready yet. Original values left')
          }
          
          
          
          if(names(listSelectMeth)[1]==unique(j[grepl("selectMeth", names(j)) == T & grepl("selectMethCluster", names(j)) == F])) {
            aNAProb = j
          }else{
            aNAProb = rbind(aNAProb, j)
          }
        }
        
        
      }else{
        aNAProb = data.frame()
      }
      
      if(nrow(aProb)>0 & nrow(aNAProb)>0){
        a=rbind(aProb, aNAProb)
      }else if(nrow(aProb)>0){
        a = aProb
      }else{
        a = aNAProb
      }
      eval(parse(text = paste0(paste0(paste("DBEpreparedObj_", paste(unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[c(2, 3, 4)], sep = "", collapse = "_"), sep = "", collapse = "_"), "$", k, sep = ""), "<-", "a")))
      
    }
    rm(tableNames)
    gc()
  }
  
  # Asslist <- get(ls()[grepl("DBEpreparedObj_",x=ls())==T])
  Asslist <- list()
  for (i in ls()[grepl("DBEpreparedObj_", x = ls()) == T]) {
    print(i)
    Asslist[length(Asslist) + 1] <- list(get(ls()[ls() == i]))
  }
  names(Asslist) <- ls()[grepl("DBEpreparedObj_", x = ls()) == T]
  ### get asslist back to ge
  if (Return == T) {
    list2env(Asslist, envir = .GlobalEnv)
  }
  # wite out data
  if (is.na(Output) == F) {
    for (i in InputFiles) {
      saveRDS(get(paste("DBEpreparedObj_", paste(unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[c(2, 3, 4)], sep = "", collapse = "_"), sep = "", collapse = "_")), file = paste(Output, paste("DBEpreparedObj_", paste(unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[c(2, 3, 4)], sep = "", collapse = "_"), sep = "", collapse = "_"), ".Rds", sep = ""))
    }
  }
  }
  }
