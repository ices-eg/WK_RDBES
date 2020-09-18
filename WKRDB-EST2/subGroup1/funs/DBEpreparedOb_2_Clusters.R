#' Load raw object and create prepared object Function relies on the data being
#' correctly named following established hierarchy
#' @param Input a string pointing towards the input folder
#' @param Output a string pointing towards the output folder
#' @param Return True or False to determine if objects are returned to the GE default is True
#' @param CreateDir Ture or False to determine if you would like to create output directory default is False
#' @export .Rdata files
#' @examples
#'
#' Input <- "WKRDB-EST2/testData/output/DBErawObj/"
#' Output <- "WKRDB-EST2/subGroup1/personal/John/PreparedOutputs/"
#'
#' CreateDBEPrepObj(Input = Input, Output = Output, Return = T)
CreateDBEPrepObj <- function(Input = NA, Output = NA, Return = T, CreateDir = F) {
  
  if(exists("Warning")==F){
    Warning <- new.env()
    Warning$Integer <- 0
  }
  ###
  # if (is.na(Input) == T) {
  #   stop("Missing Input")
  # }
  if(Warning$Integer < 3){
    if (is.na(Input) == T) {
      stop("Missing Input")
    }
  }else{
    if(Warning$Integer >= 3){stop(paste("I am not mad, i am just disappointed you made the same error ",Warning$Integer," times",sep=""))
    }
  }
  ###
  if (is.na(Output) == T) {
    warning("No output folder, objects will not be written out")
  }
  ###
  if (dir.exists(Input) == F) {
    stop("Input folder does not exist")
  }
  ###
  if(is.na(Output) == F ){
  if ( dir.exists(Output) == F & CreateDir == F) {
    stop("Output directory does not exist")
  }
  }
  
  if(is.na(Output)==F){
  if (dir.exists(Output) == F & CreateDir == T) {
    warning("Output directory path created")
    dir.create(paste(Output), recursive = T)
  }
  }
 ###
  # get file list
  InputFiles <- list.files(Input, recursive = TRUE)
  # ignroe readme
  InputFiles <- InputFiles[!InputFiles == "README.rmd" & grepl(".rds", InputFiles) == T]




  # Loads data in
  for (i in InputFiles) {
    assign(unlist(strsplit(InputFiles[InputFiles == i], "\\."))[1], readRDS(paste(Input, InputFiles[InputFiles == i], sep = "")))
  }


  # function
  source("WKRDB-EST2/subGroup1/personal/John/generateProbs_John.r")
  source("WKRDB-EST2/subGroup1/personal/John/generateClusterProbs_John.r")
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
        if (length(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = ""))))) >= 2) {
          stop("More than one selection method")
          print(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = "")))))
        }

        ## calculates values for a and checks agianst submitted in values

        CalcValues <- generateProbs(a, "selection")
        names(CalcValues) <- "CalcValues"
        if (all(a[grepl("selProb", names(a)) == T & grepl("selProbCluster", names(a)) == F] == CalcValues) == T) {
          print("Submitted and calculated values match")
        } else {
          PrintOut <- cbind(a[grepl("id", names(a)) == T], a[grepl("selProb", names(a)) == T & grepl("selProbCluster", names(a)) == F], CalcValues, Equal = c((a[grepl("selProb", names(a)) == T & grepl("selProbCluster", names(a)) == F] == CalcValues) == T))
          print(PrintOut[PrintOut$Equal == "FALSE", ])
          rm(PrintOut)
          switch(menu(c("Yes", "No"), title = "Submitted values do not match calculated calculated do you want to overwrite submitted data?"), a[grepl("selProb", names(a)) == T & grepl("selProbCluster", names(a)) == F] <- CalcValues, print("submitted values used"))
        }
        rm(CalcValues)
      }
      ###
      ###
      if (nrow(aNA) > 0) {
        # Check on number of methods
        if (length(unique(eval(parse(text = paste0(paste("aNA"), "$", names(aNA)[grepl("selectMeth", names(aNA)) == T & grepl("selectMethCluster", names(aNA)) == F], sep = ""))))) >= 2) {
          stop("More than one selection method")
          print(unique(eval(parse(text = paste0(paste("aNA"), "$", names(aNA)[grepl("selectMeth", names(aNA)) == T & grepl("selectMethCluster", names(aNA)) == F], sep = "")))))
        }
        # applies function
        aNA[grepl("selProb", names(aNA)) == T & grepl("selProbCluster", names(aNA)) == F] <- generateProbs(aNA, "selection")
        # Rbind data
        a <- rbind(a, aNA)
        rm(aNA)
      }


      # Now for inclusion
      print("Inclusion")
      # Separate Na from non NA values
      aNA <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("incProb", names(a)) == T & grepl("incProbCluster", names(a)) == F], sep = "")))) == T, ]
      a <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("incProb", names(a)) == T & grepl("incProbCluster", names(a)) == F], sep = "")))) == F, ]
      ###
      ###

      if (nrow(a) > 0) {
        # Check on number of methods for inclusion
        if (length(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = ""))))) >= 2) {
          stop("More than one selection method")
          print(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMeth", names(a)) == T & grepl("selectMethCluster", names(a)) == F], sep = "")))))
        }

        ## calculates values for a and checks agianst submitted in values

        CalcValues <- generateProbs(a, "inclusion")
        names(CalcValues) <- "CalcValue"
        if (all(a[grepl("incProb", names(a)) == T & grepl("incProbCluster", names(a)) == F] == CalcValues) == T) {
          print("Submitted and calculated values match")
        } else {
          PrintOut <- cbind(a[grepl("id", names(a)) == T], a[grepl("incProb", names(a)) == T & grepl("incProbCluster", names(a)) == F], CalcValues, Equal = c((a[grepl("incProb", names(a)) == T & grepl("incProbCluster", names(a)) == F] == CalcValues) == T))
          print(PrintOut[PrintOut$Equal == "FALSE", ])
          rm(PrintOut)
          switch(menu(c("Yes", "No"), title = "Submitted values do not match calculated calculated do you want to overwrite submitted data?"), a[grepl("incProb", names(a)) == T & grepl("incProbCluster", names(a)) == F] <- CalcValues, print("submitted values used"))
        }
        rm(CalcValues)
      }

      ###
      ###
      if (nrow(aNA) > 0) {
        # Check on number of methods
        if (length(unique(eval(parse(text = paste0(paste("aNA"), "$", names(aNA)[grepl("selectMeth", names(aNA)) == T & grepl("selectMethCluster", names(aNA)) == F], sep = ""))))) >= 2) {
          stop("More than one inclusion method")
          print(unique(eval(parse(text = paste0(paste("aNA"), "$", names(aNA)[grepl("selectMeth", names(aNA)) == T & grepl("selectMethCluster", names(aNA)) == F], sep = "")))))
        }
        # applies function
        aNA[grepl("incProb", names(aNA)) == T & grepl("incProbCluster", names(aNA)) == F] <- generateProbs(aNA, "inclusion")
      }
      # Rbind data
      a <- rbind(a, aNA)
      ###
      ###
      
      
      ###CLUSTER SECTION 
      
      
      
      
      ### Now does Cluster seleciton

      if (any(grepl("numTotalClusters|numTotalCluster", names(a))) == T) {
        if (all(is.na(a[grepl("numTotalClusters|numTotalCluster", names(a)) == T]) == F)) {
          print("Cluster selection")
          aNA <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("selProbCluster", names(a)) == T])))) == T, ]
          a <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("selProbCluster", names(a)) == T])))) == F, ]
          ###
          ###
          if (nrow(a) > 0) {
            # Check on number of methods
            if (length(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMethCluster", names(a)) == T], sep = ""))))) >= 2) {
              stop("More than one selection method")
              print(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMethCluster", names(a)) == T], sep = "")))))
            }

            ## calculates values for a and checks agianst submitted in values

            CalcValues <- generateClusterProbs(a, "selection")
            names(CalcValues) <- "CalcValues"
            if (all(a[grepl("selProbCluster", names(a)) == T] == CalcValues$CalcValues) == T) {
              print("Submitted and calculated values match")
            } else {
              PrintOut <- cbind(a[grepl("id", names(a)) == T], a[grepl("selProbCluster", names(a)) == T], CalcValues$CalcValues, Equal = c((a[grepl("selProbCluster", names(a)) == T] == CalcValues$CalcValues) == T))
              print(PrintOut[PrintOut$Equal == "FALSE", ])
              rm(PrintOut)
              switch(menu(c("Yes", "No"), title = "Submitted values do not match calculated calculated do you want to overwrite submitted data?"), a[grepl("selProbCluster", names(a)) == T] <- CalcValues$CalcValues, print("submitted values used"))
            }
            rm(CalcValues)
          }
          ###
          ###
          if (nrow(aNA) > 0) {
            # Check on number of methods
            if (length(unique(eval(parse(text = paste0(paste("aNA"), "$", names(aNA)[grepl("selectMethCluster", names(aNA)) == T], sep = ""))))) >= 2) {
              stop("More than one selection method")
              print(unique(eval(parse(text = paste0(paste("aNA"), "$", names(aNA)[grepl("selectMethCluster", names(aNA)) == T], sep = "")))))
            }
            # applies function
            CalcValues <- generateClusterProbs(aNA, "selection")

            aNA[grepl("selProbCluster", names(aNA)) == T] <- CalcValues$CalcValues
            # Rbind data
            a <- rbind(a, aNA)
            rm(aNA, CalcValues)
          }
          ###
          ###
          # And again for inclusion
          print("Cluster inclusion")
          aNA <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("selProbCluster", names(a)) == T])))) == T, ]
          a <- a[is.na(eval(parse(text = paste0("a$", names(a)[grepl("selProbCluster", names(a)) == T])))) == F, ]
          ###
          ###
          if (nrow(a) > 0) {
            # Check on number of methods
            if (length(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMethCluster", names(a)) == T], sep = ""))))) >= 2) {
              stop("More than one selection method")
              print(unique(eval(parse(text = paste0(paste("a"), "$", names(a)[grepl("selectMethCluster", names(a)) == T], sep = "")))))
            }

            ## calculates values for a and checks agianst submitted in values

            CalcValues <- generateClusterProbs(a, "inclusion")
            names(CalcValues) <- "CalcValues"
            if (all(a[grepl("incProbCluster", names(a)) == T] == CalcValues$CalcValues) == T) {
              print("Submitted and calculated values match")
            } else {
              PrintOut <- cbind(a[grepl("id", names(a)) == T], a[grepl("incProbCluster", names(a)) == T], CalcValues$CalcValues, Equal = c((a[grepl("incProbCluster", names(a)) == T] == CalcValues$CalcValues) == T))
              print(PrintOut[PrintOut$Equal == "FALSE", ])
              rm(PrintOut)
              switch(menu(c("Yes", "No"), title = "Submitted values do not match calculated calculated do you want to overwrite submitted data?"), a[grepl("incProbCluster", names(a)) == T] <- CalcValues$CalcValues, print("submitted values used"))
            }
            rm(CalcValues)
          }
          ###
          ###
          if (nrow(aNA) > 0) {
            # Check on number of methods
            if (length(unique(eval(parse(text = paste0(paste("aNA"), "$", names(aNA)[grepl("selectMethCluster", names(aNA)) == T], sep = ""))))) >= 2) {
              stop("More than one selection method")
              print(unique(eval(parse(text = paste0(paste("aNA"), "$", names(aNA)[grepl("selectMethCluster", names(aNA)) == T], sep = "")))))
            }
            # applies function
            CalcValues <- generateClusterProbs(aNA, "inclusion")

            aNA[grepl("incProbCluster", names(aNA)) == T] <- CalcValues$CalcValues
            # Rbind data
            a <- rbind(a, aNA)
            rm(aNA, CalcValues)
          }
          ###
          ###
        } else {
          print("No values in numTotalClusters")
        }
      }


      ###
      ###
      ###
      ###

      
      
      # assigns the moddifed table back to the input object
      eval(parse(text = paste0(paste0(paste("DBEpreparedObj_", paste(unlist(strsplit(InputFiles[InputFiles == i], "\\_|\\."))[c(2, 3, 4)], sep = "", collapse = "_"), sep = "", collapse = "_"), "$", k, sep = ""), "<-", "a")))

      rm(a, aNA)
      gc()
      print(k)
    }
  }
  rm(tableNames)
  gc()

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
