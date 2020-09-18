#' Load raw object and create prepared object
#' Function relies on the data being
#' correctly named following established hierarchy
#'
#' @param input a string pointing towards the input folder
#' @param output a string pointing towards the output folder
#' @export .Rdata files
#' @examples
#'
#' input <- "WKRDB-EST2/testData/output/DBErawObj/"
#' output <- "WKRDB-EST2/subGroup1/personal/John/PreparedOutputs/"
#'
#' CreateDBEPrepObj(input = input, output = output)
createDBEPrepObj <- function(input, output) {

  # get file list
  inputFiles <- list.files(input, recursive = TRUE)
  # ignroe readme
  inputFiles <- inputFiles[!inputFiles == "README.rmd" &
                             grepl(".rds", inputFiles) == T]




  # Loads data in
  for (i in inputFiles) {
    assign(unlist(strsplit(inputFiles[inputFiles == i], "\\."))[1]
           , readRDS(paste(input, inputFiles[inputFiles == i], sep = "")))
  }


  # function
  source("WKRDB-EST2/subGroup1/personal/John/generateProbs_John.r")

  # DBErawObj_DK_1966_H1$VS
  # runs function and assigns
  for (i in inputFiles) {
    print(i)

    # should assign Raw to a prepaed DBE
    assign(
      paste("DBEpreparedObj_", paste(
        unlist(
          strsplit(
            inputFiles[inputFiles == i], "\\_|\\."))[c(2, 3, 4)],
              sep = "", collapse = "_"), sep = "", collapse = "_"),
                get(unlist(strsplit(inputFiles[inputFiles == i], "\\."))[1])
    )


    tableNames <- names(
      get(unlist(strsplit(inputFiles[inputFiles == i], "\\."))[1])
      )

    # Removes FT from loop where needed
    if (unlist(strsplit(inputFiles[inputFiles == i], "\\_|\\."))[4] %in%
        c("H5", "H8", "H12", "H13") == T) {
      tableNames <- tableNames[!tableNames %in%
                                   c("SD", "DE", "FM", "VD", "SL", "FT")]
    }
    # Removes LE from loop where needed
    if (unlist(strsplit(inputFiles[inputFiles == i], "\\_|\\."))[4] %in%
        c("H6", "H7", "H9", "H11") == T) {
      tableNames <- tableNames[!tableNames %in%
                                   c("SD", "DE", "FM", "VD", "SL", "LE")]
    }
    # everything else
    if (unlist(strsplit(inputFiles[inputFiles == i], "\\_|\\."))[4] %in%
        c("H5", "H8", "H12", "H13", "H6", "H7", "H9", "H11") == F) {
      tableNames <- tableNames[!tableNames %in%
                                   c("SD", "DE", "FM", "VD", "SL")]
    }

    for (k in tableNames) {
      a <-
        eval(parse(text =
            paste0(
              paste("DBEpreparedObj_",
                paste(
                  unlist(
                    strsplit(
                      inputFiles[inputFiles == i], "\\_|\\."
                    )
                  )[c(2, 3, 4)], sep = "", collapse = "_"
                ), sep = "", collapse = "_"
              ), "$", k, sep = ""
            )
          )
        )

      # Separate Na from non NA values
      print("Selection")
      aNa <- a[is.na(
                eval(
                  parse(
                    text = paste0(
                      "a$", names(a)[
                        grepl("selProb", names(a)) == T &
                          grepl("selProbCluster", names(a)) == F], sep = ""
                      )
                    )
                  )
                ) == T, ]

      a <- a[is.na(
              eval(
                parse(
                  text = paste0(
                    "a$", names(a)[
                      grepl("selProb", names(a)) == T &
                        grepl("selProbCluster", names(a)) == F], sep = ""
                    )
                  )
                )
              ) == F, ]
      ###
      ###
      if (nrow(a) > 0) {
        # Check on number of methods
        if (length(
          unique(
            eval(
              parse(
                text = paste0(
                  paste("a"), "$",
                    names(a)[grepl("selectMeth", names(a)) == T &
                          grepl("selectMethCluster", names(a)) == F], sep = ""
                  )
                )
              )
            )
          ) >= 2) {
          stop("More than one selection method")
          print(
            unique(
              eval(
                parse(
                  text = paste0(
                    paste("a"), "$",
                      names(a)[grepl("selectMeth", names(a)) == T &
                          grepl("selectMethCluster", names(a)) == F], sep = ""
                    )
                  )
                )
              )
            )
        }

        ## calculates values for a and checks agianst submitted in values

        calcValues <- generateProbs(a, "selection")
        names(calcValues) <- "calcValues"
        if (all(
          a[grepl("selProb", names(a)) == T &
            grepl("selProbCluster", names(a)) == F] == calcValues) == T) {
          print("Submitted and calculated values match")
        } else {
          printOut <- cbind(
            a[grepl("id", names(a)) == T],
              a[grepl("selProb", names(a)) == T &
                  grepl("selProbCluster", names(a)) == F],
                    calcValues,
                      Equal = c((a[grepl("selProb", names(a)) == T &
                               grepl("selProbCluster", names(a)) == F]
                        == calcValues) == T)
            )
          print(printOut[printOut$Equal == "FALSE", ])
          rm(printOut)
          switch(menu(c("Yes", "No")
                      , title = "Submitted values do not match calculated
                      calculated do you want to overwrite submitted data?"),
                 a[grepl("selProb", names(a)) == T &
                     grepl("selProbCluster", names(a)) == F] <-
                   calcValues, print("submitted values used"))
        }
        rm(calcValues)
      }
      ###
      ###
      if (nrow(aNa) > 0) {
        # Check on number of methods
        if (length(
          unique(
            eval(
              parse(
                text = paste0(
                  paste("aNa"), "$",
                    names(aNa)[grepl("selectMeth", names(aNa)) == T &
                      grepl("selectMethCluster", names(aNa)) == F], sep = ""
                  )
                )
              )
            )
          ) >= 2) {
          stop("More than one selection method")
          print(
            unique(
              eval(
                parse(
                  text = paste0(paste("aNa"), "$",
                    names(aNa)[grepl("selectMeth", names(aNa)) == T &
                      grepl("selectMethCluster", names(aNa)) == F], sep = ""
                    )
                  )
                )
              )
            )
        }
        # applies function
        aNa[grepl(
          "selProb", names(aNa)) == T &
            grepl("selProbCluster", names(aNa)) == F] <-
              generateProbs(aNa, "selection")
        # Rbind data
        a <- rbind(a, aNa)
        rm(aNa)
      }


      # Now for inclusion
      print("Inclusion")
      # Separate Na from non NA values
      aNa <-
        a[is.na(
          eval(
            parse(
              text = paste0(
                "a$", names(a)[grepl("incProb", names(a)) == T &
                  grepl("incProbCluster", names(a)) == F], sep = ""
                )
              )
            )
          ) == T, ]
      a <-
        a[is.na(
          eval(
            parse(
              text = paste0(
                "a$", names(a)[grepl("incProb", names(a)) == T &
                  grepl("incProbCluster", names(a)) == F], sep = ""
                )
              )
            )
          ) == F, ]
      ###
      ###

      if (nrow(a) > 0) {
        # Check on number of methods for inclusion
        if (length(
          unique(
            eval(
              parse(
                text = paste0(
                  paste("a"), "$",
                    names(a)[grepl("selectMeth", names(a)) == T &
                      grepl("selectMethCluster", names(a)) == F], sep = ""
                  )
                )
              )
            )
          ) >= 2) {
          stop("More than one selection method")
          print(unique(
            eval(
              parse(
                text = paste0(
                  paste("a"), "$",
                    names(a)[grepl("selectMeth", names(a)) == T &
                      grepl("selectMethCluster", names(a)) == F], sep = ""
                  )
                )
              )
            )
          )
        }

        ## calculates values for a and checks agianst submitted in values

        calcValues <- generateProbs(a, "inclusion")
        names(calcValues) <- "CalcValue"
        if (all(
          a[grepl("selProb", names(a)) == T &
            grepl("selProbCluster", names(a)) == F] == calcValues
          ) == T) {
          print("Submitted and calculated values match")
        } else {
          printOut <-
            cbind(
              a[grepl("id", names(a)) == T],
              a[grepl("selProb", names(a)) == T &
                  grepl("selProbCluster", names(a)) == F],
              calcValues,
              Equal = c(
                (
                  a[grepl("selProb", names(a)) == T &
                      grepl("selProbCluster", names(a)) == F] == calcValues
                ) == T)
              )
          print(printOut[printOut$Equal == "FALSE", ])
          rm(printOut)
          switch(menu(c("Yes", "No"),
                  title = "Submitted values do not match calculated calculated
                  do you want to overwrite submitted data?"),
                    a[grepl("selProb", names(a)) == T &
                        grepl("selProbCluster", names(a)) == F] <-
                   calcValues, print("submitted values used")
                 )
        }
        rm(calcValues)
      }

      ###
      ###
      if (nrow(aNa) > 0) {
        # Check on number of methods
        if (length(
          unique(
            eval(
              parse(
                text = paste0(
                  paste("aNa"), "$",
                    names(aNa)[grepl("selectMeth", names(aNa)) == T &
                      grepl("selectMethCluster", names(aNa)) == F], sep = ""
                  )
                )
              )
            )
          ) >= 2) {
          stop("More than one inclusion method")
          print(unique(
            eval(
              parse(
                text = paste0(
                  paste("aNa"), "$",
                    names(aNa)[grepl("selectMeth", names(aNa)) == T &
                      grepl("selectMethCluster", names(aNa)) == F], sep = ""
                  )
                )
              )
            )
          )
        }
        # applies function
        aNa[grepl("incProb", names(aNa)) == T &
               grepl("incProbCluster", names(aNa)) == F] <-
                  generateProbs(aNa, "inclusion")
        # Rbind data
        a <- rbind(a, aNa)
        ###
        ###

        # assigns the moddifed table back to the input object
        eval(parse(
          text = paste0(
            paste0(
              paste("DBEpreparedObj_",
                    paste(
                      unlist(
                        strsplit(
                          inputFiles[inputFiles == i], "\\_|\\."))[c(2, 3, 4)],
                            sep = "", collapse = "_"
                      ), sep = "", collapse = "_"
                    ), "$", k, sep = ""
              ), "<-", "a")
            )
          )

        rm(a, aNa)
        gc()
        print(k)
      }
    }
    rm(tableNames)
    gc()
  }

  assList <- list()
  for (i in ls()[grepl("DBEpreparedObj_", x = ls()) == T]) {
    print(i)
    assList[length(assList) + 1] <- list(get(ls()[ls() == i]))
  }
  names(assList) <- ls()[grepl("DBEpreparedObj_", x = ls()) == T]
  ### get asslist back to ge
  list2env(assList, envir = .GlobalEnv)

  # wite out data

  for (i in inputFiles) {
    saveRDS(
      get(
        paste(
          "DBEpreparedObj_", paste(
            unlist(
              strsplit(
                inputFiles[inputFiles == i], "\\_|\\."))[c(2, 3, 4)],
                  sep = "", collapse = "_"
              ), sep = "", collapse = "_"
            )
          ),
      file = paste(
        output, paste(
          "DBEpreparedObj_", paste(
            unlist(
              strsplit(
                inputFiles[inputFiles == i], "\\_|\\."))[c(2, 3, 4)],
                  sep = "", collapse = "_"
              ), sep = "", collapse = "_"
            ), ".Rds", sep = ""
          )
        )
  }
}
