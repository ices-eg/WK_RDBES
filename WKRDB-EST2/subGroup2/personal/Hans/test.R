
load('./WKRDB-EST2/subGroup2/personal/Hans/private/H1_upper.RData')

H1_upper$VS$VSprob <- 1-(1-1/H1_upper$VS$VStotal)^H1_upper$VS$VSsampled
H1_upper$FT$FTprob <- 1-(1-1/H1_upper$FT$FTtotal)^H1_upper$FT$FTsampled
H1_upper$FO$FOprob <- 1-(1-1/H1_upper$FO$FOtotal)^H1_upper$FO$FOsampled




# copy of KBH's function

#' generic_su_object_upper_hie
#'
#' @param input_list All the data tables in a named list. Name should be equal 
#' to the short table names e.g. DE, SD, TE, FO. An example can be found at the share point: 
#' https://community.ices.dk/ExpertGroups/WKRDB/2019%20Meetings/WKRDB-EST%202019/06.%20Data/Kirsten/H1/H1_upper.RData
#' @param hierachy The number of the hierachy you are inputting - 1 to 13
#' 
#'
#' @return
#' @export
#'
#' @examples
#' 


# hack
#generic_su_object_upper_hie <-
#  function(input_list = H1_upper,
#           hierachy = 1) {

input_list = H1_upper
hierachy = 1
   
    
    library(dplyr)
    
    
    # Varibale names for the output
    var_names <- c(
      "hierachy",
      "su",
      "recType",
      "idAbove",
      "id",
      "stratification",
      "stratum",
      "clustering",
      "clusterName",
      "total",
      "sampled",
      "prob",
      "selectMeth",
      "selectMethCluster",
      "totalClusters",
      "sampledClusters",
      "probCluster"
    )
    
    # createing a list with expected tables for each hierachy
    expected_tables <- list(
      H1 = data.frame(
        table_names = c("DE", "SD", "VS",  "FT",  "FO"),
        su_level =    c("NA", "NA", "su1", "su2", "su3")
## also need info on stratification, where does that go in the output?
## what about mandatory tables not in hierarchy?
## why do we include SL and SA here and not for the other hierarchies?
## NA or "NA"?
      ),
      H2 = data.frame(
#        table_names = c("DE", "SD", "FT", "FO"),
#        su_level = c("NA", "NA", "su1", "su2")
        table_names = c("DE", "SD", "FT",  "FO",  "SS",  "SA"),
        su_level =    c("NA", "NA", "su1", "su2", "su3", "su4")
# should we include SS and SA?
      ),
      H3 = data.frame(
        table_names = c("DE", "SD", "TE", "VS", "FT", "FO"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
      ),

      H4 = data.frame(
        table_names = c("DE", "SD", "OS", "FT", "LE"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H5 = data.frame(
        table_names = c("DE", "SD", "OS", "LE"),
        su_level = c("NA", "NA", "su1", "su2")
      ),
      H6 = data.frame(
#        table_names = c("DE", "SD", "OS", "FT"),
#        su_level = c("NA", "NA", "su1", "su2")
      table_names = c("DE", "SD", "OS",  "FT",  "FO"),
      su_level =    c("NA", "NA", "su1", "su2", "NA")
      ),
## FO is a new mandatory table with restriction (FOaggregationLevel must be set to “T”)
## i suppose it is not a sampling unit
      H7 = data.frame(
        table_names = c("DE", "SD", "OS"),
        su_level = c("NA", "NA", "su1")
      ),
      H8 = data.frame(
        table_names = c("DE", "SD", "TE", "VS", "LE"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H9 = data.frame(
        table_names = c("DE", "SD", "LO", "TE"),
        su_level = c("NA", "NA", "su1", "su2")
      ),
      H10 = data.frame(
        table_names = c("DE", "SD", "VS", "TE", "FT", "FO"),
        su_level = c("NA", "NA", "su1", "su2", "su3", "su4")
      ),
      H11 = data.frame(
        table_names = c("DE", "SD", "LO", "TE", "FT"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H12 = data.frame(
        table_names = c("DE", "SD", "LO", "TE", "LE"),
        su_level = c("NA", "NA", "su1", "su2", "su3")
      ),
      H13 = data.frame(
        table_names = c("DE", "SD", "FO"),
        su_level = c("NA", "NA", "su1")
      )
    )
    
    
    out <- list(de=input_list$DE, sd=input_list$SD) # temporary quick job
    
    
    expected_tables_here <-
      eval(parse(text = paste0("expected_tables$H", hierachy)))
    
    for (i in c(3:length(expected_tables_here$table_names))) {
      su <-
        eval(parse(text = paste0(
          "input_list$", expected_tables_here$table_names[[i]]
        )))
      
      names(su) <-
        sub(unique(expected_tables_here$table_names[[i]]), paste0("SU",i-2), names(su))
      
      su$su <- expected_tables_here$su_level[[i]]
      su$hierachy <- hierachy
      h <- i - 1
      su$idAbove <-
        eval(parse(text = paste0(
          "su$", expected_tables_here$table_names[[h]], "id"
        )))
      
      eval(parse(
        text = paste0(
          expected_tables_here$su_level[[i]],
          "_done",
          "<- select(su, any_of(c(var_names,paste0('SU',i-2,var_names))))" #hack
        )
      ))
      
      # # Create list with the table name
      # eval(parse(
      #   text = paste0(
      #     "out$",
      #     expected_tables_here$su_level[[i]],
      #     "$name",
      #     " = ",
      #     "'",
      #     unique(su$recType),
      #     "'"
      #   )
      # ))
      

      
      # Create list with the design variables
      eval(parse(
        text = paste0(
          "out$",
          expected_tables_here$su_level[[i]],
#          "$designTable",
          " = ",
          expected_tables_here$su_level[[i]],
          "_done"
        )
      ))
      
      
      
      # Create list with the inclusion probabilities
      
      # Create list with the selection probabilities
      
      # Create list with combined inclusion probabilities
    }
    
    return(out)
  }


test <- out

# make this general for n SU's
a <- merge(test$su1,test$su2,by.x='SU1id',by.y='idAbove')
a <- merge(a,test$su3,by.x='SU2id',by.y='idAbove')

#a <- test$su1
a$superProb <- a$SU1prob# * a$SU2prob * a$SU3prob
sum(1/a$superProb,na.rm=T) / 4#quarters - should be the number of hauls

a$SU1total * a$superProb

a %>% group_by(SU1stratum) %>% 
  summarise(total=sum(SU3total * superProb,na.rm=T),
            mean=mean(SU3total * superProb,na.rm=T))
a %>% group_by() %>% summarise(total=sum(SU1total * superProb,na.rm=T),mean=mean(SU1total * superProb,na.rm=T))

head(subset(a,is.na(superProb)))
subset(H1_upper$VS,VSid==10)
