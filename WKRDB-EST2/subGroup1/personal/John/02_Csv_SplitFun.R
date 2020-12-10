library(readr)
# 
# DK_1966_H1 <- read.csv("WKRDB-EST2/testData/output/uploaded/DK_1966_H1.csv",header = F)
# df <- read.csv("WKRDB-EST2/testData/output/uploaded/DK_1966_H1.csv",header = F)
# mapColNamesFieldR$R.Name[grepl("D",substring(mapColNamesFieldR$R.Name,1,1))==T]

# readRDS("WKRDB-EST2/testData/referenceData/mapColNamesFieldR.rds")
DataID <- read.csv("WKRDB-EST2/subGroup1/personal/John/DataID.csv")

# #colnames(df[df$V1 =="DE",]) <- 
# 
# #okay that works
# df <- df[df$V1 =="DE",]
# colnames(df) <-  mapColNamesFieldR$R.Name[grepl("D",substring(mapColNamesFieldR$R.Name,1,1))==T]
# df <- df[mapColNamesFieldR$R.Name[grepl("D",substring(mapColNamesFieldR$R.Name,1,1))==T]]
# df 
# 
# Input <- "WKRDB-EST2/testData/output/uploaded/"
# 
# 
# if(exists("Input") == T){InputFiles <- list.files(Input)}else{
#     stop("Missing Input folder")}
# 
# InputFiles <- InputFiles[!InputFiles == "README.rmd"]
# #InputFiles <- InputFiles[2]
# 
# for(i in InputFiles){
#   print(i)}
# 
# dfCheck <- read.csv(paste(Input,"DK_1966_H10.csv",sep = ""),col.names = paste("V",1:50,sep=""),header = F)
#colnames(df2) <-  mapColNamesFieldR$R.Name[grepl(k,substring(mapColNamesFieldR$R.Name,1,2))==T]
#df2 <- df2[mapColNamesFieldR$R.Name[grepl(k,substring(mapColNamesFieldR$R.Name,1,2))==T]]

Input <- "WKRDB-EST2/testData/output/uploaded/"
#test <- "WKRDB-EST2/testData/output/uploaded/"


if(exists("Input") == T){InputFiles <- list.files(Input)}else{
    stop("Missing Input folder")}

InputFiles <- InputFiles[!InputFiles == "README.rmd"]

for(i in InputFiles){
    df <- read.csv(paste(Input,InputFiles[InputFiles==i],sep = ""),col.names = paste("V",1:50,sep=""),header = F)
    DataID <- read.csv("WKRDB-EST2/subGroup1/personal/John/DataID.csv")
    for(k in unique(df$V1)){
      df2 <- df[df$V1 ==k,]
      
      foobar <- matrix(nrow=dim(df2)[1],ncol=15)
      colnames(foobar) <- unique(DataID$R.Name[DataID$NoNinput =="TRUE"])
      foobar <- as.data.frame(foobar)
      
      colnames(df2) <- DataID$R.Name[DataID$ID==k & DataID$NoNinput =="FALSE"]
      
 
          df2 <- cbind(df2,foobar)
      
          df2 <- df2[DataID$R.Name[DataID$ID==k]]
          
          
      #right sort the folder
      #Frist folder
      if(dir.exists(
        paste("WKRDB-EST2/subGroup1/personal/John/outputs/",
              unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],sep=""))==F){
        dir.create(
          paste("WKRDB-EST2/subGroup1/personal/John/outputs/",
                unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],sep=""))
      }
      

      
      write.csv(df2,file=paste("WKRDB-EST2/subGroup1/personal/John/outputs/",
                              unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],"/",k,".csv",sep=""),row.names = F)
    }
}


# InputFiles[1]
# 
# # check <- as.vector(stringr::str_split(InputFiles[1],"_"))
# unlist(strsplit(InputFiles[1],"\\_|\\,|\\."))[1]
# 
# 
# unlist(strsplit(InputFiles[1],c(".")))
# 
# 
# class(check)
