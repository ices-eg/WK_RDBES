#Script to load up the RDS and fill out useing the prob functin

##gets files
Input <- "WKRDB-EST2/testData/output/DBErawObj/"
#get file list
InputFiles <- list.files("WKRDB-EST2/testData/output/DBErawObj/")
#ignroe readme
InputFiles <- InputFiles[!InputFiles == "README.rmd"]

#Loads data in
for(i in InputFiles){
  assign(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],readRDS(paste(Input,InputFiles[InputFiles==i],sep="")))
  
}
#function
source("WKRDB-EST2/subGroup1/personal/John/generateProbs.r")


#runs function and assigns 
for(i in InputFiles){
  print(i)
  a <- get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1])$SS
  a$SSselProb <- generateProbs(a, "selection" )
  a$SSincProb <- generateProbs(a, "inclusion" )
  eval(parse(text=paste0(paste0(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],"$SS",sep=""), "<-", "a")))
  rm(a)
  gc()
}

#wite out data

# for(i in InputFiles){
# write_rds(get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1]),path= paste(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],".Rds",sep=""))
# }