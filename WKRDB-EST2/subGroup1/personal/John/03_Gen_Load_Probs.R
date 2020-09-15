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
source("WKRDB-EST2/subGroup1/personal/John/generateProbs_John.r")


#runs function and assigns 
for(i in InputFiles){
  print(i)
  table_names <- names(get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1]))
  table_names <- table_names[!table_names %in% c("SD","DE","FM","VD","SL")]
  for(k in table_names){
  a <- eval(parse(text=paste0(paste0(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],"$",k,sep=""))))
  print("Selection")
  a[grepl("selProb",names(a))==T] <- generateProbs(a, "selection" )
  print("Inclusion")
  a[grepl("incProb",names(a))==T] <- generateProbs(a, "inclusion" )
  eval(parse(text=paste0(paste0(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],"$",k,sep=""), "<-", "a")))
  rm(a)
  gc()
  print(k)
  }
  rm(table_names)
  gc()
}
#wite out data

# for(i in InputFiles){
# write_rds(get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1]),path= paste(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],".Rds",sep=""))
# }

# 
# names(get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1]))
# 
# get(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1])
# 
# paste0("$",table_names[1],sep="")

eval(parse(text=paste0(paste0(unlist(strsplit(InputFiles[InputFiles==i],"\\."))[1],"$",table_names[1],sep=""))))
a[grepl("numSamp",names(a))==T]

for(k in table_names){
  
  print(k)

}

generateProbs(a,"selection")

c <- unique(a[grepl("selectMeth",names(a))==T])
unique(c[grepl("selectMethCluster",names(c))==F])
