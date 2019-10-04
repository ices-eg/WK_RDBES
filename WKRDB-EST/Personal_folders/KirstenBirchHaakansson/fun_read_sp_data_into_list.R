

library(dplyr)

data_dir <- "Q:/dfad/users/kibi/data/WK_RDBES/"
person_dir <- "Marta/"
hier_dir <- "H1/"


eval(parse(text = paste0(sub("/", "", person_dir), gsub("/", "", hier_dir), '<- list()')))

list_files <- grep('RDATA', toupper(list.files(paste(data_dir, person_dir, hier_dir, sep = ""))), value = TRUE)
for (i in 1:length(list_files)) {
  
  load(paste0(data_dir, person_dir, hier_dir, list_files[i]))
  
  assign('dat', as.data.frame(ungroup(get( load(paste0(data_dir, person_dir, hier_dir, list_files[i])))))) # rdata file with different name than name of the file
  
  eval(parse(text = paste0(sub("/", "", person_dir), gsub("/", "", hier_dir), '$', 
                           sub(".RDATA", "", list_files[i]), " = ", 'dat')))
  
  }


