

library(dplyr)

data_dir <- "Q:/dfad/users/kibi/data/WK_RDBES/"
person_dir <- "Marta/"
hier_dir <- "H1/"


eval(parse(text = paste0(sub("/", "", person_dir), sub("/", "", hier_dir), '<- list()')))

list_files <- list.files(paste(data_dir, person_dir, hier_dir, sep = ""))
for (i in 1:length(list_files)) {
  
  load(paste0(data_dir, person_dir, hier_dir, list_files[i]))
  
  dat <- ungroup(eval(parse(text = paste0(sub(".Rdata", "", list_files[i])))))
  
  eval(parse(text = paste0(sub("/", "", person_dir), sub("/", "", hier_dir), '$', 
                           sub(".Rdata", "", list_files[i]), " = ", 'dat')))
  
  }


