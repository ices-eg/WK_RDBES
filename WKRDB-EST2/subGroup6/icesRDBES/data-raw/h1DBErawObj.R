## Load the DBErawObj_DK_1966_H1.rds file from our data-raw

h1DBErawObj <- readRDS("data-raw/DBErawObj_DK_1966_H1.rds")

usethis::use_data(h1DBErawObj, overwrite = TRUE)
