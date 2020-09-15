## Load the csv files from our data-raw

unzip(zipfile = "data-raw/exampleData.zip", exdir = "data-raw")

H1_Example <- list()
H1_Example[['DE']] <- read.csv('data-raw/H1_DE.csv',stringsAsFactors = FALSE)
H1_Example[['SD']] <- read.csv('data-raw/H1_SD.csv',stringsAsFactors = FALSE)
H1_Example[['VS']] <- read.csv('data-raw/H1_VS.csv',stringsAsFactors = FALSE)
H1_Example[['FT']] <- read.csv('data-raw/H1_FT.csv',stringsAsFactors = FALSE)
H1_Example[['FO']] <- read.csv('data-raw/H1_FO.csv',stringsAsFactors = FALSE)
H1_Example[['SS']] <- read.csv('data-raw/H1_SS.csv',stringsAsFactors = FALSE)
H1_Example[['SA']] <- read.csv('data-raw/H1_SA.csv',stringsAsFactors = FALSE)
H1_Example[['FM']] <- read.csv('data-raw/H1_FM.csv',stringsAsFactors = FALSE)
H1_Example[['BV']] <- read.csv('data-raw/H1_BV.csv',stringsAsFactors = FALSE)
H1_Example[['SL']] <- read.csv('data-raw/H1_SL.csv',stringsAsFactors = FALSE)
H1_Example[['VD']] <- read.csv('data-raw/H1_VD.csv',stringsAsFactors = FALSE)

usethis::use_data(H1_Example, overwrite = TRUE)
