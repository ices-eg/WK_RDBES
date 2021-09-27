## Load the csv files from our data-raw

unzip(zipfile = "data-raw/exampleData.zip", exdir = "data-raw")

h1Example <- list()
h1Example[["DE"]] <- read.csv("data-raw/H1_DE.csv", stringsAsFactors = FALSE)
h1Example[["SD"]] <- read.csv("data-raw/H1_SD.csv", stringsAsFactors = FALSE)
h1Example[["VS"]] <- read.csv("data-raw/H1_VS.csv", stringsAsFactors = FALSE)
h1Example[["FT"]] <- read.csv("data-raw/H1_FT.csv", stringsAsFactors = FALSE)
h1Example[["FO"]] <- read.csv("data-raw/H1_FO.csv", stringsAsFactors = FALSE)
h1Example[["SS"]] <- read.csv("data-raw/H1_SS.csv", stringsAsFactors = FALSE)
h1Example[["SA"]] <- read.csv("data-raw/H1_SA.csv", stringsAsFactors = FALSE)
h1Example[["FM"]] <- read.csv("data-raw/H1_FM.csv", stringsAsFactors = FALSE)
h1Example[["BV"]] <- read.csv("data-raw/H1_BV.csv", stringsAsFactors = FALSE)
h1Example[["SL"]] <- read.csv("data-raw/H1_SL.csv", stringsAsFactors = FALSE)
h1Example[["VD"]] <- read.csv("data-raw/H1_VD.csv", stringsAsFactors = FALSE)

usethis::use_data(h1Example, overwrite = TRUE)
