
# Testing doDBEestimantionObjUpp

source("./WKRDB-EST2/subGroup7/funs/doDBEestimantionObjUppMid.R")

data_path <- "./WKRDB-EST2/testData/output/DBErawObj/"

# H5 ----

h5 <- readRDS(paste0(data_path, "DBErawObj_DK_1966_H5.rds"))

h5_test <- doDBEestimantionObjUpp(input_list = h5)

# Something wrong with isAbove - needs the FT

# H6 ----

h6 <- readRDS(paste0(data_path, "DBErawObj_DK_1966_H6.rds"))

h6_test <- doDBEestimantionObjUpp(input_list = h6)

# Runs and correct SU's

# H7 ----

h7 <- readRDS(paste0(data_path, "DBErawObj_DK_1966_H7.rds"))

h7_test <- doDBEestimantionObjUpp(input_list = h7)

# Runs, but how will this one link to a the SS - 
# there is a LE (not part of estimation object) in-between

# H8 ----
h8 <- readRDS(paste0(data_path, "DBErawObj_DK_1966_H8.rds"))

h8_test <- doDBEestimantionObjUpp(input_list = h8)

# Something wrong with isAbove - needs the FT

# H9 ----

h9 <- readRDS(paste0(data_path, "DBErawObj_DK_1966_H9.rds"))

h9_test <- doDBEestimantionObjUpp(input_list = h9)



# H10 ----

h10 <- readRDS(paste0(data_path, "DBErawObj_DK_1966_H10.rds"))

h10_test <- doDBEestimantionObjUpp(input_list = h10)

# Runs and correct SU's
