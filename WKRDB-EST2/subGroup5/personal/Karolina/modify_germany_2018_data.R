rm(list=ls())
getwd() 

# Dave's function (Est inclprob)
source("./WKRDB-EST2/subGroup5/personal/Karolina/LowerScript.R")
# LOAD Germany's 2018 data

FMG <- readRDS("./WKRDB-EST2/subGroup5/personal/Karolina/FM_Germany_2018.RDS")
BVG <- readRDS("./WKRDB-EST2/subGroup5/personal/Karolina/BV_Germany_2018.RDS")

BVG <- BVG[BVG$BVselectionMethod %in% "SRSWOR", ]

setdiff(BVG$FMid, FMG$FMid)

# MODIFY a bit

FMG$FMid <- gsub("FM_", "", FMG$FMid)
FMG$SAid <- gsub("SA_", "", FMG$FMid)
BVG$BVid <- gsub("BV_", "", BVG$BVid)
BVG$FMid <- gsub("FM_", "", BVG$FMid)
BVG$SAid <- NA

names(FMG) <- c("FMid", "SAid", "FMrecType", "FMclass", "FMnumAtUnit", "FMtype", "FMmeasEquip", "FMaccuracy", "FMsampler", "FMaddGrpMeas", "FMaddGrpMeasType")
names(BVG) <- c("BVid", "SAid", "FMid", "BVrecType", "BVfishId", "BVstratification", "BVstratumName", "BVtype", "BVvalue", "BVvalTyp", "BVmethod", "BVmeasEquip", "BVnumTotal", "BVnumSamp", "BVselProb", "BVincProb","BVselectMeth", "BVunitName", "BVsampler")

# CREATE input list 

myList <- list(
  "FM" = FMG,
  "BV" = BVG
)

# RUN Dave's script (incl probability should be estimated only for BVselMeth = SRSWOR)
# It can only run one estimation at a time 
# Deal with outside by spliting by BVtype and/or BVselectionMethod ?
# A preprocessing script is needed to split, give names to lists/sublists

myoutputA <- getLowerProbs(table = myList, hierarchyType = "A",  BVtype = "age", probType = "inclusion")
myoutputS <- getLowerProbs(table = myList, hierarchyType = "A",  BVtype = "sex", probType = "inclusion")
myoutputM <- getLowerProbs(table = myList, hierarchyType = "A",  BVtype = "maturity", probType = "inclusion")

saveRDS(myoutputA, file = "./WKRDB-EST2/subGroup5/inputs/input_FMBV_Germany_Age.rds")

# TODO
# Try spliting assuming for now the only probType = inclusion 
# Unmatched records 


