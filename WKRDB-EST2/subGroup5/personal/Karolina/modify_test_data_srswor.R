rm(list=ls())
library(tidyverse)
# LOAD test data 
getwd() 
testdat <- readRDS("./WKRDB-EST2/testData/output/DBErawObj/DBErawObj_DK_1966_H1.RDS")
set.seed(1)

FM <- testdat[["FM"]]
BV <- testdat[["BV"]]

SU <- merge(FM, BV , by = "FMid")
age <- distinct(SU, FMclass, BVvalue)

FM2 <- FM1 <- FM


# MODIFY data
# Sample up to 10 fish per LC and randomly select a X number of fish for ageing (always less than sampled per LC) from each LC
FM1$FMnumAtUnit <- sample(seq(2, 10), nrow(FM1), replace = TRUE)
FM1$BioS <- sapply(FM1$FMnumAtUnit + 1, sample, 1) - 1
FM1$BioS <- ifelse(FM1$BioS %in% 0, 1, 
                  ifelse(FM1$BioS %in% FM1$FMnumAtUnit, FM1$BioS-1 , FM1$BioS))


newBV1 <-  FM1[rep(seq(nrow(FM1)), FM1$BioS), ]
newBV1 <- merge(newBV1, age, by = "FMclass")
FM1 <- FM1[, c(1:11)]

BV1 <- newBV1 %>%
  group_by(FMid) %>%
  mutate(BVid = row_number(),
         BVrecType = "BV",
         BVfishId = row_number(),
         BVstratification = "Y", 
         BVstratumname = "", # what is the code for length stratified ?
         BVtype = "Age",
         BVvalue = BVvalue,
         BVvalTyp = "Year",
         BVmethod = NA,
         BVmeasEquip = NA,
         BVnumTotal = FMnumAtUnit,
         BVnumSamp = BioS,
         BVselProp = NA,
         BVinclProp = NA,
         BVselectMeth = "SRSWOR",
         BVunitName = paste0("BV_unit_", seq(1, unique(BioS))),
         BVsampler = NA,
         FMid = FMid) %>%
  select(BVid, BVrecType, BVfishId, BVstratification, BVstratumname, BVtype, BVvalue, BVvalTyp, BVmethod, BVmeasEquip, BVnumTotal, BVnumSamp, BVselProp, BVinclProp, BVselectMeth, BVunitName, BVsampler, FMid)

myls1  <- list(
  "FM" = FM1,
  "BV" = BV1

)

# Make it a bit more realistic?
# Sample up to 5 fish per LC and randomly select up to 2 number of fish for ageing from each LC
FM2$FMnumAtUnit <- sample(seq(2, 5), nrow(FM2), replace = TRUE)
FM2$BioS <- sample(c(1, 2), nrow(FM2), replace = TRUE)
FM2$BioS <- ifelse(FM2$BioS == FM2$FMnumAtUnit, FM2$BioS-1 , FM2$BioS)

newBV2 <-  FM2[rep(seq(nrow(FM2)), FM2$BioS), ]
newBV2 <- merge(newBV2, age, by = "FMclass")
FM2 <- FM2[, c(1:11)]

BV2 <- newBV2 %>%
  group_by(FMid) %>%
  mutate(BVid = row_number(),
         BVrecType = "BV",
         BVfishId = row_number(),
         BVstratification = "Y", 
         BVstratumname = "", # what is the code for length stratified ?
         BVtype = "Age",
         BVvalue = BVvalue,
         BVvalTyp = "Year",
         BVmethod = NA,
         BVmeasEquip = NA,
         BVnumTotal = FMnumAtUnit,
         BVnumSamp = BioS,
         BVselProp = NA,
         BVinclProp = NA,
         BVselectMeth = "SRSWOR",
         BVunitName = paste0("BV_unit_", seq(1, unique(BioS))),
         BVsampler = NA,
         FMid = FMid) %>%
  select(BVid, BVrecType, BVfishId, BVstratification, BVstratumname, BVtype, BVvalue, BVvalTyp, BVmethod, BVmeasEquip, BVnumTotal, BVnumSamp, BVselProp, BVinclProp, BVselectMeth, BVunitName, BVsampler, FMid)

myls2  <- list(
  "FM" = FM2,
  "BV" = BV2
)

# TODO 
# Estimate incl, selection prob from Dave's script
# source("./WKRDB-EST/Personal_folders/dave/Lower/LowerScript.R")



# SAVE data




# IGNORE - Test code not working
# SU <- merge(FM, BV , by = "FMid")
# newBV <-  SU[rep(seq(nrow(SU)), SU$BioS), ]
# SU$from <- 1
# SU$to <- sample(seq(2, 50), nrow(SU), replace = TRUE)

# bb <- SU %>% 
#   rowwise() %>% 
#   do(merge(as_tibble(.), tibble(z=.$from:.$to), by = NULL)) %>%
#   select( -from, -to )  
