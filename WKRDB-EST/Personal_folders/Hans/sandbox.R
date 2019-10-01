library(dplyr)
library(ggplot2)

load('./WKRDB-EST/Personal_folders/Hans/H1/CE.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/DE.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/SD.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/VS.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/VD.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/FT.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/FO.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/SS.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/SA.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/FM.Rdata')

species <- 'HAD'
area <- c('27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k')

trip <- left_join(DE, SD, by='DEid') %>% 
  left_join(., VS, by='SDid') %>%
  left_join(., VD, by='VDid') %>%
  left_join(., FT, by='VSid')

haul <- left_join(trip, FO, by='FTid') %>%
  left_join(., SS, by='FOid') %>%
  filter(FOaggregationLevel == 'H' & SScatchCategory == 'Dis' & 
           FOarea %in% area)

sample <- left_join(haul, SA, by='SSid') %>%
  filter(SAcommercialSpecies==species)

length <- left_join(sample, FM, by='SAid')

effort <- CE %>% filter(Area%in%area)

length <- length %>% mutate(RaiseSampleToHaul=SAtotal/SAsampled)
length <- length %>% mutate(RaiseHaulToTrip=FOtotal/FOsampled)

effortTotal <- effort %>% group_by(gear=FishingCategoryLvl5) %>% summarise(hoursTotal=sum(FishingSoakingTime))
effortSampled <- haul %>% group_by(gear=substring(FOmetier6,1,7)) %>% summarise(hoursSampled=sum(FOduration/60))

length <- length %>% mutate(gear=substring(FOmetier6,1,7))

length <- left_join(effortTotal, effortSampled, by='gear') %>%
  mutate(RaiseTripToFleet=hoursTotal/hoursSampled) %>%
  right_join(.,length, by='gear')

out <- length %>% group_by(gear,length=FMclass/10) %>% 
  summarise(NumSampled=sum(FMnumberAtUnit),
     NumRaised=sum(FMnumberAtUnit*RaiseSampleToHaul*RaiseHaulToTrip*RaiseTripToFleet/1000))

ggplot(out) + geom_line(aes(length,NumRaised,col=gear))


source('./WKRDB-EST/Personal_folders/Hans/ReadIntercatch.R')
IC <- ReadIntercatch('./WKRDB-EST/Personal_folders/Hans/len_intercatch_had.27.7b-k_disc2016.csv')

out1 <- IC$SD %>% group_by(gear=substring(Fleet,1,7),length=AgeLength) %>%
  summarise(NumRaised=sum(NumberCaught))

ggplot(out) + geom_line(aes(length,NumRaised,col=gear))
ggplot(out1) + geom_line(aes(length,NumRaised,col=gear))



out <- length %>% group_by(length=FMclass/10,type='RDBES') %>% 
  summarise(NumSampled=sum(FMnumberAtUnit),
            NumRaised=sum(FMnumberAtUnit*RaiseSampleToHaul*RaiseHaulToTrip*RaiseTripToFleet/1000))
out1 <- IC$SD %>% group_by(length=AgeLength,type='OldStyle') %>%
  summarise(NumRaised=sum(NumberCaught))
ggplot(rbind(out,out1)) + geom_line(aes(length,NumRaised,col=type))

