library(dplyr)

load('./WKRDB-EST/Personal_folders/Hans/H1/DE.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/SD.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/VS.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/VD.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/FT.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/FO.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/SS.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/SA.Rdata')
load('./WKRDB-EST/Personal_folders/Hans/H1/FM.Rdata')

trip <- left_join(DE, SD, by='DEid') %>% 
  left_join(., VS, by='SDid') %>%
  left_join(., VD, by='VDid') %>%
  left_join(., FT, by='VSid')

haul <- left_join(trip, FO, by='FTid') %>%
  left_join(., SS, by='FOid') %>%
  filter(FOaggregationLevel == 'H' & SScatchCategory == 'Dis' & 
           FOarea %in% c('27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k'))

sample <- left_join(haul, SA, by='SSid') %>%
  filter(SAcommercialSpecies=='HAD')

length <- left_join(sample, FM, by='SAid')

length <- length %>% mutate(RaiseSampleToHaul=SAtotal/SAsampled)
length <- length %>% mutate(RaiseHaulToTrip=FOtotal/FOsampled)
