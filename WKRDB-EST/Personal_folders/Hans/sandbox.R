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

trip <- inner_join(DE, SD, by='DEid') %>% 
  inner_join(., VS, by='SDid') %>%
  inner_join(., VD, by='VDid') %>%
  inner_join(., FT, by=c('VSid','VDid'))
nrow(trip)
nrow(FT)

haul <- inner_join(trip, FO, by='FTid') %>%
  inner_join(., SS, by='FOid') %>%
  filter(FOaggregationLevel == 'H' & SScatchCategory == 'Dis' & 
           FOarea %in% area)

sample <- inner_join(haul, SA, by='SSid') %>%
  filter(SAcommercialSpecies==species)

length <- inner_join(sample, FM, by='SAid')

effort <- CE %>% filter(Area%in%area)

length <- length %>% mutate(RaiseSampleToHaul=SAtotal/SAsampled)
length <- length %>% mutate(RaiseHaulToTrip=FOtotal/FOsampled)

haul$geararea <- NA
haul$geararea[haul$FOarea%in%c('27.7.b','27.7.c','27.7.k') & substring(haul$FOmetier6,1,3)=='OTB'] <- 'OTB7bck'
haul$geararea[haul$FOarea%in%c('27.7.g') & substring(haul$FOmetier6,1,3)=='OTB'] <- 'OTB7g'
haul$geararea[haul$FOarea%in%c('27.7.j','27.7.h') & substring(haul$FOmetier6,1,3)=='OTB'] <- 'OTB7jh'
haul$geararea[haul$FOarea%in%c('27.7.g','27.7.j') & substring(haul$FOmetier6,1,3)=='TBB'] <- 'TBB7gj'
haul$geararea[haul$FOarea%in%c('27.7.g','27.7.j') & substring(haul$FOmetier6,1,3)=='SSC'] <- 'SSC7gj'
haul$geararea[haul$FOarea%in%c('27.7.g','27.7.j') & substring(haul$FOmetier6,1,2)=='GN'] <- 'GNS7gj'
table(haul$geararea,exclude=NULL)

length$geararea <- NA
length$geararea[length$FOarea%in%c('27.7.b','27.7.c','27.7.k') & substring(length$FOmetier6,1,3)=='OTB'] <- 'OTB7bck'
length$geararea[length$FOarea%in%c('27.7.g') & substring(length$FOmetier6,1,3)=='OTB'] <- 'OTB7g'
length$geararea[length$FOarea%in%c('27.7.j','27.7.h') & substring(length$FOmetier6,1,3)=='OTB'] <- 'OTB7jh'
length$geararea[length$FOarea%in%c('27.7.g','27.7.j') & substring(length$FOmetier6,1,3)=='TBB'] <- 'TBB7gj'
length$geararea[length$FOarea%in%c('27.7.g','27.7.j') & substring(length$FOmetier6,1,3)=='SSC'] <- 'SSC7gj'
length$geararea[length$FOarea%in%c('27.7.g','27.7.j') & substring(length$FOmetier6,1,2)=='GN'] <- 'GNS7gj'
table(length$geararea,exclude=NULL)

effort$geararea <- NA
effort$geararea[effort$Area%in%c('27.7.b','27.7.c','27.7.k') & substring(effort$FishingCategoryLvl6,1,3)=='OTB'] <- 'OTB7bck'
effort$geararea[effort$Area%in%c('27.7.g') & substring(effort$FishingCategoryLvl6,1,3)=='OTB'] <- 'OTB7g'
effort$geararea[effort$Area%in%c('27.7.j','27.7.h') & substring(effort$FishingCategoryLvl6,1,3)=='OTB'] <- 'OTB7jh'
effort$geararea[effort$Area%in%c('27.7.g','27.7.j') & substring(effort$FishingCategoryLvl6,1,3)=='TBB'] <- 'TBB7gj'
effort$geararea[effort$Area%in%c('27.7.g','27.7.j') & substring(effort$FishingCategoryLvl6,1,3)=='SSC'] <- 'SSC7gj'
effort$geararea[effort$Area%in%c('27.7.g','27.7.j') & substring(effort$FishingCategoryLvl6,1,2)=='GN'] <- 'GNS7gj'
table(effort$geararea,exclude=NULL)


effortTotal <- effort %>% group_by(geararea) %>% summarise(hoursTotal=sum(FishingSoakingTime))
effortSampled <- haul %>% group_by(geararea) %>% summarise(hoursSampled=sum(FOduration/60))
effort1 <- left_join(effortTotal, effortSampled, by='geararea') %>%
  mutate(RaiseTripToFleet=hoursTotal/hoursSampled)

length <- left_join(length, effort1, by='geararea')


out <- length %>% group_by(geararea,length=FMclass/10) %>% 
  summarise(NumSampled=sum(FMnumberAtUnit),
     NumRaised=sum(FMnumberAtUnit*RaiseSampleToHaul*RaiseHaulToTrip*RaiseTripToFleet/1000))

ggplot(out) + geom_line(aes(length,NumRaised,col=geararea))


source('./WKRDB-EST/Personal_folders/Hans/ReadIntercatch.R')
IC <- ReadIntercatch('./WKRDB-EST/Personal_folders/Hans/len_intercatch_had.27.7b-k_disc2016.csv')

out1 <- IC$SD %>% group_by(gear=substring(Fleet,1,7),area=FishingArea,length=AgeLength) %>%
  summarise(NumRaised=sum(NumberCaught))

ggplot(out) + geom_line(aes(length,NumRaised,col=geararea))
ggplot(out1) + geom_line(aes(length,NumRaised,col=paste(gear,area)))


out <- subset(length,!is.na(geararea)) %>% group_by(length=FMclass/10,type='RDBES') %>% 
  summarise(NumSampled=sum(FMnumberAtUnit),
            NumRaised=sum(FMnumberAtUnit*RaiseSampleToHaul*RaiseHaulToTrip*RaiseTripToFleet/1000))
out1 <- IC$SD %>% group_by(length=AgeLength,type='OldStyle') %>%
  summarise(NumRaised=sum(NumberCaught))
ggplot(rbind(out,out1)) + geom_line(aes(length,NumRaised,col=type))


###############
library(RODBC)

Q <- "
  select Year
     ,VesselId
     ,sum(case when speciesclass = 'pelagic' then 0 else EstKgWeightSUM end) as LandWt
  from (
     select datepart(year,d.LandingDate) as year
        ,v.vesselid
        ,case when s.SpeciesClass = 'pelagic' then 'pelagic' else 'other' end as speciesclass
        ,d.EstKgWeightSUM
     from Declarations as d 
        join SpeciesLookup as s 
           on d.SpeciesID = s.SpeciesID 
        join Vessels as v 
           on d.VesselID = v.VesselID 
        left join IcesLookup i
           on d.Division = i.IcesDivision
        left join stecf.DCF_Metiers_Logbooks a
           on a.landingdate = d.LandingDate
           and a.LogID = d.LogID
           and a.VesselID = d.VesselID
           and a.ICESDivisionCorrect = i.ICESDivisionCorrect
           and coalesce(a.MeshsizeSum,-999) = coalesce(d.MeshSizeSum,-999)
           and a.[Original_Gear] = case when d.GearType is not null and len(d.GearType)>=2 then substring (d.GearType, 1, charindex (' ', d.GearType, 1) - 1)     
                                      else 'NO' end
     where v.VesselProvenance='Ireland' 
        and substring(a.Agg_Metier,1,7) in ('OTB_CRU','OTB_DEF','TBB_DEF','SSC_DEF','GNS_DEF')
        and datepart(year,d.LandingDate) = 2016
  ) a
  group by Year
     ,VesselId
  having sum(case when speciesclass = 'pelagic' then EstKgWeightSUM else 0 end) < 0.5 * sum(EstKgWeightSUM)
"


channel <- odbcDriverConnect("Driver=SQL Server; Server=VMFSSDEV02; Database=Logbooks_20190429")
  logbooks <- sqlQuery(channel,Q)
close(channel)

logbooks$selectionProb <- logbooks$LandWt/sum(logbooks$LandWt)
logbooks$VDencryptedCode <- paste0('2016L',logbooks$VesselId)
logbooks1 <- logbooks %>% select(VDencryptedCode,selectionProb)

length <- left_join(length,logbooks1, by='VDencryptedCode')
table(is.na(length$selectionProb))
  
smalln <- length(unique(subset(haul,!is.na(geararea))$VDid))
length <- length %>% mutate(inclusionProb=1-(1-selectionProb)^smalln)
length <- length %>% mutate(RaiseTripToFleetUnequal=smalln/inclusionProb)


out2 <- subset(length,!is.na(geararea)) %>% group_by(length=FMclass/10,type='RDBES unequal') %>% 
  summarise(NumSampled=sum(FMnumberAtUnit),
            NumRaised=sum(FMnumberAtUnit*RaiseSampleToHaul*RaiseHaulToTrip*RaiseTripToFleetUnequal/1000))
ggplot(rbind(out,out1,out2)) + geom_line(aes(length,NumRaised,col=type))




