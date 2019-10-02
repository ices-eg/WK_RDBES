library(data.table)

#' Checks that all values in the vector are the same, and extracts that value
#' @noRd
#' @keywords internal
annotateOne <- function(vector, errormessage){
  if (length(unique(vector))!=1){
    stop(errormessage)
  }
  else{
    return(vector[1])
  }
}

#' Checks that all selectionmethods are the same and extracts the value
#' @noRd
#' @keywords internal
annotateSelMet <- function(selmet){
  annotateOne(selmet, "Can not annotated selectionmethod for mixed or missing selectionmethods")
}

#' Checks that all units are the same and extracts the value
#' @noRd
#' @keywords internal
annotateUnit <- function(unit){
  return(annotateOne(unit, "Mixed units not supported"))
}

#' Checks that all strata counts (totals) are the same and extracts the value
#' @noRd
#' @keywords internal
annotateCountStrata <- function(BVtotal){
  return(annotateOne(BVtotal, "BVtotal differ within strata"))
}

#' Recode data table selection methods
#' @description
#' Recodes data table to reflect assumptions about selection methods.
#' For instance SRSWR can be justified for SRSWOR when the sampled fraction is small.
#' @param datatable datatable of the RDBES data model, eg. SA or BV.
#' @param actual The selection method that assumptions are to be applied for
#' @param assumed The selection methods that is assumped
#' @export
assumeSelectionMethod <- function(datatable, actual, assumed){

  if ("SArecType" %in% names(datatable)){
    datatable[datatable$SAselectMeth == actual, "SAselectMeth"] <- assumed
  } else if ("BVrecType" %in% names(datatable)){
    datatable[datatable$BVselectMeth == actual, "BVselectMeth"] <- assumed
  }
  else{
    stop("Table not supported")
  }

  return(datatable)
}

#' Calculate mean for BV table
#' @description
#'  Calculate mean value over sampled fish for some continous variable
#' @details
#'  If calculation is done by strata, encode all stratification in the BV table.
#'  If calculations are not by strata, it will be treated as single stratum calculation for a stratum called "sample"
#'
#' @param BV a BV table
#' @param BVtype identifies the BVtype records to include in calculation
#' @param contVar name of continous variable in BV
#' @param stratified logical determines wheter calculation should be done by strata.
#' @return data.table with means, columns:
#'
#'    stratum: stratification in the lower hiearchy
#'
#'    SAid: the sample the lower hiearchy registrations were recorded for
#'
#'    mean: the mean value calculated (mean weight of fish, mean length, etc.)
#'
#'    unit: the unit for the mean value (mean weight of fish, mean length, etc.)
#'
#'    proportionStrata: the proportion of the sampled fish in each strata from any stratification done during measurment (lower hiearchies)
#'
#'    selectionMethod: the selectionmethod used for selecting fish to measure the contVar If the selectionMethod is not gived or is mixed, this should be NA.
#'
#' @export
calculateBVmeans <- function(BV, type, contVar="BVvalue", stratified = T){
  bvtyped <- BV[BV$BVtype==type,]

  if (!(contVar %in% names(BV))){
    stop(paste("Column", contVar, "not present in BV."))
  }
  if (nrow(bvtyped) == 0){
    stop(paste("No records of BVtype", type))
  }

  bvtyped[[contVar]] <- as.numeric(bvtyped[[contVar]])

  if (!stratified){
    bvtyped$BVstratum <- "sample"
  }

  if (stratified & any(bvtyped$BVstratification==codelist$RS_Stratfification$unstratified)){
    stop("Running stratified calculation, but not all records are stratified")
  }

  if (any(is.na(bvtyped[[contVar]]))){
    stop("Can not calculate mean with missing observations")
  }

  means <- aggregate(list(mean=bvtyped[[contVar]]), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=mean)
  selmet <- aggregate(list(BVselectMeth=bvtyped$BVselectMeth), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=annotateSelMet)
  countstrata <- aggregate(list(countStrata=bvtyped$BVtotal), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=annotateCountStrata)
  stratatotal <- aggregate(list(stratatotal=countstrata$countStrata), by=list(SAid=countstrata$SAid), FUN=sum)
  propstrata <- merge(countstrata, stratatotal, all.x=T)
  propstrata$proportionStrata <- propstrata$countStrata / propstrata$stratatotal

  units <- aggregate(list(unit=bvtyped$BVunitVal), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=annotateUnit)
  means <- merge(means, selmet, all.x=T)
  means <- merge(means, units, all.x=T)
  means <- merge(means, propstrata, all.x=T)

  return(as.data.table(means[,c("SAid", "stratum", "mean", "unit", "proportionStrata", "BVselectMeth")]))

}

#' Calculate proportions for BV table
#' @description
#'  Calculates proportions of fish in a sample, by some categorical variable.
#'  Proportion is an observation of the sample, so variance is not defined.
#' @details
#'  If calculation is done by strata, encode all stratification in the BV table.
#'  If calculations are not by strata, it will be treated as single stratum calculation for a stratum called "sample"
#'
#' @param BV a BV table
#' @param BVtype identifies the BVtype records to include in calculation
#' @param catVar name of categorical variable in BV.
#' @param stratified logical determines wheter calculation should be done by strata.
#' @return data.table with columns:
#'
#'    stratum: stratification in the lower hiearchy
#'
#'    SAid: the sample the lower hiearchy registrations were recorded for
#'
#'    group: the levels/values for the categorical variable proprotions are obtained (catVar) for (e.g. different ages, different length groups)
#'
#'    proportion: the proportion calculated (proportion of fish in age a, or length group l, etc.)
#'
#'    proportionStrata: the proportion of the sampled fish in each strata from any stratification done during measurment (lower hiearchies)
#'
#'    selectionMethod: the selectionmethod used for selecting fish to measure the catVar. If the selectionMethod is not gived or is mixed, this should be NA.
#'
#' @export
calculateBVProportions <- function(BV, type, catVar="BVvalue", stratified=T){

  bvtyped <- BV[BV$BVtype==type,]

  if (nrow(bvtyped) == 0){
    stop(paste("No records of BVtype", type))
  }

  if (!stratified){
    bvtyped$BVstratum <- "sample"
  }

  if (stratified & any(bvtyped$BVstratification==codelist$RS_Stratfification$unstratified)){
    stop("Running stratified calculation, but not all records are stratified")
  }

  if (any(is.na(bvtyped[[catVar]]))){
    stop("Can not calculate proportions with missing observations")
  }

  counts <- aggregate(list(count=bvtyped[[catVar]]), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid, group=bvtyped[[catVar]]), FUN=length)
  totals <- aggregate(list(total=bvtyped[[catVar]]), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=length)
  selmet <- aggregate(list(BVselectMeth=bvtyped$BVselectMeth), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=annotateSelMet)
  countstrata <- aggregate(list(countStrata=bvtyped$BVtotal), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=annotateCountStrata)
  stratatotal <- aggregate(list(stratatotal=countstrata$countStrata), by=list(SAid=countstrata$SAid), FUN=sum)
  propstrata <- merge(countstrata, stratatotal, all.x=T)
  propstrata$proportionStrata <- propstrata$countStrata / propstrata$stratatotal

  proportions <- merge(counts, totals, all.x=T)
  proportions <- merge(proportions, selmet, all.x=T)
  proportions <- merge(proportions, propstrata, all.x=T)
  proportions$proportion <- proportions$count / proportions$total

  return(as.data.table(proportions[,c("SAid", "stratum", "group", "proportion", "proportionStrata", "BVselectMeth")]))
}

#' Estimate catch at age in numbers for SA table
#' @description
#'  Estimation follows estimator for Herring lottery proposed by Vølstad and Christman,
#'  but attempted generalised so that age sampling can be done by other stratification variables than length
#'  Vølstad and Christman suggests estimating the total number of fish in haul via mean weight from a regression of weight on length,
#'  here mean weight is estimated as a weighted sum of strata means (e.g. lengthgroup means).
#'
#'  Estimate catch at age in numbers for the sampling unit that the ultimate sample was taken from
#' @details
#'
#'  If calculation is done by strata, encode all stratification in the SA table.
#'  If calculations are not by strata, it will be treated as single stratum calculation for a stratum called "sample".
#'  species is interpreted as a sampling frame paramter, any stratification will be conditioned on sampling for species.
#'  For samples of zero catch, the strata and one age group is provided with number at age reported as 0.
#'
#'  If lower hiearchy estimations (proportionAtAge and meanWeights) were obtained from samples with unsupported selection methods, estimation will not proceed.
#'  Supported selection methods are: SRSWR, SRSWOR, and CENSUS
#'
#'  If samples (SA entries) where selected by unsupported selection methods, estimation will not procedd.
#'  Supported selection methods are: SRSWR, SRSWOR, and CENSUS
#'
#' @param SA data.table SA table
#' @param SS data.table SS table
#' @param SL data.table SL table
#' @param species character aphia code for species to estimate for
#' @param proportionAtAge data.table proportions in each age group calculated from lower hiearchy registrations (format described in e.g. \code{\link[h13estimator]{calculateBVProportions}})
#' @param meanWeights data.table mean weight of fish calculated from lower hiearchy registrations. (format described in e.g. \code{\link[h13estimator]{calculateBVmeans}})
#'  Can be NULL if none of the samples are sampled by weight (SAunitType=Kg)
#' @param stratified logical determines whether calculation should be done by strata.
#' @return data.table with columns:
#'
#'    SAid: the sample the catch at age was estimated from
#'
#'    age: the age the catch at age was estimated for
#'
#'    numbeAtAge: the estimated number of fish cought at this age
#'
#'    stratum: any stratum the catch at age was estimated for
#'
#'    selectionMethod: the selectionmethod used for selecting samples. If the selectionMethod is not given or is mixed, this should be NA.
#'
#'
#' @export
estimateSAcaa <- function(SA, SS, SL, species, proportionAtAge, meanWeights=NULL, stratified=T){

  #
  # checks on lower hiearchy estimates
  #
  supportedBVselectMeth <- c(codelist$RS_SelectionMethod$SRSWR, codelist$RS_SelectionMethod$SRSWOR, codelist$RS_SelectionMethod$CENSUS)

  if (!all(meanWeights$BVselectMeth %in% supportedBVselectMeth)){
    stop("Some mean weghts where obtained by unsupported selection methods")
  }

  if (!all(proportionAtAge$BVselectMeth %in% supportedBVselectMeth)){
    stop("Some age proportions where obtained by unsupported selection methods")
  }

  #
  # checks on species list configuration
  #

  if (any(SS$SSclustering != codelist$RS_Clustering$unclustered) | any(SS$SSstratification != codelist$RS_Stratfification$unstratified) | any(SS$SSselectMeth != codelist$RS_SelectionMethod$CENSUS)){
    stop("Estimation with sampling of species list not implemented")
  }

  sass <- merge(SA[,c("SAid", "SSid")], SS[,c("SSid", "SLid")])
  sasssl <- merge(sass, SL[,c("SLid", "SLsppCode")], by="SLid")
  sasssl_species <- sasssl[sasssl$SLsppCode==species,]
  if (!all(SA$SAid %in% sasssl_species$SAid)){
    stop("Species was not targeted for sampling in all samples. Deal with missing samples.")
  }

  #
  # checks on SA table configuration
  #

  supportedSAselectMeth <- c(codelist$RS_SelectionMethod$SRSWR, codelist$RS_SelectionMethod$SRSWOR, codelist$RS_SelectionMethod$CENSUS)

  if (any(!is.na(SA$SAparentid))){
    stop("Estimation from Subsamples not implemented")
  }

  if (any(SA$SAunitType==codelist$RS_UnitType$kg & SA$SApres!=codelist$RS_Presentation$whole)){
    stop(paste("Sampling by weight requires weights of fish presented as", codelist$RS_Presentation$whole, "(SApres)"))
  }

  if (!stratified){
    SA$SAstratum <- "sample"
  }

  if (stratified & any(SA$SAstratification==codelist$RS_Stratfification$unstratified)){
    stop("Running stratified calculation, but not all records are stratified")
  }

  if (!all(SA$SAselectMeth %in% supportedSAselectMeth)){
    stop("Some samples are selected by unsupported selection methods")
  }

  #
  # Estimation
  #

  SAtarget <- SA[SA$SAsppCode==species,]

  #
  # Estimate total number of fish in sampling unit SA was taken from (e.g. Fishing Operation)
  #

  supportedUnitTypes <- c(codelist$RS_UnitType$kg, codelist$RS_UnitType$number)
  if (!all(SAtarget$SAunitType %in% supportedUnitTypes)){
    stop(paste("Not all samping unit types supported (SAunitType). Currently suppports:", supportedUnitTypes))
  }

  nfish <- data.frame(SAid=integer(), nfish=integer())

  # handle sampling by weight
  if (any(SAtarget$SAunitType==codelist$RS_UnitType$kg)){
    if (is.null(meanWeights)){
      stop(paste("Need meanWeights to estimate total number of fish in samples sampled by weight (SAunitType=", codelist$RS_UnitType$kg, ")", sep=""))
    }
    meanWeights <- meanWeights[meanWeights$SAid %in% SAtarget$SAid,]
    if (any(meanWeights$unit!=codelist$RS_UnitOfValue$g)){
      stop("Units of mean weights must match that of total weight (SAtotalWtLive)")
    }

    meanWeights$wmean <- meanWeights$mean * meanWeights$proportionStrata
    meanWeightsSample <- aggregate(list(mean=meanWeights$wmean), by=list(SAid=meanWeights$SAid), FUN=sum)
    meanWeightsSample$nfish <- floor((1/meanWeightsSample$mean) * SAtarget$SAtotalWtLive)

    nfish <- rbind(nfish, meanWeightsSample[meanWeightsSample$SAid %in% SAtarget[SAtarget$SAunitType==codelist$RS_UnitType$kg,][["SAid"]],c("SAid", "nfish")])
  }

  # handle sampling by number
  if (any(SAtarget$SAunitType==codelist$RS_UnitType$number)){
    nfish <- rbind(nfish, SAtarget[SAtarget$SAunitType==codelist$RS_UnitType$number, c("SAid", "SAtotal")])
  }
  if (nrow(nfish) != nrow(SAtarget)){
    stop("Could not estimate number of fish for all SAid")
  }


  #
  # Estimate proportion at age across strata
  #

  proportionAtAge <- proportionAtAge[proportionAtAge$SAid %in% SAtarget$SAid,]

  proportionAtAge$wprop <- proportionAtAge$proportion * proportionAtAge$proportionStrata
  propAgeSample <- aggregate(list(proportion=proportionAtAge$wprop), by=list(SAid=proportionAtAge$SAid, age=proportionAtAge$group), FUN=sum)

  numAgeSample <- merge(propAgeSample, nfish, all.x=T)
  numAgeSample$numberAtAge <- numAgeSample$proportion * numAgeSample$nfish

  sastrata <- SAtarget[,c("SAid", "SAstratum")]
  names(sastrata) <- c("SAid", "stratum")

  numAgeSample <- as.data.frame(merge(numAgeSample, sastrata, all.x=T))

  keepcols <- c("SAid", "age", "numberAtAge", "stratum")
  numAgeSample <- numAgeSample[, keepcols]

  #
  # add zeroes
  #
  zeroes <- SA[!(SA$SAid %in% SAtarget$SAid),] #function halts with error if missing samples (see above)
  if (nrow(zeroes) > 0){
    someage <- numAgeSample$age[1]
    zeroes$stratum <- zeroes$SAstratum
    zeroes$age <- someage
    zeroes$numberAtAge <- 0
    zeroes <- zeroes[, keepcols, with=F]

    numAgeSample <- rbind(numAgeSample, zeroes)
  }

  #
  # Estimate proportions in each strata
  #
  propStrata <- data.frame(SAid=integer(), stratum=character(), proportionStrata=numeric())
  if (length(unique(SAtarget$SAstratum))==1){
    singlestratum <- unique(numAgeSample[,c("SAid", "stratum")])
    singlestratum$proportionStrata <- 1
    propStrata <- rbind(propStrata, singlestratum)
  } else{
    stop("Estimation for stratified sampling not implemented for SA")

    # Do as for nfish, calculate strata proportions conditioned on SA$SAunitType
  }

  numAgeSample <- merge(numAgeSample, propStrata)


  #
  # add strata proportions
  #

  numAgeSample <- merge(numAgeSample, propStrata[,c("SAid", "stratum", "proportionStrata")])

  #
  # add selection methods
  #
  selmet <- aggregate(list(SAselectMeth=SA$SAselectMeth), by=list(stratum=SA$SAstratum, SAid=SA$SAid), FUN=annotateSelMet)
  numAgeSample <- merge(numAgeSample, selmet)

  return(numAgeSample)
}

#' Estimates catch at age in number for a fishing operation (FO table)
#' @description Estimates catch at age in number for a fishing operation (FO table) that was sampled directly without intermediate sampling levels
#' @details
#'  If calculation is done by strata, encode all stratification in the FO table.
#'  If calculations are not by strata, it will be treated as single stratum calculation for a stratum called "fishingoperations".
#' @param FO data.table FO table
#' @param SS data.table SS table
#' @param SA data.table SA table
#' @param caaSA data.table with estimates for number at age in samples, format described in \code{\link[h13estimator]{estimateSAcaa}}
#' @param stratified logical determines whether calculation should be done by strata.
#' @return data.frame
#' @export
estimateFOCatchAtAge <- function(FO, SS, SA, caaSA, stratified=T){

  supportedBVselectMeth <- c(codelist$RS_SelectionMethod$SRSWR, codelist$RS_SelectionMethod$SRSWOR, codelist$RS_SelectionMethod$CENSUS)

  if (!all(caaSA$SAselectMeth %in% supportedBVselectMeth)){
    stop("Some sample estimates where obtained by unsupported selection methods")
  }


  if (!stratified){
    FO$FOstratum <- "fishingoperations"
  }
  if (stratified & any(FO$FOstratification==codelist$RS_Stratfification$unstratified)){
    stop("Running stratified calculation, but not all records are stratified")
  }

  samplesWest <- SA[SA$SAid %in% caaSA$SAid,]
  samplesWest <- merge(samplesWest, SS[,c("SSid", "FOid")])

  if (!all(FO$FOid %in% samplesWest$FOid)){
    stop("Not all fishing operations have estimate. Deal with missing.")
  }


  #
  # Estimate
  #

  caaSA <- merge(caaSA, samplesWest[,c("SAid", "FOid")], all.x=T)
  meanCaa <- aggregate(list(meanNumberAtAge=caaSA$numberAtAge), by=list(FOid=caaSA$FOid, stratum=caaSA$stratum), FUN=mean) #mean within strata
  meanCaa <- merge(meanCaa, caaSA[,c("FOid", "stratum", "proportionStrata")], all.x=T)
  meanCaa$wmean <- meanCaa$meanNumberAtAge * meanCaa$proportionStrata
  numAgeFO <- aggregate(list(numberAtAge=meanCaa$wmean), by=list(FOid=meanCaa$FOid), FUN=sum) #strata weighted sum across strata

  #
  # Calculate proportions in each strata
  #

  # deal with single strata separately, so that we can tolerate that FOtotal is not set for uneq prob sampling
  propStrata <- data.frame(SAid=integer(), stratum=character(), proportionStrata=numeric())
  if (length(unique(FO$FOstratum))==1){
    singlestratum <- unique(FO[,c("FOid", "FOstratum")])
    names(singlestratum) <- c("FOid", "stratum")
    singlestratum$proportionStrata <- 1
    propStrata <- rbind(propStrata, as.data.frame(singlestratum))
  }
  else{
    countstrata <- aggregate(list(countStrata=FO$FOtotal), by=list(stratum=FO$DOstratum, SAid=FO$SAid), FUN=annotateCountStrata)
    stratatotal <- aggregate(list(stratatotal=countstrata$countStrata), by=list(FOid=countstrata$FOid), FUN=sum)
    propstrata <- merge(countstrata, stratatotal, all.x=T)
    propstrata$proportionStrata <- propstrata$countStrata / propstrata$stratatotal
  }

  numAgeFO <- merge(numAgeFO, propStrata[,c("FOid", "stratum", "proportionStrata")], all.x=T)

  #
  # add selection methods
  #

  selmet <- aggregate(list(FOselectMeth=FO$FOselectMeth), by=list(stratum=FO$FOstratum, FOid=FO$FOid), FUN=annotateSelMet)
  numAgeFO <- merge(numAgeFO, selmet)


  return(numAgeFO)
}


#' Estimate total catch at age for the herring lottery
#' @description Example workflow for estimating from the herring lottery pilot (2018)
#' @details Assumptions made:
#'
#'  - Assumes systematic sampling of typically small fraction of catch as simple random with replacement
#'
#' @noRd
#' @keywords internal
herringlottery_workflow <- function(){
  data <- herringlottery
  proportionsAtAgeBV <- calculateBVProportions(data$BV, "Age", stratified = F)
  meanWeightsBV <- calculateBVmeans(data$BV, "Weight", stratified = F)

  data$SA <- assumeSelectionMethod(data$SA, "SYSS", "SRSWR")
  sampleTotals <- estimateSAcaa(data$SA, data$SS, data$SL, "126417", proportionsAtAgeBV, meanWeightsBV, stratified = F)

  haulTotals <- estimateFOCatchAtAge(data$FO, data$SS, data$SA, sampleTotals, stratified=F)
  #FO totals

  # grand totals

  # between SA variance
  # options:
  # - impute resampled
  # - assume zero
  # - model assisted

  #FO variance

  #grand total variance


  return(haulTotals)
}
