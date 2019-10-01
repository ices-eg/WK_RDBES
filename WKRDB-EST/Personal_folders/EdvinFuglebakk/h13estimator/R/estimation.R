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
  countsample <- aggregate(list(countSample=countstrata$countStrata), by=list(SAid=countstrata$SAid), FUN=sum)
  propstrata <- merge(countstrata, countsample, all.x=T)
  propstrata$proportionStrata <- propstrata$countStrata / propstrata$countSample

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
  countsample <- aggregate(list(countSample=countstrata$countStrata), by=list(SAid=countstrata$SAid), FUN=sum)
  propstrata <- merge(countstrata, countsample, all.x=T)
  propstrata$proportionStrata <- propstrata$countStrata / propstrata$countSample

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
#'  If calculations are not by strata, it will be treated as single stratum calculation for a stratum called "sample"
#'
#' @param SA data.table SA table
#' @param SS data.table SS table
#' @param SL data.table SL table
#' @param species
#' @param proportionAtAge data.table proportions in each age group calculated from lower hiearchy registrations (format described in e.g. \code{\link[h13estimator]{calculateBVProportions}})
#' @param meanWeights data.table mean weight of fish calculated from lower hiearchy registrations. (format described in e.g. \code{\link[h13estimator]{calculateBVmeans}})
#'  Can be NULL if none of the samples are sampled by weight (SAunitType=Kg)
#' @param stratified logical determines whether calculation should be done by strata.
#' @return data.table
#' @export
estimateSAcaa <- function(SA, SS, SL, species, proportionAtAge, meanWeights=NULL, stratified=T){

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

  #
  # Estimate total number of fish in sampling unit SA was taken from (e.g. Fishing Operation)
  #

  supportedUnitTypes <- c(codelist$RS_UnitType$kg, codelist$RS_UnitType$number)
  if (!all(SA$SAunitType %in% supportedUnitTypes)){
    stop(paste("Not all samping unit types supported (SAunitType). Currently suppports:", supportedUnitTypes))
  }

  nfish <- data.frame(SAid=integer(), nfish=integer())

  # handle sampling by weight
  if (any(SA$SAunitType==codelist$RS_UnitType$kg)){
    if (any(meanWeights$unit!=codelist$RS_UnitOfValue$g)){
      stop("Units of mean weights must match that of total weight (SAtotalWtLive)")
    }
    meanWeights$wmean <- meanWeights$mean * meanWeights$proportionStrata
    meanWeightsSample <- aggregate(list(mean=meanWeights$wmean), by=list(SAid=meanWeights$SAid), FUN=sum)
    meanWeightsSample$nfish <- floor((1/meanWeightsSample$mean) * SA$SAtotalWtLive)

    nfish <- rbind(nfish, meanWeightsSample[meanWeightsSample$SAid %in% SA[SA$SAunitType==codelist$RS_UnitType$kg,][["SAid"]],c("SAid", "nfish")])
  }

  # handle sampling by number
  if (any(SA$SAunitType==codelist$RS_UnitType$number)){
    nfish <- rbind(nfish, SA[SA$SAunitType==codelist$RS_UnitType$number, c("SAid", "SAtotal")])
  }
  if (nrow(nfish) != nrow(SA)){
    stop("Could not estimate number of fish for all SAid")
  }


  #
  # Estimate proportion at age across strata
  #

  proportionAtAge$wprop <- proportionAtAge$proportion * proportionAtAge$proportionStrata
  propAgeSample <- aggregate(list(proportion=proportionAtAge$wprop), by=list(SAid=proportionAtAge$SAid, age=proportionAtAge$group), FUN=sum)

  numAgeSample <- merge(propAgeSample, nfish, all.x=T)
  numAgeSample$numberAtAge <- numAgeSample$proportion * numAgeSample$nfish

  sastrata <- SA[,c("SAid", "SAstratum")]
  names(sastrata) <- c("SAid", "stratum")

  numAgeSample <- merge(numAgeSample, sastrata, all.x=T)

  return(numAgeSample[,c("SAid", "age", "numberAtAge", "stratum")])
}


estimateSAcaaCov <- function(){

}

# make example for replicate sampling

# introduce bootstrap for single sample
