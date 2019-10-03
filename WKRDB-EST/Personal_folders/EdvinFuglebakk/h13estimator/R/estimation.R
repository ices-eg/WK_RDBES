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

#' Calcuates the proportion of fishing operations in each strata
#' @noRd
#' @keywords internal
getPropStrataFO <- function(FO){
  # deal with single strata separately, so that we can tolerate that FOtotal is not set for uneq prob sampling
  propStrata <- data.table(FOid=integer(), stratum=character(), proportionStrata=numeric())
  if (length(unique(FO$FOstratum))==1){
    singlestratum <- unique(FO[,c("FOid", "FOstratum")])
    singlestratum <- data.table(FOid=singlestratum$FOid, stratum=singlestratum$FOstratum, proportionStrata=rep(1, nrow(singlestratum)))
    propStrata <- rbind(propStrata, singlestratum)
  }
  else{
    countstrata <- aggregate(list(countStrata=FO$FOtotal), by=list(stratum=FO$FOstratum, FOid=FO$FOid), FUN=annotateCountStrata)
    stratatotal <- aggregate(list(stratatotal=countstrata$countStrata), by=list(FOid=countstrata$FOid), FUN=sum)
    propstrata <- merge(countstrata, stratatotal, all.x=T)
    propstrata$proportionStrata <- propstrata$countStrata / propstrata$stratatotal
  }

  return(propStrata)
}

#' Add non-present ages
#' @description Adds ages not present in a fishingoperation estimate. Treat as estimated to 0.
#' @param caaFO data.table data table with estimates
#' @param ages all age groups
#' @noRd
#' @keywords internal
getAllAgesFO <- function(caaFO, ages){
  allages <- data.table(age=as.character(rep(ages, length(unique(caaFO$FOid)))), FOid=sort(rep(unique(caaFO$FOid), length(ages))))
  allages <- merge(allages, unique(caaFO[,c("FOid", "stratum", "proportionStrata", "FOprob")]), all.x=T)
  allages <- merge(allages, unique(caaFO[,c("FOid", "age", "numberAtAge")]), by=c("FOid", "age"), all.x=T)
  #
  # age groups not sampled are estimated to be zero in haul
  #
  allages[is.na(allages$numberAtAge),"numberAtAge"] <- 0

  return(allages)
}

#
#
# Functions for coding assumptions.
# These allow separation of assumptions from more rigorous implementations
#
#

#' Recode data table selection methods
#' @description
#' Recodes data table to reflect assumptions about selection methods.
#' For instance SRSWR can be justified for SRSWOR when the sampled fraction is small.
#' @param datatable data.table table in the RDBES data model, eg. SA or BV.
#' @param actual character() The selection method that assumptions are to be applied for
#' @param assumed character() The selection methods that is assumped
#' @export
assumeSelectionMethod <- function(datatable, actual, assumed){

  if ("SArecType" %in% names(datatable)){
    datatable[datatable$SAselectMeth == actual, "SAselectMeth"] <- assumed
  } else if ("BVrecType" %in% names(datatable)){
    datatable[datatable$BVselectMeth == actual, "BVselectMeth"] <- assumed
  } else if ("FOrecType" %in% names(datatable)){
    datatable[datatable$FOselectMeth == actual, "FOselectMeth"] <- assumed
  }
  else{
    stop("Table not supported")
  }

  return(datatable)
}

#' Assign constant variance for catch at age in numbers to fishing operations
#' @description Assigns a constant variance  to each fishing operation.
#' The purpose of the function is to make assumption explicit when design does not allow for design based estimation of variance of some statistic.
#' For instance a constant of zero can be assigned to assume that variance at a certain level is negliable and therefore not estimated.
#' @param caaFO data.table format described in \code{\link[h13estimator]{estimateFOCatchAtAge}}.
#' @param constant numeric() the constant variance to assign to fishing operations
#' @param ages character() the ages to give covariances for
#' @return list() a list of variance-covariance matrices, identified by FOid
#' @export
assumeFOconstantVar <- function(caaFO, constant, ages){

  constant_matrix <- diag(rep(constant, length(ages)))
  colnames(constant_matrix) <- ages
  rownames(constant_matrix) <- ages

  variances <- list()
  for (f in caaFO$FOid){
    variances[[f]] <- constant_matrix
  }

  return(variances)
}



#
#
# Lower hiearchy calculations
#
#



#' Calculate mean for BV table
#' @description
#'  Calculate mean value over sampled fish for some continous variable
#' @details
#'  If output is grouped in strata, encode all stratification in the BV table.
#'  If output is not grouped in strata, it will be treated as single stratum calculation for a stratum called "measurements"
#'
#' @param BV data.tabke BV table
#' @param BVtype charcter() identifies the BVtype records to include in calculation
#' @param contVar charcter() name of continous variable in BV
#' @param stratified logical() determines wheter output should be grouped in straa.
#' @return data.table with means, columns:
#'
#'    \item{stratum}{stratification in the lower hiearchy}
#'    \item{SAid}{the sample the lower hiearchy registrations were recorded for}
#'    \item{mean}{the mean value calculated (mean weight of fish, mean length, etc.)}
#'    \item{unit}{the unit for the mean value (mean weight of fish, mean length, etc.)}
#'    \item{proportionStrata}{the proportion of the sampled fish in each strata from any stratification done during measurment (lower hiearchies)}
#'    \item{selectionMethod}{the selectionmethod used for selecting fish to measure the contVar If the selectionMethod is not gived or is mixed, this should be NA.}
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
    bvtyped$BVstratum <- "measurements"
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
#'  If output is grouped in strata, encode all stratification in the BV table.
#'  If output is not grouped in strata, it will be treated as single stratum calculation for a stratum called "measurements"
#'
#' @param BV a BV table
#' @param BVtype character() identifies the BVtype records to include in calculation
#' @param catVar character() name of categorical variable in BV.
#' @param stratified logical() determines wheter output should be grouped in strata.
#' @return data.table with columns:
#'
#'    \item{stratum}{stratification in the lower hiearchy}
#'    \item{SAid}{the sample the lower hiearchy registrations were recorded for}
#'    \item{group}{the levels/values for the categorical variable proprotions are obtained (catVar) for (e.g. different ages, different length groups)}
#'    \item{proportion}{the proportion calculated (proportion of fish in age a, or length group l, etc.)}
#'    \item{proportionStrata}{the proportion of the sampled fish in each strata from any stratification done during measurment (lower hiearchies)}
#'    \item{selectionMethod}{the selectionmethod used for selecting fish to measure the catVar. If the selectionMethod is not gived or is mixed, this should be NA.}
#'
#' @export
calculateBVProportions <- function(BV, type, catVar="BVvalue", stratified=T){

  bvtyped <- BV[BV$BVtype==type,]

  if (nrow(bvtyped) == 0){
    stop(paste("No records of BVtype", type))
  }

  if (!stratified){
    bvtyped$BVstratum <- "measurements"
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
#'  Estimate catch at age in numbers for the sampling unit that the ultimate sample was taken from
#'  Estimation follows estimator for Herring lottery proposed by Vølstad and Christman,
#'  but attempted generalised so that age sampling can be done by other stratification variables than length.
#'
#' @details
#'  Vølstad and Christman suggests estimating the total number of fish in haul via mean weight from a regression of weight on length,
#'  here mean weight is estimated as a weighted sum of strata means (e.g. lengthgroup means).
#'
#'  If output is grouped in strata, encode all stratification in the BV table.
#'  If output is not grouped in strata, it will be treated as single stratum calculation for a stratum called "sample"
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
#' @param species character() aphia code for species to estimate for
#' @param proportionAtAge data.table proportions in each age group calculated from lower hiearchy registrations (format described in e.g. \code{\link[h13estimator]{calculateBVProportions}})
#' @param meanWeights data.table mean weight of fish calculated from lower hiearchy registrations. (format described in e.g. \code{\link[h13estimator]{calculateBVmeans}})
#'  Can be NULL if none of the samples are sampled by weight (SAunitType=Kg)
#' @param stratified logical() determines whether output should be grouped in strata.
#' @return data.table with columns:
#'
#'    \item{SAid}{the sample the catch at age was estimated from}
#'    \item{age}{the age the catch at age was estimated for}
#'    \item{numbeAtAge}{the estimated number of fish cought at this age}
#'    \item{stratum}{any stratum the catch at age was estimated for}
#'    \item{selectionMethod}{the selectionmethod used for selecting samples. If the selectionMethod is not given or is mixed, this should be NA.}
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

  nfish <- data.table(SAid=integer(), nfish=integer())

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

  numAgeSample <- as.data.table(merge(numAgeSample, sastrata, all.x=T))

  keepcols <- c("SAid", "age", "numberAtAge", "stratum")
  numAgeSample <- numAgeSample[, keepcols, with=F]

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
  propStrata <- data.table(SAid=integer(), stratum=character(), proportionStrata=numeric())
  if (length(unique(SAtarget$SAstratum))==1){
    singlestratum <- unique(numAgeSample[,c("SAid", "stratum")])
    singlestratum$proportionStrata <- 1
    propStrata <- rbind(propStrata, singlestratum)
  } else{
    stop("Estimation for stratified sampling not implemented for SA")

    # Do as for nfish, calculate strata proportions conditioned on SA$SAunitType
  }


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




#
#
# Sampling hiearchy point estimators
#
#





#' Estimates catch at age in number for a fishing operation (FO table)
#' @description Estimates catch at age in number for a fishing operation (FO table) that was sampled directly without intermediate sampling levels
#' @details
#'  If output is grouped in strata, encode all stratification in the BV table.
#'  If output is not grouped in strata, it will be treated as single stratum calculation for a stratum called "fishingoperations"
#'
#'  If samples (caaSA) where selected by unsupported selection methods, estimation will not procedd.
#'  Supported selection methods are: SRSWR, SRSWOR, and CENSUS
#' @param FO data.table FO table
#' @param SS data.table SS table
#' @param SA data.table SA table
#' @param caaSA data.table with estimates for number at age in samples, format described in \code{\link[h13estimator]{estimateSAcaa}}
#' @param stratified logical() determines whether output should be grouped in strata.
#' @return data.table with columns
#'
#'  \item{FOid}{identifies the fishing operation estimate was made for}
#'  \item{stratum}{any stratum the catch at age was estimated for}
#'  \item{age}{the age the catch in numbers was estimated for}
#'  \item{numberAtAge}{the estimated number of fish at the given age in the given stratum}
#'  \item{FOselectMeth}{the selectionmethod used for selecting fishingoperations. If the selectionMethod is not given or is mixed, this should be NA.}
#'
#' @export
estimateFOCatchAtAge <- function(FO, SS, SA, caaSA, stratified=T){

  supportedSAselectMeth <- c(codelist$RS_SelectionMethod$SRSWR, codelist$RS_SelectionMethod$SRSWOR, codelist$RS_SelectionMethod$CENSUS)

  if (!all(caaSA$SAselectMeth %in% supportedSAselectMeth)){
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

  caaSA <- merge(caaSA, samplesWest[,c("SAid", "FOid")], by="SAid", all.x=T)
  meanCaa <- aggregate(list(meanNumberAtAge=caaSA$numberAtAge), by=list(FOid=caaSA$FOid, stratum=caaSA$stratum, age=caaSA$age), FUN=mean) #mean within strata
  meanCaa <- merge(meanCaa, unique(caaSA[,c("FOid", "stratum", "proportionStrata")]), all.x=T)
  meanCaa$wmean <- meanCaa$meanNumberAtAge * meanCaa$proportionStrata
  numAgeFO <- aggregate(list(numberAtAge=meanCaa$wmean), by=list(FOid=meanCaa$FOid, age=meanCaa$age), FUN=sum) #strata weighted sum across strata

  #
  # Calculate proportions in each strata
  #

  propStrata <- propStrata <- getPropStrataFO(FO)


  numAgeFO <- merge(numAgeFO, propStrata[,c("FOid", "stratum", "proportionStrata")], all.x=T)

  #
  # add selection methods
  #

  selmet <- aggregate(list(FOselectMeth=FO$FOselectMeth), by=list(stratum=FO$FOstratum, FOid=FO$FOid), FUN=annotateSelMet)
  numAgeFO <- merge(numAgeFO, selmet)


  return(numAgeFO)
}

#' Hansen-Hurwitz estimator for catch at age in numbers
#' @description Estimates catch at age in numbers from a selection of fishing operations (FO table) that was sampled with unequal probability
#' @details
#'  If fishingoperations (caaFO) were selected by unsupported selection methods, estimation will not procedd.
#'  Supported selection methods are: UPSWR
#' @param FO data.table FO table
#' @param caaFO data.table with estimates of catch at age for each fishing operation, format described in \code{\link[h13estimator]{estimateFOCatchAtAge}}
#' @param ages character() vector of ages to estimate for
#' @return data.table with columns
#'
#'  \item{age}{age for which catch at age was estimated}
#'  \item{numberAtAge}{The estimated number of fish at age.}
#'
#' @export
estimateTotalHH <- function(FO, caaFO, ages=as.character(seq(0,max(as.integer(caaFO$age))))){

  supportedFOselectMeth <- c(codelist$RS_SelectionMethod$UPSWR)

  if (!all(caaFO$FOselectMeth %in% supportedFOselectMeth)){
    stop("Some sample estimates where obtained by unsupported selection methods")
  }

  #
  # caaFO contains estimates for only the age groups present
  # collate here for all age groups
  #
  caaFO <- merge(caaFO, FO[,c("FOid","FOprob")], all.x=T)
  allages <- getAllAgesFO(caaFO, ages)

  #
  # estimate pr strata
  #
  allages$wNumberAtAge <- allages$numberAtAge / allages$FOprob
  result_by_strata <- as.data.table(aggregate(list(numberAtAge=allages$wNumberAtAge), by=list(stratum=allages$stratum, age=allages$age), FUN=mean))

  #
  # estimate across strata
  #
  result_total <- as.data.table(aggregate(list(numberAtAge=result_by_strata$numberAtAge), by=list(age=result_by_strata$age), FUN=sum))
  result_total$age <- as.character(result_total$age)

  return(result_total)

}


#
#
# Sampling hiearchy variance estimators
#
#





#' Hansen-Hurwitz estimator for variance of catch at age in numbers
#' @description Estimates variance of catch at age in numbers from a selection of fishing operations (FO table) that was sampled with unequal probability
#' @param FO data.table FO table
#' @param caaHH data.table with estimates of total catch at age, format described in \code{\link[h13estimator]{estimateTotalHH}}
#' @param caaFO data.table with estimates of catch at age for each fishing operation, format described in \code{\link[h13estimator]{estimateFOCatchAtAge}}
#' @param varFO list of variance-covariance matrices with estimates of variance of catch at age for each fishing operation, format described in \code{\link[h13estimator]{assumeFOconstantVar}}
#' @return data.frame with columns
#' @export
estimateTotalHHVar <- function(FO, caaHH, caaFO, varFO){

    if (length(unique(lapply(varFO, dim))) != 1){
    stop("varFO contains matrices of different dimensions")
  }
  if (length(unique(lapply(varFO, colnames))) != 1){
    stop("varFO contains matrices with different column names")
  }
  if (length(unique(lapply(varFO, rownames))) != 1){
    stop("varFO contains matrices with different row names")
  }
  if (any(rownames(varFO[[1]]) != caaHH$age)){
    stop("varFO rownames that differs from caaHH$age")
  }
  if (any(colnames(varFO[[1]]) != caaHH$age)){
    stop("varFO colnames that differs from caaHH$age")
  }
  if (any(unlist(unique(lapply(varFO, dim))) != c(length(caaHH$age), length(caaHH$age)))){
    stop("varFO dimensions does not match caaHH dimensions")
  }

  if (length(unique(caaFO$FOid)) != length(varFO)){
    stop("number of fishingoperations in caaFO and varFO differ")
  }

  if (length(unique(caaFO$stratum))!=1){
    stop("Estimation from stratified station not supported")

    #need stratified variant of caaHH, option to estimateTotalHH ?
  }

  #
  # caaFO contains estimates for only the age groups present
  # collate here for all age groups
  #
  caaFO <- merge(caaFO, FO[,c("FOid","FOprob")], all.x=T)
  ages <- caaHH$age
  caaFO <- getAllAgesFO(caaFO, ages)

  if (!all(rep(caaHH$age, length(varFO)) == caaFO$age)){
    stop("Error: ages are ordered differently un caaHH and varFO")
  }

  caaHH$totalNumberAtAge <- caaHH$numberAtAge
  caaFO <- merge(caaHH[, c("age", "totalNumberAtAge")], caaFO, by="age", all.x=T)

  sqWdevMatrix <- function(x){
    mat <- outer(x,x)
    rownames(mat) <- caaHH$age
    colnames(mat) <- caaHH$age
    return(mat)
  }


  #
  # Estimate between-haul variance
  #

  caaFO$pWmDnumberAtAge <- caaFO$numberAtAge * (1/caaFO$FOprob) - caaFO$totalNumberAtAge
  outerDev <- aggregate(list(SqWeigthedDev=caaFO$pWmDnumberAtAge), by=list(FOid=caaFO$FOid, stratum=caaFO$stratum), FUN=sqWdevMatrix, simplify=F)

  SumOuterDev <- outerDev$SqWeigthedDev[[1]]
  for (m in 2:(length(outerDev$SqWeigthedDev))){
    SumOuterDev <- SumOuterDev + outerDev$SqWeigthedDev[[m]]
  }

  n <- length(outerDev$SqWeigthedDev)
  betweenHaulVar <- SumOuterDev * (1 / (n*(n - 1)))

  #
  # Estiamte within-haul variance
  #

  SumWithinH <- varFO[[1]]
  for (m in 2:length(varFO)){
    prob <- FO[FO$FOid==m,"FOprob"]
    SumWithinH <- SumWithinH + varFO[[m]] * (1 / prob**2)
  }
  withinHaulVar <- SumWithinH * (1 / n**2)


  var <- betweenHaulVar + withinHaulVar

  return(var)

}
