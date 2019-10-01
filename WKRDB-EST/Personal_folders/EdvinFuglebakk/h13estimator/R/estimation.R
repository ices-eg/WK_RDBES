library(data.table)

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
#' @return data.table with proportions, format described in \code{\link[h13estimator]{estimateSAProportions}}
#' @export
calculateBVProportions <- function(BV, type, catVar="BValue", stratified=T){

  bvtyped <- BV[BV$BVtype==type,]

  if (!stratified){
    bvtyped$BVstratum <- "sample"
  }

  if (stratified & any(bvtyped$BVstratification==codelist$RS_Stratfification$unstratified)){
    stop("Running stratified calculation, but not all records are stratified")
  }

  if (any(is.na(bvtyped[[catVar]]))){
    stop("Can not calculate proportions with missing observations")
  }

  annotateSelMet <- function(selmet){
    if (length(unique(selmet))!=1){
      stop("Can not annotated selectionmethod for mixed or missing selectionmethods")
    }
    else{
      return(selmet[1])
    }
  }

  counts <- aggregate(list(count=bvtyped[[catVar]]), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid, group=bvtyped[[catVar]]), FUN=length)
  totals <- aggregate(list(total=bvtyped[[catVar]]), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=length)
  selmet <- aggregate(list(BVselectMeth=bvtyped$BVselectMeth), by=list(stratum=bvtyped$BVstratum, SAid=bvtyped$SAid), FUN=annotateSelMet)
  proportions <- merge(counts, totals, all.x=T)
  proportions <- merge(proportions, selmet)
  proportions$proportion <- proportions$count / proportions$total

  return(as.data.table(proportions[,c("SAid", "stratum", "group", "proportion", "BVselectMeth")]))
}

#' Estimate proportions for CA table
#' @description
#'  Estimate proportions for the sampling unit that the sample was taken from
#' @details
#'
#'  The table of proportions should be common for proportions calculated form lower hiearchy registrations.
#'
#'   Columns:
#'
#'    stratum: stratification in the lower hiearchy
#'
#'    SAid: the sample the lower hiearchy registrations were recorded for
#'
#'    group: the levels/values for the categorical variable proprotions are obtained (catVar) for (e.g. different ages, different length groups)
#'
#'    proportion: the proportion calculated (proportion of fish in age a, or length group l, etc.)
#'
#'    selectionMethod: the selectionmethod used for selecting fish to measure the catVar. If they selectionMethod is not gived or is mixed, this should be NA.
#'
#' @param SA data.table SA table
#' @param SS data.table SS table
#' @param SL data.table SL table
#' @param species
#' @param proportions data.table proprotions calculated from lower hiearchy registrations
#' @export
estimateSAProportions <- function(SA, SS, SL, species, proportions){

}

# make example for replicate sampling

# introduce bootstrap for single sample
