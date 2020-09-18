h1 <- readRDS("../../../testData/output/DBErawObj/DBErawObj_DK_1966_H1.rds")
lowerEst <- readRDS("../../inputs/LowerHnumberAtAge6.rds")


getReplacementvector <- function(...){
  args <- list(...)
  repvec <- c()
  for (a in args){
    if (length(unique(a))!=1){
      stop("Heterogenous selection methods not supported")
    }
    selmet <- a[1]
    if (selmet == "SRSWR"){
      repvec <- c(repvec, T)
    }
    if (selmet == "SRSWOR"){
      repvec <- c(repvec, F)
    }
  }
  return(repvec)
}

#' adapted from Davids script
#' Hack for now.
#' Will assume this is provided in data.
fixInclusionProbabilities <- function(selmethod, nsamp, ntotal){
  uniqueSelMethod <- unique(selmethod)
  if(length(uniqueSelMethod)>1) { 
    stop ("Cannot proceed: two different selection methods")
  }
  
  if( uniqueSelMethod %in% c("SRSWOR") )
  {
    if(sum(is.na(ntotal))>0) stop ("Cannot proceed: NAs in total")
    if(sum(is.na(nsamp))>0) stop ("Cannot proceed: NAs in sampled")
    
    if (sum(ntotal==0)>0)
    {
      stop ("Cannot proceed: Zeros in BVnumTotal")
    }
    return(nsamp / ntotal)
  }
  else if( uniqueSelMethod %in% c("SRSWR") )
  {
    if(sum(is.na(ntotal))>0) stop ("Cannot proceed: NAs in total")
    if(sum(is.na(nsamp))>0) stop ("Cannot proceed: NAs in sampled")
    
    if (sum(ntotal==0)>0)
    {
      stop ("Cannot proceed: Zeros in BVnumTotal")
    }
    return(1-(1-1/ntotal)^nsamp)
  }
  else{
    stop("SelectionMehtod", uniqueSelMethod, "not supported")
  }
}

h1$VS$VSincProb <- fixInclusionProbabilities(h1$VS$VSselectMeth, h1$VS$VSnumSamp, h1$VS$VSnumTotal)
h1$FT$FTincProb <- fixInclusionProbabilities(h1$FT$FTselectMeth, h1$FT$FTnumSamp, h1$FT$FTnumTotal)
h1$FO$FOincProb <- fixInclusionProbabilities(h1$FO$FOselectMeth, h1$FO$FOnumSamp, h1$FO$FOnumTotal)
h1$SA$SAincProb <- fixInclusionProbabilities(h1$SA$SAselectMeth, h1$SA$SAnumSamp, h1$SA$SAnumTotal)

includeLowerEst <- function(h1, lowerEst){
  if(!all(is.na(h1$SA$SAparSequNum))){
    stop("subsample handling not implemented")
  }
  if (nrow(h1$SA) != length(lowerEst)){
    stop("lower hierarchy estimates are not provided for all samples")
  }
  
  #add lower hiearchy estimation to sample
  lowerEstOrdering <- sapply(lowerEst, FUN=function(x)(x$unitId))
  h1$SA <- h1$SA[match(h1$SA$SAid, lowerEstOrdering),]
  h1$SA$total <- sapply(lowerEst, FUN=function(x)(x$totals))
  
  return(h1)
}

flattenH1 <- function(h1){
  n <- nrow(h1$SA)
  flat1 <- merge(h1$SS,h1$SA, all=T)
  warning("Ignoring species selection")
  stopifnot(nrow(flat1) == n)
  flat1 <- merge(h1$FO,flat1, all=T)
  stopifnot(nrow(flat1) == n)
  flat1 <- merge(h1$FT,flat1, all=T)
  stopifnot(nrow(flat1) == n)
  flat1 <- merge(h1$VS,flat1, all=T)
  stopifnot(nrow(flat1) == n)
  flat1 <- merge(h1$SD,flat1, all=T)
  stopifnot(nrow(flat1) == n)
  flat1 <- merge(h1$DE,flat1, all=T)
  stopifnot(nrow(flat1) == n)
  
  return(flat1)
}

#' Test univariate point estimation with survey package
#' @param h1 hierarchy 1 data
#' @param lowerEst example data for lower hierarchy estimation
#' @return numeric
h1Lumley <- function(h1,lowerEst){
  require(survey)
  
  #
  # attach lower hierarchy estimates to SA - level in RDBES
  #
  
  h1 <- includeLowerEst(h1, lowerEst)
  
  #
  # make h1 into one big table
  #
  
  flat1 <- flattenH1(h1)
  
  #
  # estimate
  #
  
  repVec <- getReplacementvector(h1$VS$VSselectMeth, h1$SA$SAselectMeth)
  if (!all(repVec)){
    # The call to svydesgin below allows specifying with or without replacement sampling across all levels
    # by using either prob or fpc. They cannot be mixed, but can probably be implemented with intermediate estimation.
    stop("without replacement sampling not supported")
  }
  
  # stratification should be easy to include.
  warning("ignoring XXclustering")
  
  design <- svydesign(ids=~flat1$VSid + flat1$FTid + flat1$FOid + flat1$SAid, 
                      prob =~ flat1$VSincProb + flat1$FTincProb + flat1$FOincProb + flat1$SAincProb,
                      strata =~ flat1$VSstratumName + flat1$FTstratumName + flat1$FOstratumName + flat1$SAstratumName,
                      nest=T)
  total <- svytotal(flat1$total, design)
  
  return(total[1])
}

h1LotteryPackage <- function(h1, lowerEst){
  # package under development at: https://github.com/Sea2Data/CatchLotteryEstimation
  library(lotteryEstimator)
  
  #
  # attach lower hierarchy estimates to SA - level in RDBES
  #
  
  h1 <- includeLowerEst(h1, lowerEst)
  
  #
  # make h1 into one big table
  #
  
  flat1 <- data.table::as.data.table(flattenH1(h1))
  
  #convert ids to characters
  flat1$SAid <- as.character(flat1$SAid)
  flat1$FOid <- as.character(flat1$FOid)
  flat1$FTid <- as.character(flat1$FTid)
  flat1$VSid <- as.character(flat1$VSid)
  
  
  #
  # Estimate
  #
  
  # stratification should be easy to include in this case as well.
  warning("ignoring XXclustering stratification handled by setting incl. probabilities")
  warning("Not handling repeated selection of the same sampling units")
  
  sampstat <- function(x){return(x$total)}
  SAest <- function(x){hierarchicalHorvitzThompsonTotals(x, "SAid", subEstimator = sampstat, inclusionProbabilities = "SAincProb")}
  FOest <- function(x){hierarchicalHorvitzThompsonTotals(x, "FOid", subEstimator = SAest, inclusionProbabilities = "FOincProb")}
  FTest <- function(x){hierarchicalHorvitzThompsonTotals(x, "FTid", subEstimator = FOest, inclusionProbabilities = "FTincProb")}
  VSest <- function(x){hierarchicalHorvitzThompsonTotals(x, "VSid", subEstimator = FTest, inclusionProbabilities = "VSincProb")}
  
  return(VSest(flat1))
  
}

totLumley <- h1Lumley(h1, lowerEst)
totLottery <- h1LotteryPackage(h1, lowerEst)

