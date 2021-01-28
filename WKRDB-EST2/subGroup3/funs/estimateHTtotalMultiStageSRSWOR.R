estimateHTtotalMultiStageSRSWOR <- function(RDBobj,stages=stages,varOfInterest="SAtotalWtLive"){
  # this function calculates, and outputs, 
  # the Horvitz Thompson estimate of the population total of a single univariate variable
  # and the resulting variance of the estimate, assuming SRSWOR (simple random sampling without replacement)
  # is used at each stage
  # The function has 3 arguments:
  # RDBobj - the object containing the data, in RDBES format
  # stages - the sampling stages in the data - avoids the need to specify heirarchies
  # varOfInterest - a character string specifying the name of the variable 
  # for which we are estimating the population total (the "y variable")
 
  # set up some objects
  nStage <- length(stages)
  idPrev <- idList <- piList <- list()
  y <- nTotal <- nSamp <- meanStage <- ymean <- list()
  ssqTerm <- ssqStage <- tStage <- nTotStage <- nSampStage <- list()
  y[[nStage]] <- RDBobj[[stages[[nStage]]]][,varOfInterest]
  estVarTot <- vv <- list()
 
  # create lists of the key variables required in the calculations
  # nTotal - total number of units in each stage
  # nSamp - total number of samples in each stage
  # idList - the unique identifier for each unit in each stage
  # idPrev - a unique identifier for the units in the stage above at each stage (except stage 1)
  for (i in 1:nStage) {
    dat <- RDBobj[[stages[[i]]]]
    nTotal[[i]] <- dat[,paste(stages[[i]],"numTotal",sep="")]
    nSamp[[i]] <- dat[,paste(stages[[i]],"numSamp",sep="")]
    idList[[i]] <- dat[,paste(stages[[i]],"id",sep="")]
    if (i>1) {
      idPrev[[i]] <- dat[,paste(stages[[i-1]],"id",sep="")]
    } # end if
  } # end for 

  # calculate terms in the variance at each stage
  # this needs to be done from the lowest heirarhcy upwards
  # meanStage is the mean of of the y variable at each stage
  # nTotStage & nSampStage are the numbers of units in each stage
  # ssqStage is the sum of squares term in the variance estimate
  # tStage is the estimate of the "population" total of the y variable at each stage
  for (i in nStage:1) {
    if (i==1) {
      # for the first stage (which is calculated last) we use sum not tapply
      meanStage[[i]] <- mean(y[[i]])
      ymean[[i]] <- rep(meanStage[[i]],length(idList[[i]]))
      nTotStage[[i]] <- rep(mean(nTotal[[i]]),length(idList[[i]]))
      nSampStage[[i]] <- rep(mean(nSamp[[i]]),length(idList[[i]]))
      ssqStage[[i]] <- sum(((y[[i]]-ymean[[i]])^2)/(nSamp[[i]]-1))
      tStage[[i]] <- sum(nTotal[[i]]/nSamp[[i]]*y[[i]])
    } else {
      # as there are several units of upper heirarchices in each stage, we use tapply
      meanStage[[i]] <- tapply(y[[i]],idPrev[[i]],mean)
      ymean[[i]] <- meanStage[[i]][match(idPrev[[i]],names(meanStage[[i]]))] 
      nTotStage[[i]] <- tapply(nTotal[[i]],idPrev[[i]],mean)
      nSampStage[[i]] <- tapply(nSamp[[i]],idPrev[[i]],mean)
      ssqStage[[i]] <- tapply(((y[[i]]-ymean[[i]])^2)/(nSamp[[i]]-1),idPrev[[i]],sum)
      tStage[[i]] <- tapply(nTotal[[i]]/nSamp[[i]]*y[[i]],idPrev[[i]],sum)
    }
    # add in cases where variance is zero because whole population sampled at that stage
    ssqStage[[i]][is.infinite(ssqStage[[i]]) & nTotStage[[i]]==nSampStage[[i]]] <- 0
    ssqTerm[[i]] <- nTotStage[[i]]^2*(1-nSampStage[[i]]/nTotStage[[i]])/nSampStage[[i]]*ssqStage[[i]]
    if (i>1) y[[i-1]] <- tStage[[i]][match(idList[[i-1]],names(tStage[[i]]))]
  } #end for
  estTot <- tStage 

  # now calculate the sum of each term sequentially from the lowest hierarchy
  # to the first heirarchy
  vv <- estVarTot <- list()
  vv[[nStage]] <- 0 
  for (i in nStage:2) {
    idPrevVec <- idList[[i-1]][match(names(ssqTerm[[i]]),idList[[i-1]])]
    estVarTot[[i]] <- tapply(ssqTerm[[i]]+vv[[i]],idPrevVec,sum)
    vv[[i-1]] <- nTotStage[[i]]/nSampStage[[i]]*estVarTot[[i]]
  } 

  # output the estimate of the population total, and the associated variance for that estimate
  output <- list(estTot=estTot,estVarTot=estVarTot) 
  return(output)
}
