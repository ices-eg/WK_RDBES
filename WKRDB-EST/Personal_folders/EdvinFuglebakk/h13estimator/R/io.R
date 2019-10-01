library(data.table)

#' parse DE line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist
#' @param DEid id this line
#' @noRd
#' @keywords internal
parseDE <- function(linevec, datalist, DEid, verbose=F){

  if (verbose){
    print(paste("Reading DE", DEid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(
    DEid=as.integer(DEid),
    DErecType=as.character(linevec[[1]]),
    DEsampScheme=as.character(linevec[[2]]),
    DEyear=as.integer(linevec[[3]]),
    DEstratum=as.character(linevec[[4]]),
    DEhierarchyCor=as.character(linevec[[5]]),
    DEhierarchy=as.character(linevec[[6]])
  )
  datalist$DE <- rbind(datalist$DE, dt)
  return(datalist)
}

#' parse SD line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist
#' @param SDid id this line
#' @param DEid id of parent DE table
#' @noRd
#' @keywords internal
parseSD <- function(linevec, datalist, SDid, DEid, verbose=F){

  if (verbose){
    print(paste("Reading SD", SDid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(
                   SDid=as.integer(SDid),
                   DEid=as.integer(DEid),
                   SDrecType=as.character(linevec[[1]]),
                   SDctry=as.character(linevec[[2]]),
                   SDinst=as.character(linevec[[3]])
  )
  datalist$SD <- rbind(datalist$SD, dt)
  return(datalist)
}

#' parse FO line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist
#' @param FOid id this line
#' @param FTid id of any parent FT table
#' @param SDid id of any parent SD table
#' @noRd
#' @keywords internal
parseFO <- function(linevec, datalist, FOid, FTid=NA, SDid=NA, verbose=F){

  if (verbose){
    print(paste("Reading FO", FOid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(FOid=as.integer(FOid),
                   FTid=as.integer(FTid),
                   SDid=as.integer(SDid),
                   FOrecType=as.character(linevec[[1]]),
                   FOstratification=as.character(linevec[[2]]),
                   FOhaulNum=as.integer(linevec[[3]]),
                   FOstratum=as.character(linevec[[4]]),
                   FOclustering=as.character(linevec[[5]]),
                   FOclusterName=as.character(linevec[[6]]),
                   FOsampler=as.character(linevec[[7]]),
                   FOaggLev=as.character(linevec[[8]]),
                   FOval=as.character(linevec[[9]]),
                   FOcatReg=as.character(linevec[[10]]),
                   FOstartDate=as.POSIXct(linevec[[11]]),
                   FOstartTime=as.character(linevec[[12]]),
                   FOendDate=as.POSIXct(linevec[[13]]),
                   FOendTime=as.character(linevec[[14]]),
                   FOdur=as.integer(linevec[[15]]),
                   FOstartLat=as.numeric(linevec[[16]]),
                   FOstartLon=as.numeric(linevec[[17]]),
                   FOstopLat=as.numeric(linevec[[18]]),
                   FOstopLon=as.numeric(linevec[[19]]),
                   FOecoZone=as.character(linevec[[20]]),
                   FOarea=as.character(linevec[[21]]),
                   FOstatRect=as.character(linevec[[22]]),
                   FOsubRect=as.character(linevec[[23]]),
                   FOfu=as.character(linevec[[24]]),
                   FOdep=as.integer(linevec[[25]]),
                   FOwaterDep=as.integer(linevec[[26]]),
                   FOnatCat=as.character(linevec[[27]]),
                   FOmetier5=as.character(linevec[[28]]),
                   FOmetier6=as.character(linevec[[29]]),
                   FOgear=as.character(linevec[[30]]),
                   FOmeshSize=as.character(linevec[[31]]),
                   FOselDev=as.character(linevec[[33]]),
                   FOselDevMeshSize=as.character(linevec[[34]]),
                   FOtarget=as.character(linevec[[34]]),
                   FOobsCo=as.character(linevec[[35]]),
                   FOtotal=as.integer(linevec[[36]]),
                   FOsampled=as.integer(linevec[[37]]),
                   FOprob=as.numeric(linevec[[38]]),
                   FOselectMeth=as.character(linevec[[39]]),
                   FOselectMethCluster=as.character(linevec[[40]]),
                   FOtotalClusters=as.integer(linevec[[41]]),
                   FOsampledClusters=as.integer(linevec[[42]]),
                   FOprobCluster=as.integer(linevec[[43]]),
                   FOnoSampReason=as.character(linevec[[44]])
                   )
  datalist$FO <- rbind(datalist$FO, dt)
  return(datalist)
}

#' parse SL line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist
#' @param SLid id this line
#' @param FOid id of any parent FO table
#' @param LEid id of any parent LE table
#' @param FTid id of any parent FT table
#' @param OSid id of any parent OS table
#' @param TEid id of any parent TE table
#' @noRd
#' @keywords internal
parseSL <- function(linevec, datalist, SLid, FOid=NA, LEid=NA, FTid=NA, OSid=NA, TEid=NA, verbose=F){

  if (verbose){
    print(paste("Reading SL", SLid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(SLid=as.integer(SLid),
                   FOid=as.integer(FOid),
                   LEid=as.integer(LEid),
                   FTid=as.integer(FTid),
                   OSid=as.integer(OSid),
                   TEid=as.integer(TEid),
                   SLrecType=as.character(linevec[[1]]),
                   SLlistName=as.character(linevec[[2]]),
                   Slyear=as.integer(linevec[[3]]),
                   SLsppCode=as.character(linevec[[4]]),
                   SLcommSpp=as.character(linevec[[5]]),
                   SLCatchFrac=as.character(linevec[[6]]))
  datalist$SL <- rbind(datalist$SL, dt)
  return(datalist)
}

#' parse SS line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist
#' @param SSid id this line
#' @param SLid id for parent species selection table
#' @param LEid id for any parent LE table
#' @param FOid id for any parent FO table
#' @noRd
#' @keywords internal
parseSS <- function(linevec, datalist, SSid, SLid, LEid=NA, FOid=NA, verbose=F){

  if (verbose){
    print(paste("Reading SS", SSid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(SSid=as.integer(SSid),
                   SLid=as.integer(SLid),
                   LEid=as.integer(LEid),
                   FOid=as.integer(FOid),
                   SSrecType=as.character(linevec[[1]]),
                   SSstratification=as.character(linevec[[2]]),
                   SSobsActTyp=as.character(linevec[[3]]),
                   SScatchCat=as.character(linevec[[4]]),
                   SSobsTyp=as.character(linevec[[5]]),
                   SSstratum=as.character(linevec[[6]]),
                   SSclustering=as.character(linevec[[7]]),
                   SSclusterName=as.character(linevec[[8]]),
                   SSsampler=as.character(linevec[[9]]),
                   SSsppListName=as.character(linevec[[10]]),
                   SStotal=as.integer(linevec[[11]]),
                   SSsampled=as.integer(linevec[[12]]),
                   SSselectMeth=as.character(linevec[[13]]),
                   SSselectMethCluster=as.character(linevec[[14]]),
                   SStotalClusters=as.integer(linevec[[15]]),
                   SSsampledClusters=as.integer(linevec[[16]]),
                   SSprobCluster=as.numeric(linevec[[17]]),
                   SSnoSampReason=as.character(linevec[[18]]))
  datalist$SS <- rbind(datalist$SS, dt)
  return(datalist)
}

#' parse SA line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist
#' @param SAid id this line
#' @param SSid id for parent SS table
#' @param SAparentid id for any parent sample (this line is a subsample)
#' @noRd
#' @keywords internal
parseSA <- function(linevec, datalist, SAid, SSid, SAparentid=NA, verbose=F){

  if (verbose){
    print(paste("Reading SA", SAid))
  }
  linevec[linevec==""]<-NA
  dt <- data.table(SAid=as.integer(SAid),
                   SAparentid=as.integer(SAparentid),
                   SSid=as.integer(SSid),
                   SArecType=as.character(linevec[[1]]),
                   SAnatCode=as.character(linevec[[2]]),
                   SAstratification=as.character(linevec[[3]]),
                   SAstratum=as.character(linevec[[4]]),
                   SAsppCode=as.character(linevec[[5]]),
                   SAcommSpp=as.character(linevec[[6]]),
                   SApres=as.character(linevec[[7]]),
                   SAcatchCat=as.character(linevec[[8]]),
                   SAlandCat=as.character(linevec[[9]]),
                   SAcommCatScl=as.character(linevec[[10]]),
                   SAcommCat=as.character(linevec[[11]]),
                   SAsex=as.character(linevec[[12]]),
                   SAunitType=as.character(linevec[[13]]),
                   SAtotalWtLive=as.integer(linevec[[14]]),
                   SAsampWtLive=as.integer(linevec[[15]]),
                   SAtotal=as.numeric(linevec[[16]]),
                   SAsampled=as.numeric(linevec[[17]]),
                   SAprob=as.numeric(linevec[[18]]),
                   SAselectMeth=as.character(linevec[[19]]),
                   SAlowHierarchy=as.character(linevec[[20]]),
                   SAsampler=as.character(linevec[[21]]),
                   SAnoSampReasonFM=as.character(linevec[[22]]),
                   SAnoSampReasonBV=as.character(linevec[[23]]),
                   SAtotalWtMes=as.integer(linevec[[24]]),
                   SAsampWtMes=as.integer(linevec[[25]]),
                   SAconFacMesLive=as.numeric(linevec[[26]]),
                   SAspecState=as.character(linevec[[27]]))
  datalist$SA <- rbind(datalist$SA, dt)
  return(datalist)
}

#' parse BV line, add result to datalist
#' @param linevec list of elements on line
#' @param datalist
#' @param BVid id for this row
#' @param SAid id for any parent SA row
#' @param FMid id for any parent Fm row
#' @noRd
#' @keywords internal
parseBV <- function(linevec, datalist, BVid, SAid=NA, FMid=NA, verbose=F){

  if (verbose){
    print(paste("Reading BV", BVid))
  }

  linevec[linevec==""]<-NA
  dt <- data.table(BVid=as.character(BVid),
                   SAid=as.integer(SAid),
                   FMid=as.integer(FMid),
                   BVrecType=as.character(linevec[[1]]),
                   BVfishID=as.character(linevec[[2]]),
                   BVstratification=as.character(linevec[[3]]),
                   BVstratum=as.character(linevec[[4]]),
                   BVtype=as.character(linevec[[5]]),
                   BVvalue=as.character(linevec[[6]]),
                   BVunitVal=as.character(linevec[[7]]),
                   BVunitRefList=as.character(linevec[[8]]),
                   BVmethod=as.character(linevec[[9]]),
                   BVMeEq=as.character(linevec[[10]]),
                   BVtotal=as.numeric(linevec[[11]]),
                   BVsampled=as.numeric(linevec[[12]]),
                   BVprob=as.numeric(linevec[[13]]),
                   BVselectMeth=as.character(linevec[[14]]),
                   BVsampler=as.character(linevec[[15]]))
  datalist$BV <- rbind(datalist$BV, dt)
  return(datalist)
}

#' Reads RDBES exhange format
#' @description
#'  Parser is only implemented for the tables needed for hiearachy 13
#' @param filename filename for csv file with RDBES exhcange format v1.17
#' @param verbose logical determining whether progress information should be printed while parsing
#' @return list of data.tables corresponding to the different tables parsed. Tables are identified with their Record Type, and columns are identified with their R Name
#' @export
parseRDBESexchange <- function(filename, verbose=F){

  #populate this with one data frame for each table
  datalist <- list()

  # assigning id based on order in exhcange format
  # keeping track of last assigned id
  lastid <-list()
  lastid$DE <- 0
  lastid$SD <- 0
  lastid$FO <- 0
  lastid$SL <- 0
  lastid$SS <- 0
  lastid$SA <- 0
  lastid$BV <- 0

  f <- file(filename, open="rU")
  lines <- readLines(f)
  close(f)

  for (l in lines){

    linevec <- tstrsplit(paste(l,",",sep=""), ",") #add trailing comma as strsplit discards trailing empty values

    if (linevec[[1]]=="DE"){
      lastid$DE <- lastid$DE+1
      datalist <- parseDE(linevec, datalist, lastid$DE, verbose=verbose)
    }
    else if (linevec[[1]]=="SD"){
      lastid$SD <- lastid$SD+1
      datalist <- parseSD(linevec, datalist, lastid$SD, lastid$DE, verbose=verbose)
    }
    else if (linevec[[1]]=="FO"){
      lastid$FO <- lastid$FO+1
      datalist <- parseFO(linevec, datalist, lastid$FO, SDid = lastid$SD, verbose=verbose)
    }
    else if (linevec[[1]]=="SL"){
      lastid$SL <- lastid$SL+1
      datalist <- parseSL(linevec, datalist, lastid$SL, FOid = lastid$FO)
    }
    else if (linevec[[1]]=="SS"){
      lastid$SS <- lastid$SS+1
      datalist <- parseSS(linevec, datalist, lastid$SS, lastid$SL, FOid=lastid$FO)
    }
    else if (linevec[[1]]=="SA"){
      lastid$SA <- lastid$SA+1
      datalist <- parseSA(linevec, datalist, lastid$SA, lastid$SS, verbose=verbose)
    }
    else if (linevec[[1]]=="BV"){
      lastid$BV <- lastid$BV+1
      datalist <- parseBV(linevec, datalist, lastid$BV, SAid=lastid$SA)
    }
    else{
      stop(paste("Record type", linevec[[1]], "not supported"))
    }
  }

  return(datalist)
}
