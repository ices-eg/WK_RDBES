GetStock <- function(obj, rect = T, stockref_path = "Q:/mynd/SAS Library/Stock/", stuff = "xxx")
{
#-----------------------------------------------
# function to assign stocks to a data frame with information on species and areas
#
# stock master table is loaded in the library "fishPiCodes". 
#               It comes from the Stock List table from IC (provided by Henrick) 
#               Description and AreaType variables are removed to avoid mistakes with symbols 
#               
# input: obj 
#        it is a dataframe which must have the followings variables:
#          - "sspFAO"  : species FAO code
#          - "area"    : area ices subdivision (27.8.c, 27.6.a.... )
#          - "rect"    : statistical rectangle (it is needed for Nephrops)
#
# output: a new "stock" variable in obj with the corresponding ICES stock
#
#----------------------------------------------------
#Updates
#   _v1_1 - 20180710
#       - Step 4 - Fixed Nephrops stock in area 27.3.a don't match by rectangle, but area
#       - Step 5 - Added Nephrops stock outFU in area 27.4, 27.7 and 27.6.a
#       - Step 5 - Added Ammodytes stocks - these have areas like SA.1, which are defined by rect
#       - Step 5 - Added Crangon crangon stock - this one is not in the ICES list. Found it on a stock list from InterCatch (20170907).
#       - Step 5 - Added Pandalus borealis stocks in 27.4.a. are divided between 27.4.a.e and 27.4.a.w, which are defined by rect

#   _v1_2 - 20200804
#       - Step 1 - removed library fishPiCode (not updated anymore), so the stock reference tables now comes from a specified local path


#-------------------------------------------------------------

  y <- stuffy
  
  
  # 1. read stock mastertable
####
stocks <- read.csv(paste0(stockref_path, "stock.csv"))
names(stocks)[1] <- "stock"
stocks$stock<- as.character(stocks$stock)
stocks$sppFAO<- as.character(stocks$sppFAO)
stocks<-stocks[!duplicated(stocks),]


# 2. Expand stock mastertable to include ices subdivisions
####
for (i in unique(stocks$area)){
  temp<-stocks[stocks$area==i,]
  temparea<- unique(stocks$Areas[substr(stocks$area,1,nchar(i)+1)==paste(i,".", sep="")])
  
  copytemp <- as.data.frame(lapply(temp, rep, length(temparea)))    
  copytemp$area <- unlist(lapply(temparea, rep, dim(temp)[1])) 
  
  stocks <- rbind(stocks, copytemp)
}


# 3. expand stock mastertable to include ANK & MON for ANF stocks, and LBD & MEG for LEZ stocks 
#    ("anf.27.1-2", "anf.27.3a46","lez.27.4a6a","lez.27.6b" ).
####
groupedsp<- data.frame(rbind(cbind(group="ANF", sppFAO=c("ANK", "MON")),
                             cbind(group="LEZ", sppFAO=c("LDB", "MEG"))))

for(i in unique(groupedsp$group)){
  temp<-stocks[stocks$sppFAO==i,]
  tempsp<- unique(groupedsp$sppFAO[groupedsp$group==i])
  copytemp <- as.data.frame(lapply(temp, rep, length(tempsp)))    
  copytemp$sppFAO <- unlist(lapply(tempsp, rep, dim(temp)[1])) 
  stocks <- rbind(stocks, copytemp)
}


stocks$stockid <- paste(stocks$sppFAO, stocks$area, sep="_")


# 4. Create variable stock in our obj data frame
####
#     * Nephrops stocks defined according to statistical rectangle - outside 27.3.a.
#     * All the rest of stocks defined according to area
####
obj_nep <- subset(obj, sppFAO=="NEP" & !(area %in% c("27.3.a","27.3.a.20","27.3.a.21")))
nep_stockid <- paste(obj_nep$sppFAO, obj_nep$rect, sep="_")
obj_nep$stock<- as.character(stocks$stock[match(nep_stockid, stocks$stockid)])

obj_rest <- subset(obj, sppFAO!="NEP" | (sppFAO=="NEP" & area %in% c("27.3.a","27.3.a.20","27.3.a.21")) | is.na(sppFAO) | is.na(area))
rest_stockid <- paste(substr(obj_rest$sppFAO, 1, 3), obj_rest$area, sep="_")
obj_rest$stock<- as.character(stocks$stock[match(rest_stockid, stocks$stockid)])

obj <- rbind(obj_nep, obj_rest)


# 5. Add stock to stock that don't follow the general rules in step 4
####
#     * Nephrops stock outside FU are not defined by rect
#     * Ammodytes areas differs e.g. SA.1. These are defined by rect, but the rects are not included in the file from ICES
#     * Crangon crangon stock - the species is not in the ICES list. Found it on a stock list from InterCatch (20170907).
#     * 
####

#Code for outFU in area 27.4, 27.7 and 27.6.a
obj$stock <- ifelse(obj$sppFAO=="NEP" & is.na(obj$stock) & substr(obj$area,1,4)=="27.4", "nep.27.4outFU",
                    ifelse(obj$sppFAO=="NEP" & is.na(obj$stock) & substr(obj$area,1,6)=="27.6.a", "nep.27.6aoutFU",
                           ifelse(obj$sppFAO=="NEP" & is.na(obj$stock) & substr(obj$area,1,4)=="27.7", "nep.27.7outFU", obj$stock)))


#Code for Ammodytes stocks

if (rect == T) {
obj$stock <- ifelse(obj$sppFAO=="SAN" & obj$rect %in% c("31F0", "31F1",	"31F2", "31F3", "32F0",	"32F1",	"32F2", "32F3",	"33F1",	"33F2",	"33F3", "33F4", "34F0",	"34F1",	"34F2", "34F3", "34F4",	"35F0",	"35F1",	"35F2",	"35F3", "35F4", "35F5",	"36E9",	"36F0",	"36F1",	"36F2",	"36F3",	"36F4",	"36F5", "36F6", "37E9",	"37F0",	"37F1",	"37F2",	"37F3",	"37F4",	"37F5", "37F6", "38F0",	"38F1",	"38F2",	"38F3", "38F4",	"38F5",	"39F0",	 "39F1",	"39F2",	"39F3",	"39F4",	"39F5",	"40F0",	"40F1",	"40F2",	"40F3",	"40F4",	"40F5",	"41F4",	"41F5"), "san.sa.1r",
              ifelse(obj$sppFAO=="SAN" & obj$rect %in% c("35F7", "35F8",	"36F7",	"36F8",	"36F9",	"37F7",	"37F8",	"38F6",	"38F7",	"38F8",	"39F6",	"39F7",	
                            "39F8",	"40F6",	"40F7",	"40F8",	"41F6", "41F7",	"41F8", "42F6", "42F7", "42F8", "43F7", "43F8", "43F9", "44F9", "44G0", "45G0", "45G1", 
                            "46G1"), "san.sa.2r",
               ifelse(obj$sppFAO=="SAN" & obj$rect %in% c("41F1",	"41F2",	"41F3",	"42F1",	"42F2",	"42F3",	"42F4",	"42F5",	"43F1",	"43F2",	"43F3",	"43F4",	
                            "43F5",	"43F6",	"44F1",	"44F2",	"44F3",	"44F4",	"44F5",	"44F6",	"44F7",	"44F8",	"45F1",	"45F2",	"45F3",	"45F4",	"45F5",	"45F6",	"45F7", 
                            "45F8", "45F9", "46F1",	"46F2",	"46F3",	"46F4",	"46F5", "46F9", "46G0" ,"47G0"), "san.sa.3r",
                ifelse(obj$sppFAO=="SAN"  & obj$rect %in% c("38E8",	"38E9",	"39E8",	"39E9",	"40E7",	"40E8",	"40E9",	"41E6",	"41E7",	"41E8",	"41E9",	"41F0",
                            "42E7",	"42E8",	"42E9",	"42F0",	"43E7",	"43E8",	"43E9",	"43F0",	"44E5",	"44E6",	"44E7",	"44E8",	"44E9",	"44F0",	"45E6",	"45E7",	"45E8",	
                            "45E9",	"45F0",	"46E6",	"46E7",	"46E8",	"46E9",	"46F0"), "san.sa.4",
                 ifelse(obj$sppFAO=="SAN"  & obj$rect %in% c("47F1",	"47F2",	"47F3",	"47F4",	"47F5",	"47F6",	"48F1",	"48F2",	"48F3",	"48F4",	"48F5",	"48F6",	
                            "49F1",	"49F2",	"49F3",	"49F4",	"49F5",	"49F6",	"50F1",	"50F2",	"50F3",	"50F4",	"50F5",	"51F1",	"51F2",	"51F3",	"51F4",	"51F5",	"52F1",	"52F2",	
                            "52F3",	"52F4",	"52F5"), "san.sa.5r",
                  ifelse(obj$sppFAO=="SAN" & obj$rect %in% c("41G0",	"41G1",	"41G2",	"42G0",	"42G1",	"42G2",	"43G0",	"43G1",	"43G2",	"44G1"), "san.sa.6", 
                   ifelse(obj$sppFAO=="SAN" & obj$rect %in% c("47E6", "47E7",	"47E8",	"47E9", "47F0",	"48E6", "48E7",	"48E8",	"48E9", "48F0",	"49E6", "49E7",	
                            "49E8",	"49E9",	"49F0", "50E6", "50E7",	"50E8",	"50E9", "50F0",	"51E6", "51E7",	"51E8",	"51E9", "51F0", "52E6", "52E7",	"52E8",	"52E9", "52F0"),
                          "san.sa.7r", obj$stock)))))))

#Code for Crangon crangon stock
obj$stock <- ifelse(obj$sppFAO=="CSH" & is.na(obj$stock) & substr(obj$area,1,4)=="27.4" | obj$sppFAO=="CSH" & is.na(obj$stock) & substr(obj$area,1,6)=="27.7.d", 
                    "csh.27.47d", obj$stock)

#Code for Pandalus borealis
obj$stock <- ifelse(obj$sppFAO=="PRA" & is.na(obj$stock) & obj$area=="27.4.a" & substr(obj$rect,1,2)>43 & substr(obj$rect,1,2)<53 & 
                      paste(match(substr(obj$rect,3,3), LETTERS[1:26]), substr(obj$rect,4,4), sep="")>55 &  
                      paste(match(substr(obj$rect,3,3), LETTERS[1:26]), substr(obj$rect,4,4), sep="")<62, "pra.27.4a",
                    ifelse(obj$sppFAO=="PRA" & is.na(obj$stock) & obj$area=="27.4.a"  & substr(obj$rect,1,2)>43 & substr(obj$rect,1,2)<53 & 
                             paste(match(substr(obj$rect,3,3), LETTERS[1:26]), substr(obj$rect,4,4), sep="")>61 &  
                             paste(match(substr(obj$rect,3,3), LETTERS[1:26]), substr(obj$rect,4,4), sep="")<68, "pra.27.3a4a", obj$stock))

}

# 6. Species & area with no stock defined are identified
####
obj$stock[is.na(obj$stock)] <- "no.defined.stock"


obj
}
