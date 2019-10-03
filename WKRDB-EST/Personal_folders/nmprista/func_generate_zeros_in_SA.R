# ===================
# Demonstration of approach to the generation of true zeros and NAs in SA table using SL table of RDBES
# Nuno, Lucia, Henrik, Sven, Josefina, Liz, Karolina @ ICES WKRDB-EST, 10/2019
# ===================

    # to be developed
        # multi-species consultation
        # handling of the generated SAid (suggestion in "1" -> "1a"
        # handling of parentIds and stratification

generate_zeros_in_SA<-function(sppCode, SAtable, sppFrame){
    
    ls1 <- split(SAtable, SAtable$SAid)
    ls2 <- lapply(ls1, function(x) {

        if(sppCode %in% sppFrame$SLsppCode ==FALSE) {
                if(!sppCode %in% x$SAsppCode) { x <- rbind(data.frame(SAid = paste("X",x$SAid[1],sep=""), SAsppCode = sppCode, SAtotalWtLive = NA), x); x} else x
                } else {
                    if(!sppCode %in% x$SAsppCode) { x <- rbind(data.frame(SAid = paste("X", x$SAid[1],sep=""), SAsppCode = sppCode, SAtotalWtLive = 0), x); x} else x
                    }
        x<-x[order(x$SAid, decreasing=T),]
        x
        })

    # shows execution steps
    cat("\n sampled \n")
    print(SAtable)
    # print("list")
    # print(sppFrame)
    cat("\n outcome with zeros \n")   
    print(ls2)   

}

    # Example
    # creates example species frame (according to discussions held on commercial species)
 
     sppFrame <- data.frame(SLlistName = "XYZ", SLyear = "20XX", SLcatchFraction = "Lan", SLcommAphiaID = c(125802, 126436), SLcommName = c("Monkfishes","Cod"), 
          SLsppCode = c(126555, 126555,126436), SLsppName = c("Lophius piscatorius", "Gadus morhua"))
    
    # generates example SA table
    SA<-data.frame(SAid = as.character(c(1, 1, 2, 2)), SAsppCode = c(126554, 126436, 126555, 126554), SAtotalWtLive = c(34, 10,20,30))

    # tests in list
        generate_zeros_in_SA(sppCode = 126554, SAtable = SA, sppFrame = sppFrame)
        generate_zeros_in_SA(sppCode = 126555, SAtable = SA, sppFrame = sppFrame)
        generate_zeros_in_SA(sppCode = 126555, SAtable = SA[c(1:2,4),], sppFrame = sppFrame)

    # tests out of list 
        generate_zeros_in_SA(sppCode = 126556, SAtable = SA, sppFrame = sppFrame)
        # accidentally sampled
        generate_zeros_in_SA(sppCode = 126554, SAtable = SA[c(2:4),], sppFrame = sppFrame)


