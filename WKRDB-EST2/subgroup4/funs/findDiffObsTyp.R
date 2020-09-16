   findDiffObsTyp<- function(x = SS){
    # Ana Fernandes
    # makes a check on
        # NAs in SS$obsTyp
        # two different observation types in the same Fishing Operation
        for (i in 1:length(unique(x$FOid))) {
        if(sum(is.na(x$SSobsTyp))>0) stop ("cannot proceed: NAs in obsTyp")
        if(length(unique(x$SSobsTyp))>1) stop ("ATTENTION: two different observation types in the same Fishing Operation")
        y=table(x$FOid,x$SSobsTyp)
        }
        print(y)
        }


