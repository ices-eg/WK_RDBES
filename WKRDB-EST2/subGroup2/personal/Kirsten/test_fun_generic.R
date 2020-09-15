

load("Q:/dfad/users/kibi/data/WK_RDBES/Kirsten/H1/H1_upper.RDATA")

H1_upper$VS$VSprob <- 1-(1-1/H1_upper$VS$VStotal)^H1_upper$VS$VSsampled

H1_upper$FT$FTprob <- 1-(1-1/H1_upper$FT$FTtotal)^H1_upper$FT$FTsampled

H1_upper$FO$FOprob <- 1-(1-1/H1_upper$FO$FOtotal)^H1_upper$FO$FOsampled



test <-
  generic_su_object_upper_hie(input_list = H1_upper, hierachy = 1)


