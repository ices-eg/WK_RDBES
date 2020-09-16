


load("Q:/dfad/users/kibi/data/WK_RDBES/Kirsten/H1/H1_upper.RDATA")

# Add inclusion probabilities if missing

H1_upper$VS$VSprob <- 1-(1-1/H1_upper$VS$VStotal)^H1_upper$VS$VSsampled

H1_upper$FT$FTprob <- 1-(1-1/H1_upper$FT$FTtotal)^H1_upper$FT$FTsampled

H1_upper$FO$FOprob <- 1-(1-1/H1_upper$FO$FOtotal)^H1_upper$FO$FOsampled

source("./WKRDB-EST2/subGroup2/funs/fun_generic_su_object_upper_hie.R")

test_old <-
  generic_su_object_upper_hie(input_list = H1_upper, hierachy = 1)

source("./WKRDB-EST2/subGroup2/personal/Kirsten/fun_generic_su_object_upper_hie.R")

test_new <-
  generic_su_object_upper_hie(input_list = H1_upper, hierachy = 1)
# I want to estimate total number of vessels



total_vessel_strata <-
  data.frame(
    stratum = test$su1$designTable$stratum,
    probs = test$su1$comb_select_probs
  )




# Script - people nost subset on their own before running 

dat_sub <- subset()

test <-
  generic_su_object_upper_hie(input_list = H1_upper, hierachy = 1)


# test_prob <-
#   fun_add_probs(input_list = test, type_of_probs = "inclusion")

# test_prob_1 <-
#   fun_add_probs(input_list = test_prob, type_of_probs = "selection")

test_prob <- function(input_list = test, type_of_probs = "inclusion")

total_vessel_strata <-
  data.frame(
    stratum = test_prob_1$su1$designTable$stratum,
    probs = test_prob_1$su1$comb_select_probs
  )
summarise(group_by(total_vessel_strata, stratum),
          total = sum(1 / probs) / length(probs))
s# I think it makes more sense to have the inc and selection in the orginal data frame and then the inc_probs should be the combined
# inclusion for that unit e.g for su2 it is the su1_inc_probs * su2_inc_probs t$


su1)

unique(test$su1$selectMeth)

test$su2$selectMeth <- "SRSWR"

test_nuno <- generate_probs(x=test$su2, type="inclusion")


unique(H1_upper$VS)
