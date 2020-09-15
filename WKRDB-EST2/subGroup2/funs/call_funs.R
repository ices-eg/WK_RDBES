


load("Q:/dfad/users/kibi/data/WK_RDBES/Kirsten/H1/H1_upper.RDATA")

# Add inclusion probabilities if missing




# Script - people nost subset on their own before running 

dat_sub <- subset()

test <-
  generic_su_object_upper_hie(input_list = H1_upper, hierachy = 1)


# test_prob <-
#   fun_add_probs(input_list = test, type_of_probs = "inclusion")

# test_prob_1 <-
#   fun_add_probs(input_list = test_prob, type_of_probs = "selection")


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
