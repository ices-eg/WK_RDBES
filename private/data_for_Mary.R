
library(dplyr)


load("Q:/mynd/RDB/RDBES_dev/H1_upper.Rdata")

# Dataset for R code for calculating the inclusion, selection and jont inclusion

mary <- filter(left_join(left_join(H1_upper$DE, H1_upper$SD), H1_upper$VS), DEstratum == "quarter_1" & VSstratum != "U")

mary_1 <- mutate(select(mary, VSid, SDid, VDid, TEid, VSrecType, VSstratification, VSstratum, VSclustering, VSclusterName, VSsampler, VStotal, VSsampled,
                    VSprob, VSselectMeth, VSselectMethCluster, VStotalClusters, VSsampledClusters, VSprobCluster, VSnoSampReason), no = 1)


mary_2 <- select(mutate(group_by(mary_1, VSstratum), order = cumsum(no), selection_weight = 1), -no)


write.csv(mary_2, "Q:/mynd/RDB/RDBES_dev/wkrdbes_example_data_SRSWR.csv", row.names = F)

mary_srswor <- mutate(mary_2, VSselectMeth = "SRSWOR")

write.csv(mary_srswor, "Q:/mynd/RDB/RDBES_dev/wkrdbes_example_data_SRSWOR.csv", row.names = F)

ups <- left_join(mary_2, H1_upper$FT, by = c("VSid" = "VSid"))

test <- distinct(ungroup(ups), FTtotal, FTsampled)
total <- sum(test$FTtotal)

ups <- mutate(ups, VSselectMeth = "UPSWOR", selection_weight = FTtotal/sum(test$FTtotal), SDid = SDid.x, VDid = VDid.x)

ups_2 <- select(ups, VSid, SDid, VDid, TEid, VSrecType, VSstratification, VSstratum, VSclustering, VSclusterName, VSsampler, VStotal, VSsampled,
                VSprob, VSselectMeth, VSselectMethCluster, VStotalClusters, VSsampledClusters, VSprobCluster, VSnoSampReason,
                order, selection_weight)

write.csv(ups_2, "Q:/mynd/RDB/RDBES_dev/wkrdbes_example_data_UPSWOR.csv", row.names = F)

ups_3 <- mutate(ups_2,  VSselectMeth = "UPSWR")
write.csv(ups_3, "Q:/mynd/RDB/RDBES_dev/wkrdbes_example_data_UPSWR.csv", row.names = F)

# Dataset for R code for bootstrapping the variance
s1 <- filter(ups_2, VSstratum == "Hirtshals - Trawler/Seiner - North Sea")
s2 <- left_join(s1, H1_upper$FT, by = c("VSid" = "VSid"))
s3 <- left_join(s2, H1_upper$FO, by = c("FTid" = "FTid"))
s4 <- left_join(s3, H1_upper$SS)
s5 <- left_join(s4, H1_upper$SA)

write.csv(s5, "Q:/mynd/RDB/RDBES_dev/wkrdbes_example_data_4_stage_sampling.csv", row.names = F)
