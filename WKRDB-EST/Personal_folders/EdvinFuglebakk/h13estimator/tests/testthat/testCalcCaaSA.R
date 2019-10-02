data <- parseRDBESexchange("../../inst/testresources/herringlottery_trimmed_H13.csv")
context("estimateSAcaa: run simple example")
est <- calculateBVmeans(data$BV, type = "Weight", stratified = F)
prop <- calculateBVProportions(data$BV, "Age", stratified = F)
caa <- estimateSAcaa(assumeSelectionMethod(data$SA,"SYSS", "SRSWR"), data$SS, data$SL, "126417", prop, est, stratified=F)
expect_equal(nrow(caa), 6)
expect_equal(sum(caa$numberAtAge==0),0)

context("estimateSAcaa: zero samples")
datamock <- data
datamock$SA[1,"SAsppCode"]<-"dummy"
caa_w_zeroes <- estimateSAcaa(assumeSelectionMethod(datamock$SA,"SYSS", "SRSWR"), datamock$SS, datamock$SL, "126417", prop, est, stratified=F)

expect_gt(sum(caa_w_zeroes$numberAtAge==0),0)
comp <- merge(caa, caa_w_zeroes, by=c("SAid", "age", "stratum"), suffixes = c("pre","post"))
expect_true(all(comp$numberAtAgepost==comp$numberAtAgepost))
