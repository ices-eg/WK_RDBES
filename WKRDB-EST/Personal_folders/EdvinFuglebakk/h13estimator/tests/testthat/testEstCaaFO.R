
data <- parseRDBESexchange(system.file("testresources","herringlottery_trimmed_H13.csv", package="h13estimator"))
context("estimateFOCatchAtAge: run simple example")
est <- calculateBVmeans(data$BV, type = "Weight", stratified = F)
prop <- calculateBVProportions(data$BV, "Age", stratified = F)
caaSA <- estimateSAcaa(assumeSelectionMethod(data$SA,"SYSS", "SRSWR"), data$SS, data$SL, "126417", prop, est, stratified=F)
caaFO <- estimateFOCatchAtAge(data$FO, data$SS, data$SA, caaSA, stratified = F)
expect_equal(nrow(caaFO), 6)
expect_true(all(!is.na(caaFO$numberAtAge)))
