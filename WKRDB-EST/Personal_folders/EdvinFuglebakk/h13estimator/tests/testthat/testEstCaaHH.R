data <- parseRDBESexchange("../../inst/testresources/herringlottery_trimmed_H13.csv")

context("estimateTotalHH: run simple example")
est <- calculateBVmeans(data$BV, type = "Weight", stratified = F)
prop <- calculateBVProportions(data$BV, "Age", stratified = F)
caaSA <- estimateSAcaa(assumeSelectionMethod(data$SA,"SYSS", "SRSWR"), data$SS, data$SL, "126417", prop, est, stratified=F)
data$FO <- assumeSelectionMethod(data$FO, "SRSWR", "UPSWR")
caaFO <- estimateFOCatchAtAge(data$FO, data$SS, data$SA, caaSA, stratified = F)
caaTotal <- estimateTotalHH(data$FO, caaFO)
expect_equal(nrow(caaTotal), max(as.integer(data$BV[data$BV$BVtype=="Age",][["BVvalue"]]))+1)

context("estimateTotalHH: run custom ages")
caaCustom <- estimateTotalHH(data$FO, caaFO, ages=as.character(5:10))
expect_equal(nrow(caaCustom), 6)
