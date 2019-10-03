data <- parseRDBESexchange("../../inst/testresources/herringlottery_trimmed_H13.csv")

context("estimateTotalHHVar: run simple example")

est <- calculateBVmeans(data$BV, type = "Weight", stratified = F)
prop <- calculateBVProportions(data$BV, "Age", stratified = F)
caaSA <- estimateSAcaa(assumeSelectionMethod(data$SA,"SYSS", "SRSWR"), data$SS, data$SL, "126417", prop, est, stratified=F)
data$FO <- assumeSelectionMethod(data$FO, "SRSWR", "UPSWR")
caaFO <- estimateFOCatchAtAge(data$FO, data$SS, data$SA, caaSA, stratified = F)
caaTotal <- estimateTotalHH(data$FO, caaFO)
FOvarZero <- assumeFOconstantVar(caaFO, 0, ages=caaTotal$age)

covar <- estimateTotalHHVar(data$FO, caaTotal, caaFO, FOvarZero)
expect_equal(sum(diag(covar)==0),sum(caaTotal$numberAtAge==0))


# add some correctness test
