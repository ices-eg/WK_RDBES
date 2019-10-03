data <- parseRDBESexchange("../../inst/testresources/herringlottery_trimmed_H13.csv")

context("getAllAgesFO")
est <- calculateBVmeans(data$BV, type = "Weight", stratified = F)
prop <- calculateBVProportions(data$BV, "Age", stratified = F)
caaSA <- estimateSAcaa(assumeSelectionMethod(data$SA,"SYSS", "SRSWR"), data$SS, data$SL, "126417", prop, est, stratified=F)
caaFO <- estimateFOCatchAtAge(data$FO, data$SS, data$SA, caaSA, stratified = F)
caaFO <- merge(caaFO, data$FO[,c("FOid","FOprob")], all.x=T)

allages <- getAllAgesFO(caaFO, ages=as.character(seq(0,max(as.integer(caaFO$age)))))
expect_true(all(allages[allages$age==9,"numberAtAge"]>0))
