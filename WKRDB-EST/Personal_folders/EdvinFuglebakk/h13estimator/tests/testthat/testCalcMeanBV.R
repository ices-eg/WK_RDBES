data <- parseRDBESexchange("../../inst/testresources/herringlottery_trimmed_H13.csv")
context("calculateBVmeans: run simple example")
est <- calculateBVmeans(data$BV, type = "Weight", stratified = F)
expect_equal(nrow(est), 3)

context("calculateBVmeans: check against alt calc")
altcalc <- mean(as.numeric(data$BV[data$BV$SAid==2 & data$BV$BVtype=="Weight",][["BVvalue"]]))
expect_equal(est[est$SAid==2,][["mean"]], altcalc)
