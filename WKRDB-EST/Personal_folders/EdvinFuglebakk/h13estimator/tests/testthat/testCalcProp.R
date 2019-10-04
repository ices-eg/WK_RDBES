
data <- parseRDBESexchange(system.file("testresources","herringlottery_trimmed_H13.csv", package="h13estimator"))
context("calculateBVProportions: run simple example")
prop <- calculateBVProportions(data$BV, "Age", stratified = F)

context("calculateBVProportions: check proportions sum to 1")
propsum <- aggregate(list(proportion=prop$proportion), by=list(SAid=prop$SAid), FUN=sum)
expect_true(all(propsum$proportions==1))

context("calculateBVProportions: check proportionStrata present")
expect_true(!any(is.na(prop$proportionStrata)))
