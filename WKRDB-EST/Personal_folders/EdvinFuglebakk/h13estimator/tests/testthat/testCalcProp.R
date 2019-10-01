data <- parseRDBESexchange("../../inst/testresources/herringlottery_trimmed_H13.csv")
context("run simple example")
prop <- calculateBVProportions(data$BV, "Age", stratified = F)

context("check proportions sum to 1")
propsum <- aggregate(list(proportion=prop$proportion), by=list(SAid=prop$SAid), FUN=sum)
expect_true(all(propsum$proportions==1))
