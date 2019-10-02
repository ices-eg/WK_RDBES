data <- parseRDBESexchange("../../inst/testresources/herringlottery_trimmed_H13.csv")
data$SA[1, "SAselectMeth"] <- "dummy"
context("assumeSelectionMethod: run simple example")
SAafter <- assumeSelectionMethod(data$SA, "SYSS", "SRSWR")

expect_equal(sum(data$SA$SAselectMeth=="SYSS"),2)
expect_equal(sum(data$SA$SAselectMeth=="dummy"),1)
context("assumeSelectionMethod: check that replacement is selective")
expect_equal(sum(SAafter$SAselectMeth=="SYSS"),0)
expect_equal(sum(SAafter$SAselectMeth=="SRSWR"),2)
expect_equal(sum(SAafter$SAselectMeth=="dummy"),1)
