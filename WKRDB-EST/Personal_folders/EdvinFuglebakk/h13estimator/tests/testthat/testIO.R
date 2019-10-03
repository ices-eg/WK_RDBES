context("parseRDBESexchang: read")
data <- parseRDBESexchange("../../inst/testresources/herringlottery_trimmed_H13.csv")
expectedtables <- c("BV", "SA", "SS", "SL", "FO", "SD", "DE")
expect_equal(length(data), length(expectedtables))
expect_true(all(names(data) %in% expectedtables))

context("parseRDBESexchange: keys unique")
testkeys <- function(table, key){
  expect_true(key %in% names(table))
  expect_false(any(is.na(table[[key]])))
  expect_equal(nrow(table), length(unique(table[[key]])))
}
testkeys(data$DE, "DEid")
testkeys(data$SD, "SDid")
testkeys(data$FO, "FOid")
testkeys(data$SS, "SSid")
testkeys(data$SL, "SLid")
testkeys(data$SA, "SAid")
testkeys(data$BV, "BVid")

test_foreign <- function(table, foreignkeys, unusedkeys){
  expect_true(all(c(foreignkeys, unusedkeys) %in% names(table)))
  expect_true(all(is.na(table[,unusedkeys, with=F])))
  expect_false(any(is.na(table[,foreignkeys, with=F])))
}

# these are specific to hierarchy 13, lower hiearchy C without subsampling of SA
context("parseRDBESexchange: foreign keys correct")
test_foreign(data$SD, c("DEid"), c())
test_foreign(data$FO, c("SDid"), c("FTid"))
test_foreign(data$SS, c("FOid", "SLid"), c("LEid"))
test_foreign(data$SL, c("FOid", "SLid"), c("LEid", "FTid", "OSid", "TEid"))
test_foreign(data$SA, c("SSid"), c("SAparentid"))
test_foreign(data$BV, c("SAid"), c("FMid"))

context("parseRDBESexchange: data records")
# try a few, add more when bugs come up
expect_gt(nrow(data$BV[data$BV$BVtype=="Age",]), 0)
expect_gt(nrow(data$BV[data$BV$BVtype=="Length",]), 0)
ages <- data$BV[data$BV$BVtype=="Age",]
expect_true(any(!is.na(ages$BVvalue)))
expect_true(all(data$FO$FOselectMeth=="SRSWR"))

context("parseRDBESexchange: SS SL relation")
expect_true(all(data$SS$SSid == data$SL$SLid ))
