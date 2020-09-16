context("computeDBEresultsTotalPointLow")

#extract estimation objects
estimationObjectNumber <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHiearchy = "A", stat="number")
estimationObjectCountAtAge6 <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHiearchy = "A", stat="numberAtAge6")
estimationObjectCountAtAge <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHiearchy = "A", stat="numberAtAge")

#estimation
resultNat6 <- computeDBEresultsTotalPointLowSingleSample(estimationObjectCountAtAge6)
resultN <- computeDBEresultsTotalPointLowSingleSample(estimationObjectNumber)

#mutlivariate estimation
resultNatAge <- computeDBEresultsTotalPointLowSingleSample(estimationObjectCountAtAge)

expect_equal(resultNat6$totals, resultNatAge$totals[[6]])
expect_equal(resultN$totals, sum(resultNatAge$totals))