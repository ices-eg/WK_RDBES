context("doDBEestimationObjLow")

estimationObjectNumber <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHiearchy = "A", stat="number")
estimationObjectCountAtAge6 <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHiearchy = "A", stat="numberAtAge6")
estimationObjectCountAtAge <- doDBEestimationObjLowSpecimenParams(FMsa, BVsa, lowerHiearchy = "A", stat="numberAtAge")

expect_equal(ncol(estimationObjectCountAtAge$StatisticTable),21)
expect_equal(ncol(estimationObjectCountAtAge$DesignTable),8)
expect_equal(ncol(estimationObjectNumber$StatisticTable),2)
expect_equal(ncol(estimationObjectNumber$DesignTable),8)
