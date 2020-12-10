# subGroup7: linking the work of subgroups, implementing an estimation example

subGroup chair: tbd

**SG ToRs**
- [ ] Review scripts and functions of subgroups. Work on compatibility between inputs and outputs of the different functions.	
- [ ] draft estimation process for one variable
- [ ] Using example data produces a point estimate for that variable
- [ ] Discuss road-forward in terms of estimating other variables and results to include in final estimation object
- [ ] Document database and estimation issues: https://docs.google.com/document/d/1DKUQF9xAqdgXZoHi5sxG4VCDS1mRL8xu4KRsD8NeH7s/edit?usp=sharing



input files:
LowerHnumberAtAge6.rds contains an example output from subgroup 5. The structure is a bit tentative but it should exemplify what information is available from lower hierarchy estimation and give some concrete data to work with. It is a list of estimates for each SA in testData/output/DBErawObj/DBErawObj_DK_1966_H1.rds. Each estimate is represented as a list with members:
statistic: name of the statistic estimated
unitID: SAid the estimate was made for
totals: the estimated total for the SA
variance: not used, may be removed.

The univariate statistic estimated in the example is the somewhat contrived: "number at age 6". 
