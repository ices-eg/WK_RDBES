## Code for WKRDB-EST 2019
A prototype for a design based estimator for hierarchy 13 with unequal probability sampling of hauls is implemented in the package h13estimator.

The example data set was obtained with data conversion code at: https://github.com/edvinf/wkrdb-est-dataconversion

Generally, generated files are not included in the repository. To build and test package, run:
devtools::document()
devtools::build()
devtools::test()

To render report, run:
knitr::knit("report/estimation_report.Rmd")