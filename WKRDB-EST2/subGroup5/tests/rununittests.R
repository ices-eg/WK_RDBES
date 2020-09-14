library(testthat)
for (f in list.files("funs", pattern="*.R")){
  print(file.path("funs", f))
  source(file.path("funs", f))  
}

#
# Checks folder ./tests/unittests for tests to run
# test files must be named test-<something>
# test does not otherwise have to bother with importing anything
# 
# This test setup is desinged some that functions and tests can be copied to apprioriate place in a packages that uses testthat
#

testthat::test_dir("tests/unittests/")