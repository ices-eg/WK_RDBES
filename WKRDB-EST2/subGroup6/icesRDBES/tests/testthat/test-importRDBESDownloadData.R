# common parameters
ddir <- "./h1_v_1_19/"
expObj  <- readRDS(paste0(ddir, "h1_rdbesRawObject.rds"))


test_that("importing zipped H1 example data works", {
  zipFiles <- c("H1_2021_000_example.zip",
             "HSL_2021_002_example.zip",
             "HVD_2021_001_example.zip")

  genObj <- importRDBESDownloadData(paste0(ddir, zipFiles))
  expect_equal(genObj, expObj)
})

test_that("importing some data that is not zipped H1 example data works", {
  zipFiles <- c("H1_2021_000_example.zip",
                "HSL_2021_002_example.zip",
                "VesselDetails.csv")

  genObj <- importRDBESDownloadData(paste0(ddir, zipFiles))
  expect_equal(genObj, expObj)
})

test_that("importing subset H1 example data works", {
  zipFiles <- c("HSL_2021_002_example.zip",
                "VesselDetails.csv")

  genObj <- importRDBESDownloadData(paste0(ddir, zipFiles))
  expect_equal(genObj$VD, expObj$VD)
  expect_equal(genObj$SS, NULL)
  expect_equal(genObj$SL, expObj$SL)
})

test_that("Overwriting a tabel from a zip file produces a warning", {
  zipFiles <- c("HVD_2021_001_example.zip",
                "VesselDetails.csv")

  expect_warning(importRDBESDownloadData(paste0(ddir, zipFiles)),
                 "Overwriting file: VesselDetails.csv, this might be intended!")
})
