context("segPath function")
testthat::test_that("path segmentation works", {

  # read in data names
  revdata = "../testdata/recurse435_008.csv"
  htdata = "../testdata/435_008.csv"
  # run function
  testoutput <- watlasUtils::funcSegPath(revdata = revdata,
                                         htdata = htdata,
                                         resTimeLimit = 2, travelSeg = 5,
                                         infPatchTimeDiff = 1800, infPatchSpatDiff = 100,
                                         inferPatches = TRUE)
  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))
  # test that there is a type column
  testthat::expect_true("type" %in% names(testoutput),
                        info = "patches do not have type tag")
})
