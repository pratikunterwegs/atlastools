context("get residence patches")
testthat::test_that("patches are calculated", {

  # read in data
  revdata = data.table::fread("../testdata/recurse435_008.csv")
  htdata = data.table::fread("../testdata/435_008.csv")

  # run function for patch inference
  inference_output <- funcInferResidence(revdata = revdata,
                                   htdata = htdata,
                                   resTimeLimit = 2,
                                   travelSeg = 5,
                                   infPatchTimeDiff = 1800,
                                   infPatchSpatDiff = 100)


  # run function for classification
  classified_output <- watlasUtils::funcClassifyPath(somedata = inference_output)

  # run function for patch construction
  testoutput <- watlasUtils::funcGetPatch

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time)), na.rm = TRUE), 0)
})

# end here
