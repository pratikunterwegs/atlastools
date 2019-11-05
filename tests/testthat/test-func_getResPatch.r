context("get residence patches")
testthat::test_that("patches are calculated correctly", {

  # read in data
  revdata = data.table::fread("../testdata/recurse435_008.csv")
  htdata = data.table::fread("../testdata/435_008.csv")

  # run function for patch inference
  inference_output <- watlasUtils::funcInferResidence(revdata = revdata,
                                                      htdata = htdata,
                                                      resTimeLimit = 2,
                                                      travelSeg = 5,
                                                      infPatchTimeDiff = 1800,
                                                      infPatchSpatDiff = 100)


  # run function for classification
  classified_output <- watlasUtils::funcClassifyPath(somedata = inference_output)

  # run function for patch construction
  testoutput <- watlasUtils::funcGetResPatch(somedata = classified_output,
                                             bufferSize = 10,
                                             spatIndepLim = 50,
                                             tempIndepLim = 3600,
                                             makeSf = "FALSE")

  # make sf output
  testoutput_sf <- watlasUtils::funcGetResPatch(somedata = classified_output,
                                                makeSf = "TRUE")

  # do tests
  # test that the output class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))

  # test that names are present in output cols
  expnames <- c("id", "tidalcycle", "type", "patch", "time_mean",
                "tidaltime_mean", "x_mean", "y_mean", "duration", "distInPatch",
                "distBwPatch", "propfixes")
  for(i in 1:length(expnames)){
    testthat::expect_true(expnames[i] %in% colnames(testoutput),
                          info = glue::glue('{expnames[i]} expected in output but not produced'))
  }

  # test that the sf output class is at least sf
  testthat::expect_s3_class(object = testoutput_sf, class = c("sf"))

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time_mean)), na.rm = TRUE), 0)
})

testthat::test_that("residence patch construction works on artificial data", {
  # read in data and segment it
  testrevdata = data.table::fread("../testdata/test_revdata.csv")
  testhtdata = data.table::fread("../testdata/test_htdata.csv")
  # run function for patch inference
  inference_output <- watlasUtils::funcInferResidence(revdata = testrevdata,
                                                      htdata = testhtdata,
                                                      resTimeLimit = 2,
                                                      travelSeg = 5,
                                                      infPatchTimeDiff = 1800,
                                                      infPatchSpatDiff = 100)
  # run function for classification
  classified_output <- watlasUtils::funcClassifyPath(somedata = inference_output)

  # run function for patch construction
  testoutput <- watlasUtils::funcGetResPatch(somedata = classified_output,
                                             bufferSize = 10,
                                             spatIndepLim = 50,
                                             tempIndepLim = 3600,
                                             makeSf = "FALSE")

  # make sf output
  testoutput_sf <- watlasUtils::funcGetResPatch(somedata = classified_output,
                                                makeSf = "TRUE")
  # test that element one is a data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.frame", "tbl"))
  # test that element two is an sf *POLYGON
  testthat::expect_s3_class(object = testoutput_sf, class = c("sf"))
  # test that names are present in output cols
  expnames <- c("id", "tidalcycle", "type", "patch", "time_mean",
                "tidaltime_mean", "x_mean", "y_mean", "duration", "distInPatch",
                "distBwPatch", "propfixes")
  for(i in 1:length(expnames)){
    testthat::expect_true(expnames[i] %in% colnames(testoutput),
                          info = glue::glue('{expnames[i]} expected in output but not produced'))
  }

  # do tests - WORKING TESTS
  # test that there are 2 patches
  testthat::expect_equivalent(max(testoutput$patch), 12)
  # there may need to be a better test

})

# ends here
