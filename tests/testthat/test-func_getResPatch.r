context("residence patches and classified points")
testthat::test_that("patch calc on empirical data", {

  # read in data
  revdata = data.table::fread("../testdata/recdata/recurse435_009.csv")
  htdata = data.table::fread("../testdata/htdata/435_009.csv")

  # run function for patch inference
  inference_output <- watlasUtils::funcInferResidence(revdata = revdata,
                                                      htdata = htdata,
                                                      infResTime = 2,
                                                      infPatchTimeDiff = 30,
                                                      infPatchSpatDiff = 100)


  # run function for classification
  classified_output <- watlasUtils::funcClassifyPath(somedata = inference_output)

  # run function for patch construction
  testoutput <- watlasUtils::funcGetResPatch(somedata = classified_output,
                                             bufferSize = 10,
                                             spatIndepLim = 100,
                                             tempIndepLim = 30,
                                             restIndepLim = 10,
                                             minFixes = 3)

  # do tests
  # test that the sf output class is at least sf
  testthat::expect_s3_class(object = testoutput, class = c("sf", "data.frame"))

  # test that names are present in output cols
  expnames <- c("id", "tidalcycle", "type", "patch", "time_mean",
                "tidaltime_mean", "x_mean", "y_mean", "duration", "distInPatch",
                "distBwPatch", "propfixes", "polygons")
  for(i in 1:length(expnames)){
    testthat::expect_true(expnames[i] %in% colnames(testoutput),
                          info = glue::glue('{expnames[i]} expected in output but not produced'))
  }

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time_mean)), na.rm = TRUE), 0)
})

testthat::test_that("patch data access function works", {

  # read in data
  revdata = data.table::fread("../testdata/recdata/recurse435_008.csv")
  htdata = data.table::fread("../testdata/htdata/435_008.csv")

  # run function for patch inference
  inference_output <- watlasUtils::funcInferResidence(revdata = revdata,
                                                      htdata = htdata,
                                                      infResTime = 2,
                                                      infPatchTimeDiff = 30,
                                                      infPatchSpatDiff = 100)


  # run function for classification
  classified_output <- watlasUtils::funcClassifyPath(somedata = inference_output)

  # run function for patch construction
  testoutput <- watlasUtils::funcGetResPatch(somedata = classified_output,
                                             bufferSize = 10,
                                             spatIndepLim = 50,
                                             tempIndepLim = 30)

  # access testoutput spatial
  data_access_sf <- watlasUtils::funcGetPatchData(resPatchData = testoutput,
                                                  dataColumn = "data",
                                                  whichData = "spatial")

  # access testoutput spatial
  data_access_pt <- watlasUtils::funcGetPatchData(resPatchData = testoutput,
                                                  dataColumn = "data",
                                                  whichData = "points")

  # test class pts
  testthat::expect_s3_class(object = data_access_pt, class = c("data.frame", "tbl"))
  # test class sf
  testthat::expect_s3_class(object = data_access_sf, class = c("sf"))
  # test that names are present in output cols
  expnames <- c("id", "tidalcycle", "type", "patch", "time_mean",
                "tidaltime_mean", "x_mean", "y_mean", "duration", "distInPatch",
                "distBwPatch", "propfixes", "polygons")
  # test col names in data access
  for(i in 1:length(expnames)){
    testthat::expect_true(expnames[i] %in% colnames(data_access_sf),
                          info = glue::glue('{expnames[i]} expected in output but not produced'))
  }

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time_mean)), na.rm = TRUE), 0)

})

testthat::test_that("residence patch construction works on artificial data", {
  # read in data and segment it
  testrevdata = data.table::fread("../testdata/test_revdata.csv")
  testrevdata$resTime = 500
  testhtdata = data.table::fread("../testdata/test_htdata.csv")
  # run function for patch inference
  inference_output <- watlasUtils::funcInferResidence(revdata = testrevdata,
                                                      htdata = testhtdata,
                                                      infResTime = 2,
                                                      infPatchTimeDiff = 30,
                                                      infPatchSpatDiff = 100)
  # run function for classification
  classified_output <- watlasUtils::funcClassifyPath(somedata = inference_output)

  # run function for patch construction
  testoutput <- watlasUtils::funcGetResPatch(somedata = classified_output,
                                             bufferSize = 10,
                                             spatIndepLim = 50,
                                             tempIndepLim = 30)
  # test that element one is a data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.frame", "tbl", "sf"))
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
  # testthat::expect_equivalent(max(testoutput$patch), 12)
  # there may need to be a better test

})

# ends here
