context("residence patches and classified points")
testthat::test_that("patch calc on empirical data", {

  # read in data
  somedata = data.table::fread("../testdata/435_025_revisit.csv")

  # run function for patch inference
  inference_output <- watlastools::wat_infer_residence(somedata,
                                                      infPatchTimeDiff = 30,
                                                      infPatchSpatDiff = 100)


  # run function for classification
  classified_output <- watlastools::wat_classify_points(somedata = inference_output)

  # run function for patch construction
  testoutput <- watlastools::wat_make_res_patch(somedata = classified_output,
                                             bufferSize = 10,
                                             spatIndepLim = 100,
                                             tempIndepLim = 30,
                                             restIndepLim = 10,
                                             minFixes = 3)

  # do tests
  # test that the sf output class is at least sf
  testthat::expect_s3_class(object = testoutput, class = c("sf", "data.frame", "data.table"))

  # test that names are present in output cols
  expnames <- c("id", "tide_number", "type", "patch", "time_mean",
                "tidaltime_mean", "x_mean", "y_mean", "duration", "distInPatch",
                "distBwPatch",  "dispInPatch")
  purrr::walk(expnames, function(expname){
    testthat::expect_true(expname %in% colnames(testoutput),
                          info = glue::glue('{expname} expected in output but not produced'))
  })

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time_mean)), na.rm = TRUE), 0)
})

testthat::test_that("patch data access function works", {

  # read in data
  somedata = data.table::fread("../testdata/435_025_revisit.csv")

  # run function for patch inference
  inference_output <- watlastools::wat_infer_residence(df = somedata,
                                                      infPatchTimeDiff = 30,
                                                      infPatchSpatDiff = 100)


  # run function for classification
  classified_output <- watlastools::wat_classify_points(somedata = inference_output)

  # run function for patch construction
  testoutput <- watlastools::wat_make_res_patch(somedata = classified_output,
                                             bufferSize = 10,
                                             spatIndepLim = 50,
                                             tempIndepLim = 30)

  # access testoutput summary
  copy1 <- copy2 <- copy3 <- testoutput
  data_access_summary <- watlastools::wat_get_patch_summary(resPatchData = copy1,
                                                      whichData = "summary")

  # access testoutput spatial
  data_access_sf <- watlastools::wat_get_patch_summary(resPatchData = copy2,
                                                  whichData = "spatial")

  # access testoutput spatial
  data_access_pt <- watlastools::wat_get_patch_summary(resPatchData = copy3,
                                                  whichData = "points")

  # test class summary
  testthat::expect_s3_class(object = data_access_summary, class = c("data.frame", "tbl"))
  # test class pts
  testthat::expect_s3_class(object = data_access_pt, class = c("data.frame", "tbl"))
  # test class sf
  testthat::expect_s3_class(object = data_access_sf, class = c("sf"))

  # test that names are present in output cols
  expnames <- c("id", "tide_number", "type", "patch", "time_mean",
                "tidaltime_mean", "x_mean", "y_mean", "duration", "distInPatch", "waterlevel_mean",
                "distBwPatch", "dispInPatch")
  # test col names in data access
  purrr::walk(expnames, function(expname){
    testthat::expect_true(expname %in% colnames(data_access_sf),
                          info = glue::glue('{expname} expected in output but not produced'))
  })

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time_mean)), na.rm = TRUE), 0)

})

# ends here
