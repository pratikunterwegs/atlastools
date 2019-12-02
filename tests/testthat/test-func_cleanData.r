context("clean raw data")
testthat::test_that("cleaning raw data works", {

  # make testdata
  starttime <- Sys.time()
  attr(starttime, "tzone") <- "CET"
  starttime_num <- as.numeric(Sys.time())*1000

  testdata <- data.table::data.table(X = cumsum(runif(n = 1e3, min=0, max=1)),
                                     Y = runif(n = 1e3, min=0, max=1),
                                     TIME = starttime_num:(starttime_num+999),
                                     NBS = round(runif(1e3, min=1, max = 5)),
                                     TAG = "31001435",
                                     SD = 2e4,
                                     VARX = 0,
                                     VARY = 0,
                                     COVXY = 0)

  # make sure first NBS is greater than min for time check
  testdata[1,]$NBS = 5e3

  # run function
  testoutput <- watlasUtils::funcCleanData(somedata = testdata,
                                           moving_window=5,
                                           nbs_min=3,
                                           sd_threshold=5e5,
                                           plot=FALSE)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$TIME)), na.rm = TRUE), 0)

  # check that low nbs rows are gone
  testthat::expect_gt(min(testoutput$NBS), 2)

  # check that some rows are removed
  testthat::expect_gt(nrow(testdata), nrow(testoutput))

  # check that time is correctly handled
  testthat::expect_identical(testoutput[1,]$ts, starttime)
})
