context("clean raw data\n")
testthat::test_that("cleaning raw data works", {

  # make testdata
  starttime <- Sys.time()
  attr(starttime, "tzone") <- "CET"
  starttime_num <- as.numeric(Sys.time()) * 1e3 # get numeric in milliseconds
  message(glue::glue("starttime = {starttime} and \\
                     starttime num = {starttime_num}"))

  testdata <- data.table::data.table(X = cumsum(runif(n = 1e3,
                                                      min = 0, max = 1)),
                                     Y = cumsum(runif(n = 1e3,
                                                      min = 0, max = 1)),
                                     TIME = seq(starttime_num,
                                                (starttime_num + 1e6),
                                                length.out = 1000),
                                     NBS = round(runif(1e3, min = 1, max = 5)),
                                     TAG = "31001000435",
                                     SD = 50,
                                     VARX = 0,
                                     VARY = 0,
                                     COVXY = 0)

  # make sure first NBS is greater than min for time check
  testdata[1, ]$NBS <- 5e3

  # run function
  testoutput <- atl_clean_data(data = testdata,
                               moving_window = 3,
                               nbs_min = 3,
                               sd_threshold = 5e5,
                               filter_speed = TRUE,
                               speed_cutoff = 150)

  # test on real data
  real_data <- data.table::fread("../testdata/whole_season_tx_435.csv")
  testoutput_real <- atl_clean_data(data = real_data,
                                    moving_window = 5,
                                    nbs_min = 3,
                                    sd_threshold = 100)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput,
                            class = c("data.table", "data.frame"))
  testthat::expect_s3_class(object = testoutput_real,
                            class = c("data.table", "data.frame"))

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time)), na.rm = TRUE), 0)

  # check that low nbs rows are gone
  testthat::expect_gt(min(testoutput$NBS), 2)

  # check that some rows are removed
  testthat::expect_gt(nrow(testdata), nrow(testoutput))

  # check that time is correctly handled
  testthat::expect_lte(starttime, testoutput[1, ]$ts)
  message(glue::glue("cleandata starttime = {testoutput[1,]$ts}"))
})
