context("aggregate cleaned data\n")
testthat::test_that("aggregated cleaned data", {

  interval <- 60
  # make testdata
  testdata <- data.table::data.table(x = as.double(1:1e3),
                                     y = as.double(1:1e3),
                                     time = as.numeric(1:1e3),
                                     ts = as.POSIXct(1:1e3, origin = "1970-01-01"),
                                     id = as.factor("abc"),
                                     VARX = runif(1000) + 300,
                                     VARY = runif(1000) + 300,
                                     COVXY = runif(1000) + 200)
  # run function
  testoutput <- atlastools::wat_agg_data(testdata, interval = 60)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput,
                            class = c("data.table", "data.frame"))

  # check that some rows are removed or that none are added
  testthat::expect_gte(nrow(testdata), nrow(testoutput))

  # check that the correct interval is achieved
  lag <- diff(testoutput$time)
  message(glue::glue("min lag = {min(lag)}"))
  testthat::expect_gte(min(lag), interval)

  # make more test data for expected points calculation
  testdata <- data.table::data.table(x = 1, y = 1,
                                     time = seq(30, 200, 10),
    ts = as.POSIXct(seq(30, 200, 10), origin = "2018-08-08"),
    id = as.factor("abc"),
    VARX = runif(18) + 30,
    VARY = runif(18) + 30,
    COVXY = runif(18) + 200)
  # how many multiples of the interval?
  n_30 <- sum(testdata$time %% 30 == 0)
  # aggregate data
  testoutput <- wat_agg_data(testdata, interval = 30)

  # test data rows -- must be one
  testthat::expect_equal(nrow(testoutput),
                         n_30);

})
