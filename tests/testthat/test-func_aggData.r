context("aggregate cleaned data\n")
testthat::test_that("aggregated cleaned data", {

  interval = 60
  # make testdata
  testdata <- data.table::data.table(x = as.double(1:1e3),
                                     y = as.double(1:1e3),
                                     time = as.numeric(1:1e3),
                                     ts = as.POSIXct(1:1e3, origin = "1970-01-01"),
                                     id = as.factor("abc"))
  # run function
  testoutput <- watlastools::wat_agg_data(testdata, interval = 60)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))

  # check that some rows are removed or that none are added
  testthat::expect_gte(nrow(testdata), nrow(testoutput))

  # check that the correct interval is achieved
  lag <- diff(testoutput$time)
  message(glue::glue('min lag = {min(lag)}'))
  testthat::expect_gte(min(lag), interval)

})
