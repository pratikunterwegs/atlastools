context("thin cleaned data\n")
testthat::test_that("aggregating cleaned data", {

  interval <- 60
  # make test_data
  test_data <- data.table::data.table(x = as.double(1:1e3),
                                     y = as.double(1:1e3),
                                     time = as.numeric(1:1e3),
                                     ts = as.POSIXct(1:1e3,
                                          origin = "1970-01-01"),
                                     id = as.factor("abc"),
                                     VARX = runif(1000) + 300,
                                     VARY = runif(1000) + 300,
                                     COVXY = runif(1000) + 200)
  # run function
  test_output <- atlastools::atl_thin_data(test_data,
                                           interval = 60,
                                           method = "aggregate")

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = test_output,
                            class = c("data.table", "data.frame"))

  # check that some rows are removed or that none are added
  testthat::expect_gte(nrow(test_data), nrow(test_output))

  # check that the correct interval is achieved
  lag <- diff(test_output$time)
  message(glue::glue("min lag = {min(lag)}"))
  testthat::expect_gte(min(lag), interval)

})

# test for resampling
testthat::test_that("resampling cleaned data", {

  interval <- 60
  # make test_data
  test_data <- data.table::data.table(x = as.double(1:1e3),
                                     y = as.double(1:1e3),
                                     time = as.numeric(1:1e3),
                                     ts = as.POSIXct(1:1e3,
                                          origin = "1970-01-01"),
                                     id = as.factor("abc"),
                                     VARX = runif(1000) + 300,
                                     VARY = runif(1000) + 300,
                                     COVXY = runif(1000) + 200)
  # run function
  test_output <- atlastools::atl_thin_data(test_data,
                                           interval = 60,
                                           method = "resample")

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = test_output,
                            class = c("data.table", "data.frame"))

  # check that some rows are removed or that none are added
  testthat::expect_gte(nrow(test_data), nrow(test_output))

  # check that the correct interval is achieved
  lag <- diff(test_output$time)
  message(glue::glue("min lag = {min(lag)}"))
  testthat::expect_gte(min(lag), interval)
})

# test for other option
testthat::test_that("other options fail", {

  interval <- 60
  # make test_data
  test_data <- data.table::data.table(x = as.double(1:1e3),
                                     y = as.double(1:1e3),
                                     time = as.numeric(1:1e3),
                                     ts = as.POSIXct(1:1e3,
                                          origin = "1970-01-01"),
                                     id = as.factor("abc"),
                                     VARX = runif(1000) + 300,
                                     VARY = runif(1000) + 300,
                                     COVXY = runif(1000) + 200)
  # run function
  testthat::expect_error(atlastools::atl_thin_data(test_data,
                           interval = 60,
                           method = "another_option"))

})
