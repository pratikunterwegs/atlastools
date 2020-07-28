context("filtering data in bounds\n")
testthat::test_that("data kept within bounds", {

  # make test_data
  test_data <- data.table::data.table(X = as.double(seq_len(1000)),
                                      Y = as.double(seq_len(1000)),
                                      TIME = seq_len(1000))

  test_data <- test_data[200:500, `:=`(X = rnorm(301, 300, 20),
                                       Y = rnorm(301, 800, 20))]

  # run function
  test_output <- atlastools::atl_filter_bounds(data = test_data,
                                               x = "X",
                                               y = "Y",
                                               x_range = c(250, 450),
                                               y_range = c(750, 850),
                                               remove_inside = FALSE)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = test_output, class = c("data.table",
                                                           "data.frame"))

  # check that some rows are removed or that none are added
  testthat::expect_gte(nrow(test_data), nrow(test_output))

  # check the correct points are kept
  testthat::expect_true(all(data.table::between(test_output$X, 250, 450)),
                        info = "within bounds not working")

  testthat::expect_true(all(data.table::between(test_output$Y, 750, 850)),
                        info = "within bounds not working")

})

testthat::test_that("data removed within bounds", {

  # make test_data
  test_data <- data.table::data.table(X = as.double(seq_len(1000)),
                                      Y = as.double(seq_len(1000)),
                                      TIME = seq_len(1000))

  test_data <- test_data[200:500, `:=`(X = rnorm(301, 300, 20),
                                       Y = rnorm(301, 800, 20))]

  # run function
  test_output <- atlastools::atl_filter_bounds(data = test_data,
                                               x = "X",
                                               y = "Y",
                                               x_range = c(250, 450),
                                               y_range = c(750, 850),
                                               remove_inside = TRUE)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = test_output, class = c("data.table",
                                                            "data.frame"))

  # check that some rows are removed or that none are added
  testthat::expect_gte(nrow(test_data), nrow(test_output))

  # check the correct points are kept
  testthat::expect_true(all(!data.table::between(test_output$X, 250, 450)),
                        info = "within bounds not working")

  testthat::expect_true(all(!data.table::between(test_output$Y, 750, 850)),
                        info = "within bounds not working")

})
