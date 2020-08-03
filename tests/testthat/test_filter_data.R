context("check data filtering\n")
testthat::test_that("data is correctly filtered", {
  
  # make some test data
  test_data <- data.table::data.table(x = as.double(1:1e3),
                                     y = as.double(1:1e3),
                                     time = as.numeric(1:1e3),
                                     cov_1 = runif(1000, 1, 100))
  # test output here
  test_output <- atlastools::atl_filter_covariates(test_data,
                    "data.table::between(cov_1, 25, 75)")
  
  # check for min and max
  testthat::expect_gte(min(test_output$cov_1), 25)
  testthat::expect_lte(min(test_output$cov_1), 75)
  
  # check warning
  testthat::expect_warning(atlastools::atl_filter_covariates(test_data,
                        "data.table::between(cov_1, 0, 0.5)"),
                        regexp = "*no rows remaining*")
})