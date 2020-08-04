context("compare reference and processed tracks")
testthat::test_that("compare tracks works", {
  
  # make some test data
  reference_data <- data.table::data.table(x = as.double(1:1e3),
                                     y = as.double(1:1e3),
                                     time = as.numeric(1:1e3))
  
  # process it
  processed_data <- data.table::copy(reference_data)
  processed_data[, `:=`(x = x + stats::runif(1000, -2, 2),
                        y = y + stats::runif(1000, -2, 2))]
  
  test_output <- atlastools::atl_compare_tracks(processed_data,
                                                reference_data)
  
  # expect a single value which is not na
  testthat::expect_type(test_output, "double")
})