context("overall distance function")
testthat::test_that("simple distance works", {

  # make test positions
  testdf <- tibble::tibble(x = 1, y = 1:100)
  # run function
  testoutput <- watlasUtils::funcDistance(testdf)
  # do tests
  # should return as many elements as nrows in df
  testthat::expect_equal(length(testoutput), nrow(testdf),
                         info = "distances returned are not same length as data provided")
  # test that the first element is NA
  testthat::expect_identical(testoutput[1], as.double(NA),
                             info = "first distance is not NA")
  # test that the vector class is numeric or double
  testthat::expect_type(testoutput, "double")
  # test that the distances except first are 1 in this case
  testthat::expect_identical(testoutput, c(NA, rep(1.0, 99)),
                             info = "the distance calculation is wrong")

})
