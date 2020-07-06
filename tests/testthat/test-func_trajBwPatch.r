context("trajectory function")
testthat::test_that("trajectories are made", {

  # make test positions
  testdf <- tibble::tibble(a_start = seq(10, 100, 10),
                           a_end = a_start + 2,
                           b_start = 1, b_end = 1)
  # run function
  testoutput <- atlastools::wat_patch_traj(testdf,
    x1 = "a_end", x2 = "a_start", y1 = "b_end", y2 = "b_start")

  # do tests
  # should return as many elements as (nrows-1) in df
  testthat::expect_equal(length(testoutput), nrow(testdf) - 1,
                         info = "wrong number of trajectories calced")

  # test that MULTILINESTRING in there somewhere
  testthat::expect_true("MULTILINESTRING" %in% class(testoutput))

})
