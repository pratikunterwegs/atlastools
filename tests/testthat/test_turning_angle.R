context("angle functions")
testthat::test_that("angles are calculated", {

  # make test positions
  test_df <- data.table::data.table(
    y = sinpi(seq_len(30) / 10),
    x = seq_len(30) / 30,
    time = 1:30
  )
  # run function with custom col names
  test_output <- atlastools::atl_turning_angle(test_df)

  test_df[, angle := test_output]

  # do tests
  # should return as many elements as nrows in df
  testthat::expect_equal(length(test_output), nrow(test_df),
    info = "angles returned are not same length
                                 as data provided"
  )
  # test that the first element is NA
  testthat::expect_equal(test_output[1], NA_real_,
    info = "first angle is not NA"
  )
  # test that the vector class is numeric or double
  testthat::expect_type(test_output, "double")

  test_df_2 <- data.table::data.table(
    y = seq_len(30),
    x = seq_len(30),
    time = 1:30
  )

  test_output <- atlastools::atl_turning_angle(test_df_2)

  # test for correctness
  # test that the angles except first are 45 in this case
  # the angles are rounded
  testthat::expect_equal(floor(test_output),
    c(NA, rep(0, 28), NA),
    info = "the angle calculation is wrong"
  )

  # check no data case
  test_df <- data.table::data.table(
    y = seq_len(1),
    x = seq_len(1),
    time = 1
  )
  bad_angle <- atlastools::atl_turning_angle(test_df)

  testthat::expect_equal(bad_angle, NA_real_,
    info = "bad data returns angles"
  )
})
