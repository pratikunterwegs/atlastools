context("distance and speed functions")
testthat::test_that("simple distance and speed works", {

  # make test positions
  test_df <- data.table::data.table(a = 1,
                                   b = 1:100,
                                   time = 1:100)
  # run function with custom col names
  test_output <- atlastools::atl_simple_dist(test_df, x = "a", y = "b")

  # get speeds as well
  test_speed <- atlastools::atl_get_speed(test_df, x = "a",
                                           y = "b",
                                           time = "time")

  # do tests
  # should return as many elements as nrows in df
  testthat::expect_equal(length(test_output), nrow(test_df),
                         info = "distances returned are not same length
                                 as data provided")
  testthat::expect_equal(length(test_speed), nrow(test_df),
                         info = "speeds returned are not same length
                                 as data provided")
  # test that the first element is NA
  testthat::expect_equal(test_output[1], NA_real_,
                             info = "first distance is not NA")
  testthat::expect_equal(test_speed[1], NA_real_,
                             info = "first speed is not NA")
  # test that the vector class is numeric or double
  testthat::expect_type(test_output, "double")
  testthat::expect_type(test_speed, "double")

  # test that the distances except first are 1 in this case
  testthat::expect_equal(test_output, c(NA, rep(1.0, 99)),
                             info = "the distance calculation is wrong")

})

testthat::test_that("simple distance is correct", {
test_data <- data.table::fread("../testdata/whole_season_tx_435.csv")[1:1000, ]

  # distance using custom fun
  test_distances <- atlastools::atl_simple_dist(test_data,
                                                 x = "X", y = "Y")

  # get a test speed
  test_speed <- atlastools::atl_get_speed(test_data,
                                           x = "X", y = "Y",
                                           time = "TIME")

  # distance using sf
  data_sf <- sf::st_as_sf(test_data,
                          coords = c("X", "Y"))
  sf::st_crs(data_sf) <- 32631

  sf_distance <- sf::st_distance(data_sf$geometry[seq_len(nrow(data_sf) - 1)],
                                 data_sf$geometry[-1], by_element = T)

  sf_distance <- as.numeric(c(NA, sf_distance))

  # check that the distances are correct
  testthat::expect_equal(test_distances, sf_distance)
  # check the speeds are correct
  testthat::expect_equal(sf_distance / c(NA, diff(test_data[["TIME"]])),
                         test_speed)
})
