context("filtering data in bounds\n")
testthat::test_that("data kept within bounds", {

  # make test_data
  test_data <- data.table::data.table(X = as.double(seq_len(1000)),
                                      Y = as.double(seq_len(1000)),
                                      TIME = seq_len(1000))
  
  test_data <- test_data[200:500, `:=`(X = rnorm(301, 300, 20),
                                       Y = rnorm(301, 800, 20))]
  # test polygon
  test_polygon <- sf::st_point(x = c(500, 500))
  test_polygon <- sf::st_buffer(test_polygon, dist = 500)
  test_area <- sf::st_sf(data.frame(feature = 1, 
                                    geom = sf::st_sfc(test_polygon)))
  sf::st_crs(test_area) <- 32631 # the WATLAS system CRS
  
  # run function
  test_output <- atlastools::atl_filter_bounds(data = test_data,
                                               x = "X",
                                               y = "Y",
                                               x_range = c(200, 500),
                                               y_range = c(700, 850),
                                               sf_polygon = test_area,
                                               remove_inside = FALSE)
  
  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = test_output, class = c("data.table",
                                                           "data.frame"))

  # check that some rows are removed or that none are added
  testthat::expect_gte(nrow(test_data), nrow(test_output))

  # check the correct points are kept
  testthat::expect_true(all(data.table::between(test_output$X, 200, 500)),
                        info = "within bounds not working")

  testthat::expect_true(all(data.table::between(test_output$Y, 700, 850)),
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
                                               x_range = c(200, 500),
                                               y_range = c(700, 900),
                                               remove_inside = TRUE)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = test_output, class = c("data.table",
                                                            "data.frame"))

  # check that some rows are removed or that none are added
  testthat::expect_gte(nrow(test_data), nrow(test_output))

  # check the correct points are kept
  testthat::expect_true(all(!(data.table::between(test_output$X, 200, 500) &
                              data.table::between(test_output$Y, 700, 900))),
                        info = "within bounds not working")

})
