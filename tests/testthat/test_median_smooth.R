context("clean raw data\n")
testthat::test_that("cleaning raw data works", {

  # make test_data
  starttime <- Sys.time()
  attr(starttime, "tzone") <- "CET"
  starttime_num <- as.numeric(Sys.time()) * 1e3 # get numeric in milliseconds
  message(glue::glue("starttime = {starttime} and \\
                     starttime num = {starttime_num}"))

  test_data <- data.table::data.table(
    X = cumsum(runif(
      n = 1e3,
      min = 0, max = 1
    )),
    Y = cumsum(runif(
      n = 1e3,
      min = 0, max = 1
    )),
    TIME = seq(starttime_num,
      (starttime_num + 1e6),
      length.out = 1000
    ),
    NBS = round(runif(1e3, min = 1, max = 5)),
    TAG = "31001000435",
    SD = 50,
    VARX = 0,
    VARY = 0,
    COVXY = 0
  )

  # run function
  test_output <- atlastools::atl_median_smooth(
    data = test_data,
    moving_window = 3
  )

  # test on real data
  real_data <- data.table::fread("../testdata/whole_season_tx_435.csv")
  test_output_real <- atl_median_smooth(
    data = real_data,
    moving_window = 5
  )

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(
    object = test_output,
    class = c("data.table", "data.frame")
  )
  testthat::expect_s3_class(
    object = test_output_real,
    class = c("data.table", "data.frame")
  )

  # check that no rows are removed
  testthat::expect_equal(nrow(test_data), nrow(test_output))
})
