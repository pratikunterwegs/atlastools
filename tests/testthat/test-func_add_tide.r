context("add tide data\n")
testthat::test_that("adding tide data works", {

  # read in data subset
  testdata <- data.table::fread("../testdata/whole_season_tx_435.csv")[1:1000, ]

  # process with rm attractor, clean data, and add tide
  { testoutput <- wat_clean_data(data = testdata,
                                 moving_window = 5,
                                 nbs_min = 3,
                                 sd_threshold = 5e5)

    testoutput <- wat_add_tide(data = testoutput,
                               tide_data = "../testdata/tidesSummer2018.csv")
  }

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput,
                            class = c("data.table", "data.frame"))

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$TIME)), na.rm = TRUE), 0)

  # check that tide number and time since high tide are added
  # test that names are present in output cols
  expnames <- c("tide_number", "tidaltime")
  # test col names in data access
  purrr::walk(expnames, function(ename) {
    testthat::expect_true(ename %in% colnames(testoutput),
      info = glue::glue('{ename} expected in output but not produced'))
  })
})

# ends here
