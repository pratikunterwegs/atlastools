context("classify residence points")
testthat::test_that("residence classification works", {

  # read in data
  revdata = data.table::fread("../testdata/435_025_revisit.csv")

  # run function
  testoutput <- watlastools::wat_classify_points(data = revdata, resTimeLimit = 2)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time)), na.rm = TRUE), 0)
})

# end here
