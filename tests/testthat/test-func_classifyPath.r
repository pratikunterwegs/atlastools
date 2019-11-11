context("classify residence points")
testthat::test_that("residence classification works", {

  # read in data
  revdata = data.table::fread("../testdata/recdata/recurse435_008.csv")

  # run function
  testoutput <- watlasUtils::funcClassifyPath(somedata = revdata)

  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time)), na.rm = TRUE), 0)
})

# end here
