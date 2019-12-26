context("infer residence function")
testthat::test_that("residence inference works", {

  # read in data
  somedata = data.table::fread("../testdata/435_025_revisit.csv")

  # run function
  testoutput <- funcInferResidence(df = somedata,
                                         infResTime = 2,
                                         infPatchTimeDiff = 1800,
                                         infPatchSpatDiff = 100)
  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))
  # test that there is a type column
  testthat::expect_true("type" %in% names(testoutput),
                        info = "data does not have type tag")
})

# end here
