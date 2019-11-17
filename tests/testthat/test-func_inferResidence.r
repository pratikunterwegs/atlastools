context("infer residence function")
testthat::test_that("residence inference works", {

  # read in data
  revdata = data.table::fread("../testdata/recdata/recurse435_008.csv")
  htdata = data.table::fread("../testdata/htdata/435_008.csv")

  # run function
  testoutput <- funcInferResidence(revdata = revdata,
                                         htdata = htdata,
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
