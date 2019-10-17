context("segPath function overall")
testthat::test_that("path segmentation works", {

  # read in data names
  datafiles <- list.files(path = "tests/testthat/testdata/", full.names = TRUE)
  # run function
  testoutput <- watlasUtils::funcSegPath(revdata = datafiles[2], htdata = datafiles[1],
                                         resTimeLimit = 2, travelSeg = 5,
                                         infPatchTimeDiff = 1800, infPatchSpatDiff = 100,
                                         inferPatches = TRUE)
  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"),
                            info = "output is not of data.frame class")
  # test that there are two inferred patches
  testthat::expect_equal(object = unique(testoutput$infPatch), c(NA, 1, 2),
                         info = "wrong number of patches inferred")
  # test that there are two inferred patches
  testthat::expect_equal(object = max(testoutput$resPatch), 12,
                         info = "wrong number of patches constructed")
  # test that there is a type column
  testthat::expect_true("type" %in% names(testoutput),
                        info = "data does not contain type tag")
})
