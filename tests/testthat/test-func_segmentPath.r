context("segPath function")
testthat::test_that("path segmentation works on real data", {

  # read in data
  revdata = data.table::fread("../testdata/recurse435_008.csv")
  htdata = data.table::fread("../testdata/435_008.csv")

  # run function
  testoutput <- watlasUtils::funcSegPath(revdata = revdata,
                                         htdata = htdata,
                                         resTimeLimit = 2, travelSeg = 5,
                                         infPatchTimeDiff = 1800, infPatchSpatDiff = 100,
                                         inferPatches = TRUE)
  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))
  # test that there is a type column
  testthat::expect_true("type" %in% names(testoutput),
                        info = "patches do not have type tag")
})

testthat::test_that("path segmentation works on artificial data", {

  # make restimelimit
  resTimeLimit = 4

  # create data where there are 5 patches roughly forming a square
  # there are no missing patches
  # the third cluster of points has a restime below the limit
  testrevdata = tibble::tibble(id = 4e3,
    tidalcycle = 1e3,
    x = c(rep(0, 100), rep(0, 100), rep(10, 100),
          rep(10, 100), rep(1, 100)),
    y = c(rep(10, 100), rep(0, 100), rep(0, 100),
          rep(10, 100), rep(11, 100)),
    time = c(seq(1, 1e4, length.out = 500)),
    resTime = c(rep(resTimeLimit, 200), rep(2, 100),
                rep(resTimeLimit, 200)))

  # create ht data
  testhtdata = tibble::tibble(id = 4e3,
    tidalcycle = 1e3,
    x = c(rep(0, 100), rep(0, 100), rep(10, 100),
          rep(10, 100), rep(1, 100)),
    y = c(rep(10, 100), rep(0, 100), rep(0, 100),
          rep(10, 100), rep(11, 100)),
    time = c(seq(1, 1e4, length.out = 500)),
    tidaltime = seq(1, 12.41*60, length.out = 500))

  # run function
  testoutput <- watlasUtils::funcSegPath(revdata = testrevdata,
                                         htdata = testhtdata,
                                         resTimeLimit = resTimeLimit,
                                         travelSeg = 1,
                                         infPatchTimeDiff = 1800, infPatchSpatDiff = 100,
                                         inferPatches = TRUE)
  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_s3_class(object = testoutput, class = c("data.table", "data.frame"))
  # test that there is a type column
  testthat::expect_true("type" %in% names(testoutput),
                        info = "patches do not have type tag")

  # testing function behaviour
  # should not have inferred patches
  testthat::expect_false("inferred" %in% testoutput$type,
                          info = "patches are inferred where none!")

  # should have fewer points than it started with
  testthat::expect_lt(nrow(testoutput), nrow(testrevdata))

  # should be 2 patches -- this is correct from a segmentation point of view
  # should be handlded in the residence patch construction test
  testthat::expect_equal(max(testoutput$resPatch), 2,
                         info = "does not calculate right number of patches")
})
