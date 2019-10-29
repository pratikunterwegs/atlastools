context("residencePatch function")
testthat::test_that("residence patch construction works on real data", {

  # read in data names
  revdata = data.table::fread("../testdata/recurse435_008.csv")
  htdata = data.table::fread("../testdata/435_008.csv")
  # run segmentation function
  segoutput <- funcSegPath(revdata = revdata,
                                        htdata = htdata,
                                        resTimeLimit = 2, travelSeg = 5,
                                        infPatchTimeDiff = 1800, infPatchSpatDiff = 100,
                                        inferPatches = TRUE)
  # run patch construction
  testoutput <- funcGetResPatches(df = segoutput,
                                               x = "x",
                                               y = "y", time = "time", buffsize = 10,
                                               returnSf = TRUE)
  # do tests
  # test that the vector class is data.table and data.frame
  testthat::expect_type(object = testoutput, c("list"))
  # test tht element one is a data.frame
  testthat::expect_s3_class(object = testoutput[[1]], class = c("data.frame", "tbl"))
  # test that element two is an sf *POLYGON
  testthat::expect_s3_class(object = testoutput[[2]], class = c("sf"))
  # test that names are present in output cols
  expnames <- c("id", "tidalcycle", "type", "patch", "time_mean",
                "tidaltime_mean", "X_mean", "Y_mean", "duration", "distInPatch",
                "distBwPatch", "propFixes")
  for(i in 1:length(expnames)){
    testthat::expect_true(expnames[i] %in% colnames(testoutput[[1]]),
                          info = glue::glue('{expnames[i]} expected in output but not produced'))
  }

})

testthat::test_that("residence patch construction works on artificial data", {
  # read in data and segment it
  resTimeLimit = 4

  testrevdata = data.table::fread("../testdata/test_revdata.csv")
  testhtdata = data.table::fread("../testdata/test_htdata.csv")


  # run function
  segoutput <- watlasUtils::funcSegPath(revdata = testrevdata,
                                         htdata = testhtdata,
                                         resTimeLimit = resTimeLimit,
                                         travelSeg = 1,
                                         infPatchTimeDiff = 1800, infPatchSpatDiff = 100,
                                         inferPatches = TRUE)

  # run residence patch function
  testoutput <- watlasUtils::funcGetResPatches(df = segoutput,
                                  x = "x",
                                  y = "y", time = "time", buffsize = 10,
                                  returnSf = TRUE)
  # do tests - BASIC TESTS
  testthat::expect_type(object = testoutput, c("list"))
  # test that element one is a data.frame
  testthat::expect_s3_class(object = testoutput[[1]], class = c("data.frame", "tbl"))
  # test that element two is an sf *POLYGON
  testthat::expect_s3_class(object = testoutput[[2]], class = c("sf"))
  # test that names are present in output cols
  expnames <- c("id", "tidalcycle", "type", "patch", "time_mean",
                "tidaltime_mean", "X_mean", "Y_mean", "duration", "distInPatch",
                "distBwPatch", "propFixes")
  for(i in 1:length(expnames)){
    testthat::expect_true(expnames[i] %in% colnames(testoutput[[1]]),
                          info = glue::glue('{expnames[i]} expected in output but not produced'))
  }

  # do tests - WORKING TESTS
  # test that there are 2 patches
  testthat::expect_equivalent(max(testoutput[[1]]$patch), 12)
  # there may need to be a better test

})

# ends here
