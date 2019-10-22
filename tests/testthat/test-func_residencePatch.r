context("residencePatch function")
testthat::test_that("residence patch construction works", {

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
