context("repairing high tide patches")
testthat::test_that("high tide repair works", {

  # read in data
  files_list <- list.files("tests/testdata/", pattern = "435_", full.names = T)
  data_list <- lapply(files_list, fread)

  # assume all patches are real
  data_list <- lapply(data_list, function(df){
    df[,type:='real']
    df <- wat_classify_points(somedata = df)
    df <- wat_make_res_patch(somedata = df)
  })

  repaired_data <- watlastools::wat_re

  # do tests
  # test that the sf output class is at least sf
  testthat::expect_s3_class(object = testoutput, class = c("sf", "data.frame", "data.table"))

  # test that names are present in output cols
  expnames <- c("id", "tide_number", "type", "patch", "time_mean",
                "tidaltime_mean", "x_mean", "y_mean", "duration", "distInPatch",
                "distBwPatch",  "dispInPatch", "polygons")
  for(i in 1:length(expnames)){
    testthat::expect_true(expnames[i] %in% colnames(testoutput),
                          info = glue::glue('{expnames[i]} expected in output but not produced'))
  }

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(testoutput$time_mean)), na.rm = TRUE), 0)
})
