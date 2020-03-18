context("repairing high tide patches")
testthat::test_that("high tide repair works", {

  # read in data
  files_list <- list.files("../testdata/", pattern = "413_", full.names = T)
  data_list <- lapply(files_list[1:10], fread)

  # assume all patches are real
  data_list <- lapply(data_list, function(df){
    df <- wat_infer_residence(df)
    df <- wat_classify_points(somedata = df, resTimeLimit = 2)
    df <- wat_make_res_patch(somedata = df,
                             bufferSize = 10,
                             spatIndepLim = 100,
                             tempIndepLim = 30,
                             restIndepLim = 30,
                             minFixes = 3)
  })

  repaired_data <- wat_repair_ht_patches(patch_data_list = data_list,
                                         spatIndepLim = 100,
                                         tempIndepLim = 30,
                                         bufferSize = 10)

  # do tests
  # test that the sf output class is at least sf
  testthat::expect_s3_class(object = repaired_data, class = c("sf", "data.frame", "data.table"))

  # test that names are present in output cols
  expnames <- c("id", "tide_number", "type", "patch", "time_mean",
                "tidaltime_mean", "x_mean", "y_mean", "duration", "distInPatch",
                "distBwPatch",  "dispInPatch", "polygons")
  for(i in 1:length(expnames)){
    testthat::expect_true(expnames[i] %in% colnames(repaired_data),
                          info = glue::glue('{expnames[i]} expected in output but not produced'))
  }

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(repaired_data$time_mean)), na.rm = TRUE), 0)

  # check that patches across tidal cycles are indpendent
  time_end <- repaired_data$time_end; time_end <- time_end[2:length(time_end)]
  time_start <- repaired_data$time_start; time_start <- time_start[1:length(time_start)-1]

  temp_indep <- c(NA, as.numeric((time_end-time_start)/60)) >= 30
  spat_indep <- wat_bw_patch_dist(repaired_data) >= 100
  rest_indep <- TRUE # for patches separated by residence time

  testthat::expect_true(all(purrr::pmap_int(list(temp_indep, spat_indep, rest_indep),
                                        any)[-1]))

})
