context("residence patches and classified points")
testthat::test_that("patch calc on empirical data", {

  # read in data
  test_data <- data.table::fread("../testdata/435_025_revisit.csv")

  # run function for patch construction
  test_output <- atlastools::atl_res_patch(
    data = test_data,
    buffer_radius = 10,
    lim_spat_indep = 100,
    lim_time_indep = 30,
    min_fixes = 3,
    summary_variables = "waterlevel",
    summary_functions = c("mean", "sd")
  )

  # do tests
  # test that the sf output class is at least sf
  testthat::expect_s3_class(
    object = test_output,
    class = c("sf", "data.frame", "data.table")
  )

  # test that names are present in output cols
  expected_names <- c(
    "id", "patch", "time_median",
    "x_median", "y_median", "duration", "dist_in_patch",
    "dist_bw_patch", "disp_in_patch",
    "waterlevel_mean", "waterlevel_sd"
  )
  atlastools:::atl_check_data(test_output, names_expected = expected_names)

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(test_output$time_median)),
    na.rm = TRUE
  ), 0)
})

testthat::test_that("patch data access function works", {

  # read in data
  test_data <- data.table::fread("../testdata/435_025_revisit.csv")

  # run function for patch construction
  test_output <- atlastools::atl_res_patch(
    data = test_data,
    buffer_radius = 10,
    lim_spat_indep = 50,
    lim_time_indep = 30
  )

  # access test_output summary
  data_access_smry <- atlastools::atl_patch_summary(
    patch_data = test_output,
    which_data = "summary"
  )

  # access test_output spatial
  data_access_sf <- atlastools::atl_patch_summary(
    patch_data = test_output,
    which_data = "spatial"
  )

  # access test_output spatial
  data_access_pt <- atlastools::atl_patch_summary(
    patch_data = test_output,
    which_data = "points"
  )

  # test class summary
  testthat::expect_s3_class(
    object = data_access_smry,
    class = c("data.table", "data.frame")
  )
  # test class pts
  testthat::expect_s3_class(
    object = data_access_pt,
    class = c("data.table", "data.frame")
  )
  # test class sf
  testthat::expect_s3_class(object = data_access_sf, class = c("sf"))

  # test that names are present in output cols
  expected_names <- c(
    "id", "patch", "time_median",
    "x_median", "y_median", "duration", "dist_in_patch",
    "dist_bw_patch", "disp_in_patch"
  )
  # test col names in data access
  atlastools:::atl_check_data(test_output, names_expected = expected_names)

  # check that data are ordered in time
  testthat::expect_gt(min(as.numeric(diff(test_output$time_median)),
    na.rm = TRUE
  ), 0)
})

# ends here
