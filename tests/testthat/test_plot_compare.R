context("check plot works\n")
testthat::test_that("data can be plotted", {

    # some test data
    test_data <- data.table::fread("../testdata/simulated_data_reflection.csv")

    # expect no error
    pdf(NULL)
    testthat::expect_silent(object = {
        invisible(
            atl_before_after(data = test_data,
                             fun = atlastools::atl_median_smooth,
                             x = "x",
                             y = "y",
                             time = "time",
                             moving_window = 3)
        )
    })
 })