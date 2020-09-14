context("check plot works\n")
testthat::test_that("data can be plotted", {

    # some test data
    starttime <- Sys.time()
    attr(starttime, "tzone") <- "CET"
    starttime_num <- as.numeric(Sys.time()) * 1e3 # get numeric in milliseconds
    test_data <- data.table::data.table(X = cumsum(runif(n = 1e3,
                                                      min = 1, max = 10)),
                                     Y = cumsum(runif(n = 1e3,
                                                      min = 1, max = 10)),
                                     TIME = seq(starttime_num,
                                                (starttime_num + 1e6),
                                                length.out = 1000),
                                     NBS = round(runif(1e3, min = 1, max = 5)),
                                     TAG = "31001000435",
                                     SD = 50,
                                     VARX = 0,
                                     VARY = 0,
                                     COVXY = 0)

    # expect no error
    testthat::expect_silent(object = {
        atl_before_after(data = test_data,
                      fun = atlastools::atl_median_smooth,
                      x = "X",
                      y = "Y",
                      time = "TIME",
                      moving_window = 3)
    })
 })