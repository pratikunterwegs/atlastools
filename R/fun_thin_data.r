#' Thin tracking data.
#'
#' @param data Cleaned data to aggregate. Must have a numeric column named time.
#' @param interval The interval in seconds over which to aggregate.
#' @param type Should the data be thinned by subsampling or aggregation.
#' If subsampling, the first position of each group is taken.
#' If aggregation, the group positions' median is taken.
#'
#' @return A dataframe aggregated taking the mean over the interval.
#' @export
#'
atl_thin_data <- function(data,
                         interval = 60,
                         type = c("subsample",
                                  "aggregate")) {

  id <- time <- NULL

  # check input type
  assertthat::assert_that("data.frame" %in% class(data),
                          msg = "atl_thin_data: input not a dataframe object!")

  # check that type is a character and within scope
  assertthat::assert_that(type %in% c("subsample",
                                      "aggregate"),
                          msg = "atl_thin_data: type must be subsample or \\
                          aggregate")

  # id input is not a data.table set it so
  if (!data.table::is.data.table(data)) {
    setDT(data)
  }

  # include asserts checking for required columns
  data_names <- colnames(data)
  atlastools:::atl_check_data(data,
                 names_expected = c("x", "y", "time"))

  # check aggregation interval is greater than min time difference
  assertthat::assert_that(interval > min(diff(data$time)),
      msg = "atl_thin_data: aggregation interval less than tracking interval!")


  # aggregate over tracking interval
  data[, time := floor(time / interval) * interval]
  data <- data[, lapply(.SD, median, na.rm = TRUE), by = list(time, id)]

  # THIS IS MATHEMATICALLY WRONG AND NEEDS CHANGING
  # now recalculate the SD as "square root of varx + vary + 2 covxy"
  # or zero, whichever is greater
  data[, SD := dplyr::if_else((VARX + VARY + 2 * COVXY) > 0,
                      sqrt(VARX + VARY + 2 * COVXY), 0)]

  # check output class of ts
  assertthat::assert_that(is.numeric(data$ts) == FALSE,
              msg = "atl_thin_data: ts is no longer POSIXct, instead numeric")

  return(data)
}
