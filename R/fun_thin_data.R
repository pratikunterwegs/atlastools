#' Thin tracking data by resampling or aggregation.
#'
#' @param data Cleaned data to aggregate. Must have a numeric column named time.
#' @param interval The interval in seconds over which to aggregate.
#' @param id_columns Column names for grouping columns.
#' @param method Should the data be thinned by subsampling or aggregation.
#' If resampling, the first position of each group is taken.
#' If aggregation, the group positions' median is taken.
#'
#' @return A dataframe aggregated taking the mean over the interval.
#' @export
#'
atl_thin_data <- function(data,
                          interval = 60,
                          id_columns = NULL,
                          method = c("resample",
                                     "aggregate")) {

  id <- time <- SD <- VARX <- VARY <- COVXY <- NULL

  # check input type
  assertthat::assert_that("data.frame" %in% class(data),
                          msg = "thin_data: input not a dataframe object!")

  # check that type is a character and within scope
  assertthat::assert_that(method %in% c("resample",
                                      "aggregate"),
                          msg = "thin_data: type must be 'resample' or \\
                          'aggregate'")

  # id input is not a data.table set it so
  if (!data.table::is.data.table(data)) {
    setDT(data)
  }

  # include asserts checking for required columns
  atl_check_data(data,
                 names_expected = c("x", "y", "time", id_columns))

  # check aggregation interval is greater than min time difference
  assertthat::assert_that(interval > min(diff(data$time)),
      msg = "thin_data: thinning interval less than tracking interval!")

  # round interval and reassign, because otherwise the original data is
  # not retained as is, instead also being modified
  data_copy <- data.table::copy(data)
  data_copy[, time := floor(time / interval) * interval]

  # handle method option
  if (method == "aggregate") {
    # aggregate over tracking interval
    data_copy <- data_copy[, lapply(.SD, stats::median, na.rm = TRUE), 
                 by = c("time", id_columns)]
  } else if (method == "resample") {
    # resample one observation per rounded interval
    data_copy <- data_copy[, lapply(.SD, data.table::first), 
                 by = c("time", id_columns)]
  }

  # check for class and whether there are rows
  assertthat::assert_that("data.frame" %in% class(data_copy),
                  msg = "filter_bbox: cleaned data is not a dataframe object!")

  # print warning if all rows are removed
  if (nrow(data_copy) == 0) {
    warning("filter_bbox: cleaned data has no rows remaining!")
  }
  
  return(data_copy)
}
