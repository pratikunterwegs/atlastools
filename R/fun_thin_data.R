#' Thin tracking data by resampling or aggregation.
#'
#' @param data Cleaned data to aggregate. Must have a numeric column named time.
#' @param interval The interval in seconds over which to aggregate.
#' @param method Should the data be thinned by subsampling or aggregation.
#' If resampling, the first position of each group is taken.
#' If aggregation, the group positions' median is taken.
#'
#' @return A dataframe aggregated taking the mean over the interval.
#' @export
#'
atl_thin_data <- function(data,
                         interval = 60,
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
                 names_expected = c("x", "y", "time"))

  # check aggregation interval is greater than min time difference
  assertthat::assert_that(interval > min(diff(data$time)),
      msg = "thin_data: thinning interval less than tracking interval!")

  # round interval
  data[, time := floor(time / interval) * interval]

  # handle method option
  if (method == "aggregate") {
    # aggregate over tracking interval
    data <- data[, lapply(.SD, stats::median, na.rm = TRUE), 
                 by = list(time, id)]
    
    # THIS IS MATHEMATICALLY WRONG AND NEEDS CHANGING
    # now recalculate the SD as "square root of varx + vary + 2 covxy"
    # or zero, whichever is greater
    data[, SD := dplyr::if_else((VARX + VARY + 2 * COVXY) > 0,
                                sqrt(VARX + VARY + 2 * COVXY), 0)]
  } else if (method == "resample") {
    # resample one observation per rounded interval
    data <- data[, lapply(.SD, data.table::first), by = list(time, id)]
  }

  # check for class and whether there are rows
  assertthat::assert_that("data.frame" %in% class(data),
                  msg = "filter_bbox: cleaned data is not a dataframe object!")

  # print warning if all rows are removed
  if (nrow(data) == 0) {
    warning("filter_bbox: cleaned data has no rows remaining!")
  }

  return(data)
}
