#' Thin tracking data by resampling or aggregation.
#'
#' @param data Cleaned data to aggregate. Must have a numeric column named time.
#' @param interval The interval in seconds over which to aggregate.
#' @param id_columns Column names for grouping columns.
#' @param method Should the data be thinned by subsampling or aggregation.
#' If resampling, the first position of each group is taken.
#' If aggregation, the group positions' mean is taken.
#'
#' @return A dataframe aggregated taking the mean over the interval.
#' @export
#'
atl_thin_data <- function(data,
                          interval = 60,
                          id_columns = NULL,
                          method = c(
                            "resample",
                            "aggregate"
                          )) {
  time <- SD <- VARX <- VARY <- COVXY <- NULL
  x <- y <- time_agg <- NULL

  # check input type
  assertthat::assert_that("data.frame" %in% class(data),
    msg = "thin_data: input not a dataframe object!"
  )

  # check that type is a character and within scope
  assertthat::assert_that(method %in% c(
    "resample",
    "aggregate"
  ),
  msg = "thin_data: type must be 'resample' or \\
                          'aggregate'"
  )

  # id input is not a data.table set it so
  if (!data.table::is.data.table(data)) {
    setDT(data)
  }

  # include asserts checking for required columns
  atl_check_data(data,
    names_expected = c("x", "y", "time", id_columns)
  )

  # check aggregation interval is greater than min time difference
  assertthat::assert_that(interval > min(diff(data$time)),
    msg = "thin_data: thinning interval less than tracking interval!"
  )

  # round interval and reassign, this modifies by reference!
  data[, time_agg := floor(as.numeric(time) / interval) * interval]

  # handle method option
  if (method == "aggregate") {
    if (all(c("VARX", "VARY") %in% colnames(data))) {
      # aggregate over tracking interval
      data <- data[, c(lapply(.SD, mean, na.rm = TRUE),
        VARX_agg = sum(VARX, na.rm = TRUE),
        VARY_agg = sum(VARY, na.rm = TRUE),
        count = length(x)
      ),
      by = c("time_agg", id_columns)
      ]
      # remove old columns for variance
      data[, `:=`(VARX = NULL, VARY = NULL, COVXY = NULL, SD = NULL)]
      # reset names
      data.table::setnames(data,
        old = c("VARX_agg", "VARY_agg"),
        new = c("VARX", "VARY")
      )
    }
  } else if (method == "resample") {
    # resample the first observation per rounded interval
    data <- data[, c(lapply(.SD, data.table::first),
      count = length(x)
    ),
    by = c("time_agg", id_columns)
    ]
  }

  # assert copy time is of interval
  lag <- diff(data$time_agg)
  assertthat::assert_that(min(lag) >= interval,
    msg = "thin_data: diff time < interval asked!"
  )

  # check for class and whether there are rows
  assertthat::assert_that("data.frame" %in% class(data),
    msg = "filter_bbox: cleaned data is not a dataframe object!"
  )

  # print warning if all rows are removed
  if (nrow(data) == 0) {
    warning("filter_bbox: cleaned data has no rows remaining!")
  }

  return(data)
}
