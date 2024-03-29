#' Thin tracking data by resampling or aggregation.
#'
#' Uniformly reduce data volumes with either aggregation or resampling
#' (specified by the \code{method} argument) over an interval specified in
#' seconds using the \code{interval} argument.
#' Both options make two important assumptions:
#' (1) that timestamps are named \code{time}, and
#' (2) all columns except the identity columns can be averaged in \code{R}.
#' While the \code{subsample} option returns a thinned dataset with all columns 
#' from the input data, the \code{aggregate} option drops the column 
#' \code{COVXY}, since this cannot be propagated to the averaged position.
#' Both options handle the column \code{time} differently: while \code{subsample} 
#' returns the actual timestamp (in UNIX time) of each sample, \code{aggregate} 
#' returns the mean timestamp (also in UNIX time).
#' In both cases, an extra column, \code{time_agg}, is added which has a uniform
#'  difference between each element corresponding to the user-defined thinning
#' interval.
#' The \code{aggregate} option only recognises errors named \code{VARX} and
#' \code{VARY}, and standard deviation around each position named \code{SD}.
#' If all of these columns are not present together the function assumes there
#' is no measure of error, and drops those columns.
#' If there is actually no measure of error, the function simply returns the
#' averaged position and covariates in each time interval.
#' Grouping variables' names (such as animal identity) may be passed as a
#' character vector to the \code{id_columns} argument.
#'
#' @param data Cleaned data to aggregate. Must have a numeric column named time.
#' @param interval The interval in seconds over which to aggregate.
#' @param time The timestamp column name, ideally referring to a column with
#' an integer type.
#' @param id_columns Column names for grouping columns.
#' @param method Should the data be thinned by subsampling or aggregation.
#' If resampling (\code{method = "subsample"}), the first position of each group
#' is taken. If aggregation (\code{method = "aggregate"}), the group positions'
#' mean is taken.
#'
#' @return A dataframe aggregated taking the mean over the interval.
#' @import data.table
#' @examples
#' \dontrun{
#' thinned_data <- atl_thin_data(data,
#'   interval = 60,
#'   id_columns = c("animal_id"),
#'   method = "aggregate"
#' )
#' }
#'
#' @export
#'
atl_thin_data <- function(data,
                          interval = 60,
                          time = "time",
                          id_columns = NULL,
                          method = c(
                            "subsample",
                            "aggregate"
                          )) {
  SD <- VARX <- VARY <- COVXY <- NULL
  time_agg <- NULL

  # check input type
  assertthat::assert_that("data.frame" %in% class(data),
    msg = "thin_data: input not a dataframe object!"
  )

  # check that type is a character and within scope
  assertthat::assert_that(method %in% c(
    "subsample",
    "aggregate"
  ),
  msg = "thin_data: type must be 'subsample' or \\
                          'aggregate'"
  )

  # id input is not a data.table set it so
  if (!data.table::is.data.table(data)) {
    setDT(data)
  }

  # include asserts checking for required columns
  atl_check_data(data,
    names_expected = c(time, id_columns)
  )

  if(!(all(c("x", "y") %in% colnames(data)))) {
    warning(
      "atl_thin_data: could not find columns named 'x' and 'y'
      make sure coordinate columns exist in this data."
    )
  }

  # check aggregation interval is greater than min time difference
  assertthat::assert_that(interval > min(diff(data[[time]])),
    msg = "thin_data: thinning interval less than tracking interval!"
  )

  # round interval and reassign, this modifies by reference!
  data[, time_agg := floor(as.numeric((time)) / interval) * interval]

  # handle method option
  if (method == "aggregate") {
    if (all(c("VARX", "VARY", "SD") %in% colnames(data))) {
      # aggregate over tracking interval
      # the variance of an average is the sum of variances / sample size square
      data <- data[, c(lapply(.SD, mean, na.rm = TRUE),
        VARX_agg = sum(VARX, na.rm = TRUE) / (length(VARX)^2),
        VARY_agg = sum(VARY, na.rm = TRUE) / (length(VARY)^2),

        # variance of an average is sum of variances sum(SD ^ 2)
        # divided by sample size squared length(SD) ^ 2
        # the standard deviation is the square root of the variance

        SD = sqrt(sum(SD^2, na.rm = TRUE) / (length(SD)^2)),
        count = (.N)
      ),
      by = c("time_agg", id_columns)
      ]
    } else {
      # aggregate over tracking interval
      data <- data[, c(lapply(.SD, mean, na.rm = TRUE),
        count = (.N)
      ),
      by = c("time_agg", id_columns)
      ]
    }

    # remove error columns
    data <- data[, setdiff(
      colnames(data),
      c("VARX", "VARY", "COVXY")
    ),
    with = FALSE
    ]

    # reset names to propagated error
    data.table::setnames(data,
      old = c("VARX_agg", "VARY_agg"),
      new = c("VARX", "VARY"),
      skip_absent = TRUE
    )
  } else if (method == "subsample") {
    # subsample the first observation per rounded interval
    data <- data[, c(lapply(.SD, data.table::first),
      count = (.N)
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
    msg = "thin_data: thinned data is not a dataframe object!"
  )

  return(data)
}
