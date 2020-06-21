#' A function to aggregate WATLAS data over an interval.
#'
#' @param data Cleaned data to aggregate. Must have a numeric column named time.
#' @param interval The interval in seconds over which to aggregate.
#'
#' @return A dataframe aggregated taking the mean over the interval.
#' @export
#'
wat_agg_data <- function(data,
                         interval = 60) {

  id <- time <- NULL

  # check input type
  assertthat::assert_that("data.frame" %in% class(data),
                          msg = "wat_agg_data: input not a dataframe object!")

  # id input is not a data.table set it so
  if (!data.table::is.data.table(data)) {
    setDT(data)
  }

  # include asserts checking for required columns
  data_names <- colnames(data)
  names_req <- c("x", "y", "time", "VARX", "VARY", "COVXY")
  purrr::walk(names_req, function(nr) {
    assertthat::assert_that(nr %in% data_names,
                            msg = glue::glue("wat_agg_data: {nr} is
                         required but missing from data!"))
  })

  # check aggregation interval is greater than min time difference
  assertthat::assert_that(interval > min(diff(data$time)),
            msg = "aggData: aggregation interval less than tracking interval!")


  # aggregate over tracking interval
  data[, time := floor(time / interval) * interval]
  data <- data[, lapply(.SD, mean, na.rm = TRUE), by = list(time, id)]

  # now recalculate the SD as "square root of varx + vary + 2 covxy"
  # or zero, whichever is greater
  data[, SD := dplyr::if_else((VARX + VARY + 2 * COVXY) > 0,
                      sqrt(VARX + VARY + 2 * COVXY), 0)]

  # check output class of ts
  assertthat::assert_that(is.numeric(data$ts) == FALSE,
              msg = "aggData: ts is no longer POSIXct, instead numeric")

  return(data)
}
