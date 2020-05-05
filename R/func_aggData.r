#' A function to aggregate WATLAS data over an interval.
#'
#' @param df Cleaned data - the output of the cleanData function - to aggregate.
#' Must have a numeric column named time.
#' @param interval The interval in seconds over which to aggregate.
#'
#' @return A dataframe aggregated taking the mean over the interval.
#' @export
#'
wat_agg_data <- function(df,
                        interval = 60){

  id <- time <- NULL

  # check input type
  assertthat::assert_that("data.frame" %in% class(df),
                          msg = "wat_agg_data: input not a dataframe object!")

  # include asserts checking for required columns
  {
    dfnames <- colnames(df)
    namesReq <- c("x", "y", "time")
    purrr::walk (namesReq, function(nr) {
      assertthat::assert_that(nr %in% dfnames,
                              msg = glue::glue('wat_agg_data: {nr} is
                         required but missing from data!'))
    })

    # check aggregation interval is greater than min time difference
    assertthat::assert_that(interval > min(diff(df$time)),
      msg = "aggData: aggregation interval less than tracking interval!")
  }

  # aggregate over tracking interval
  {
    df[, time := floor(time/interval) * interval]
    df <- df[,lapply(.SD, mean, na.rm=TRUE), by = list(time, id)]
  }

  # check output class of ts
  {
    assertthat::assert_that(is.numeric(df$ts) == FALSE,
      msg = "aggData: ts is no longer POSIXct, instead numeric")
  }

  return(df)
}
