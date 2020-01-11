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
  # check input type
  assertthat::assert_that("data.frame" %in% class(df),
                          msg = "rmAttractor: input not a dataframe object!")

  # include asserts checking for required columns
  {
    dfnames <- colnames(df)
    namesReq <- c("x", "y", "time")
    for (i in 1:length(namesReq)) {
      assertthat::assert_that(namesReq[i] %in% dfnames,
                              msg = glue::glue('rmAttractor: {namesReq[i]} is
                         required but missing from data!'))
    }

    # check aggregation interval is greater than min time difference
    assertthat::assert_that(interval > min(diff(df$time)),
      msg = "aggData: aggregation interval less than tracking interval!")
  }

  # aggregate over tracking interval
  {
    df[,time:= round(time/interval) * interval]
    df <- df[,lapply(.SD, mean, na.rm=TRUE), by = .(time, id)]
  }

  # check output class of ts
  {
    assertthat::assert_that(is.numeric(df$ts) == FALSE,
      msg = "aggData: ts is no longer POSIXct, instead numeric")
  }

  return(df)
}
