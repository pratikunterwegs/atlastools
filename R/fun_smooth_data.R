#' Filter and smooth data.
#'
#' @param data A dataframe object returned by getData.
#' Must contain the columns "X", "Y", "SD", "NBS", "TAG", "TIME";
#' these are the X coordinate, Y coordinate, standard deviation in measurement,
#' number of ATLAS towers that received the signal, the tag number, and
#' the numeric time, in milliseconds from 1970-01-01.
#' @param moving_window The size of the moving window for the median smooth.
#' Must be an odd number.
#' @param x The X coordinate.
#' @param y The Y coordinate.
#' @param time The timestamp, ideally as an integer.
#' median calculation.
#' @return A datatable class object (extends data.frame) which has the
#' additional columns posID and ts, which is TIME converted to human
#' readable POSIXct format.
#' @export
#'
atl_median_smooth <- function(data,
                              x = "X",
                              y = "Y",
                              time = "TIME",
                              moving_window = 3) {

  # check parameter types and assumptions
  assertthat::assert_that("data.frame" %in% class(data),
                          msg = "cleanData: not a dataframe object!")

  # check the data
  names_req <- c(x, y, time)
  atl_check_data(data, names_req)

  # check args positive
  assertthat::assert_that(min(c(moving_window)) > 1,
                          msg = "cleanData: moving window not > 1")
  assertthat::assert_that(moving_window %% 2 != 0,
                          msg = "moving window must be an odd number")
  
  # convert both to DT if not
  if (data.table::is.data.table(data) != TRUE) {
    data.table::setDT(data)
  }
  
  # set in ascending order of time
  data.table::setorderv(data, time)

  # mutate in place
  data[, c(x, y) := lapply(.SD,
                           function(z) {
                             rev(stats::runmed(rev(stats::runmed(z, moving_window)),
                                           moving_window))}),
       .SDcols = c(x, y)]

  assertthat::assert_that("data.frame" %in% class(data),
              msg = "cleanData: cleanded data is not a dataframe object!")

  if (nrow(data) > 0) {
    return(data)
  } else {
    warning("median_smooth: no data remaining")
    return(NULL)
  }
}

# ends here
