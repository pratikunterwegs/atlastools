#' Calculate instantaenous speed.
#'
#' @param data A dataframe or similar which must have the columns
#' specified by \code{x}, \code{y}, and \code{time}.
#' @param x The x coordinate.
#' @param y The y coordinate.
#' @param time The timestamp in seconds since the UNIX epoch.
#' @param type The type of speed (incoming or outgoing) to return.
#' Incoming speeds are specified by \code{type = "in"}, and outgoing speeds
#' by \code{type = "out"}.
#'
#' @return A vector of numerics representing speed.
#' The first position is assigned a speed of NA.
#' @export
atl_get_speed <- function(data,
                          x = "x",
                          y = "y",
                          time = "time",
                          type = c("in", "out")) {

  atl_check_data(data, names_expected = c(x, y, time))

  # set order in time
  data.table::setorderv(data, time)

  # get distance
  distance <- atlastools::atl_simple_dist(data, x, y)

  # get time
  time <- c(NA, diff(data[[time]]))

  # simple speed
  if (type == "in") {
    speed <- distance / time
  } else if (type == "out") {
    speed <- data.table::shift(distance, type = "lead") / 
              data.table::shift(time, type = "lead")
  }  

  return(speed)

}
