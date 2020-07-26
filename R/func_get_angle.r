#' Get the turning angle between points.
#'
#' @param data A dataframe or similar which must have the columns
#' specified by \code{x}, \code{y}, and \code{time}.
#' @param x The x coordinate.
#' @param y The y coordinate.
#' @param time The timestamp in seconds since the UNIX epoch.
#' @return A vector of turning angles in degrees.
#' Negative degrees indicate 'left' turns. There are two fewer
#' angles than the number of rows in the dataframe.
#' @export
wat_turning_angle <- function(data,
                          x = "x",
                          y = "y",
                          time = "time") {

  # check for column names
  wat_check_data(data,
                 names_expected = c(x, y, time))

  # set order in time
  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }
  data.table::setorderv(data, time)

  # handle good data case
  if (nrow(data) > 1) {
    x1 <- data[[x]][seq_len(nrow(data) - 2)]
    x2 <- data[[x]][2:(nrow(data) - 1)]
    x3 <- data[[x]][3:nrow(data)]

    y1 <- data[[y]][seq_len(nrow(data) - 2)]
    y2 <- data[[y]][2:(nrow(data) - 1)]
    y3 <- data[[y]][3:nrow(data)]

    # get three sides of a triangle of (x1,y1), (x2,y2), (x3,y3)
    dist_x1_x2 <- sqrt(((x2 - x1) ^ 2) + ((y2 - y1) ^ 2))
    dist_x2_x3 <- sqrt(((x3 - x2) ^ 2) + ((y3 - y2) ^ 2))
    dist_x3_x1 <- sqrt(((x3 - x1) ^ 2) + ((y3 - y1) ^ 2))

    # use the law of cosines
    angle <- acos(((dist_x1_x2 ^ 2) +
                     (dist_x2_x3 ^ 2) -
                     (dist_x3_x1 ^ 2)) /
                    (2 * dist_x1_x2 * dist_x2_x3))

    # convert to degrees
    angle <- angle * 180 / pi

    # subtract from 180 to get the external angle
    angle <- 180 - (angle)

    # add NA to maintain length
    angle <- c(NA_real_, angle, NA_real_)

  } else if (nrow(data) == 1) {
    angle <- NA_real_
  }
  return(angle)
}
