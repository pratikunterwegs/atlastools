#' Calculate euclidean distances between successive points.
#'
#' @param x A column name in a data.frame object that contains the numeric X or
#' longitude coordinate for position data.
#' @param y A column name in a data.frame object that contains the numeric Y or
#' latitude coordinate for position data.
#' @param data A dataframe object of or extending the class data.frame,
#' which must contain two coordinate columns for the X and Y coordinates.
#'
#' @return Returns a vector of distances between consecutive points.
#' @export
#'
atl_simple_dist <- function(data, x = "x", y = "y") {
  # check for basic assumptions
  atl_check_data(data, names_expected = c(x, y))
  if (nrow(data) > 1) {
    x1 <- data[[x]][seq_len(nrow(data) - 1)]
    x2 <- data[[x]][2:nrow(data)]
    y1 <- data[[y]][seq_len(nrow(data) - 1)]
    y2 <- data[[y]][2:nrow(data)]
    dist <- c(NA, sqrt((x1 - x2)^2 + (y1 - y2)^2))
  } else {
    dist <- NA_real_
  }
  return(dist)
}
