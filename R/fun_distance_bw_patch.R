#' Get the distance between patches.
#' 
#' Gets the linear distance between the first point of patch \code{i} and
#' the last point of the previous patch patch \code{i - 1}.
#' Distance is returned in metres.
#' This function is used internally by other functions, and rarely on its own.
#'
#' @param data A dataframe of or extending the class data.frame, such as a
#' data.table.
#' This must contain two pairs of coordinates, the start and end X and Y
#' coordinates of a feature.
#' @param x1 The first X coordinate or longitude; for inter-patch distances,
#' this is the last coordinate (x_end) of a patch \eqn{i}.
#' @param x2 The second X coordinate; for inter-patch distances, this is the
#' first coordinate (x_start) of a subsequent patch \eqn{i + 1}.
#' @param y1 The first Y coordinate or latitude; for inter-patch distances,
#' this is the last coordinate (y_end) of a patch \eqn{i}.
#' @param y2 The second Y coordinate; for inter-patch distances, this is the
#' first coordinate (y_start) of a subsequent patch \eqn{i + 1}.
#'
#' @return A numeric vector of the length of the number of patches, or rows in
#' the input dataframe.
#' For single patches, returns \code{NA}.
#' The vector has as its elements \code{NA}, followed by 
#' n-1 distances, where n is the number of rows.
#'
#' @examples 
#' # basic usage of atl_patch_dist
#' \dontrun{
#' atl_patch_dist(data = data,
#'                x1 = "x_end", x2 = "x_start",
#'                y1 = "y_end", y2 = "y_start")
#' }
#' 
#' @export
#' 
atl_patch_dist <- function(data,
                           x1 = "x_end", x2 = "x_start",
                           y1 = "y_end", y2 = "y_start") {
  # check for basic assumptions
  assertthat::assert_that(is.data.frame(data),
    is.character(x1),
    is.character(y1),
    msg = "bw_patch_dist: some data assumptions are not met"
  )

  # get distance returning zero if single point or NA by default
  if (nrow(data) > 1) {
    # get x and y
    x1 <- data[[x1]][seq_len(nrow(data) - 1)]
    x2 <- data[[x2]][2:nrow(data)]
    y1 <- data[[y1]][seq_len(nrow(data) - 1)]
    y2 <- data[[y2]][2:nrow(data)]
    # get dist
    dist <- c(NA, sqrt((x1 - x2)^2 + (y1 - y2)^2))
  } else {
    dist <- NA_real_
  }

  return(dist)
}

# end here
