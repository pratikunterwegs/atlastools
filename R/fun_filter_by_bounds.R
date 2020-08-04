#' Remove positions within a bounding box.
#'
#' @param data A dataframe or extension which contains X and Y coordinates.
#' @param x The X coordinate column.
#' @param y The Y coordinate column.
#' @param x_range The range of X coordinates.
#' @param y_range The range of Y coordinates.
#' @param remove_inside Whether to remove points from within the range.
#' Setting \code{negate = TRUE} removes positions within the bounding
#' box specified by the X and Y ranges.
#'
#'
#' @return A data frame of tracking locations with attractor points removed.
#' @export
#'
atl_filter_bounds <- function(data,
                            x = "x",
                            y = "y",
                            x_range = c(639470, 639472),
                            y_range = c(5887143, 5887145),
                            remove_inside = TRUE) {
  # check input type
  assertthat::assert_that("data.frame" %in% class(data),
                          msg = "filter_bbox: input not a dataframe object!")
  assertthat::assert_that(is.logical(remove_inside),
                          msg = "filter_bbox: remove inside needs TRUE/FALSE")

  # include asserts checking for required columns
  names_req <- c(x, y)
  atl_check_data(data, names_req)

  # check input length of attractors
  invisible(lapply(list(x_range, y_range), function(f) {
         assertthat::assert_that(length(f) == 2,
                     msg = "filter_bbox: incorrect bound lengths")
  }))

  # convert to data.table
  if (is.data.table(data) != TRUE) {
    data.table::setDT(data)
  }

  # filter for spatial extent either inside or outside
  if (remove_inside) {
    data <- data[!(data.table::between(data[[x]], x_range[1], x_range[2]) & 
                   data.table::between(data[[y]], y_range[1], y_range[2])), ]
  } else {
    data <- data[(data.table::between(data[[x]], x_range[1], x_range[2]) & 
                    data.table::between(data[[y]], y_range[1], y_range[2])), ]
  }

  assertthat::assert_that("data.frame" %in% class(data),
    msg = "filter_bbox: cleaned data is not a dataframe object!")

  # print warning if all rows are removed
  if (nrow(data) == 0) {
    warning("filter_bbox: cleaned data has no rows remaining!")
  }

  return(data)
}

# ends here
