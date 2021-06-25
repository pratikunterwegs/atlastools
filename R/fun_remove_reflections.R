#' Remove reflected positions.
#'
#' Remove reflections, or prolonged spikes from a movement track by identifying
#' the bounds and removing positions between them.
#' The important function arguments here are \code{point_angle_cutoff} ($A$),
#' \code{reflection_speed_cutoff} ($S$).
#' If the prolonged spike ends before the last row of data, the true end point
#' is used as the outer bound of the spike.
#' If the prolonged spike does not end within the last row of data, all the
#' data are retained and a message is printed.
#'
#' @author Pratik R. Gupte
#'
#' @param data A dataframe or similar which has previously been cleaned.
#' @param x The name of the X coordinate column.
#' @param y The name of the Y coordinate column.
#' @param time The name of the timestamp column.
#' @param point_angle_cutoff The turning angle (in degrees) above which
#' high instantaneous speeds are considered an anomaly rather than fast transit.
#' @param reflection_speed_cutoff The speed (in m/s) above which an anomaly is
#' detected when combined with a high turning angle.
#'
#' @return A dataframe with reflections removed.
#' @examples
#' \dontrun{
#' filtered_data <- atl_remove_reflections(
#'   data = track_data,
#'   x = "x", y = "y", time = "time",
#'   point_angle_cutoff = A,
#'   reflection_speed_cutoff = S
#' )
#' }
#' @export
atl_remove_reflections <- function(data,
                                   x = "x",
                                   y = "y",
                                   time = "time",
                                   point_angle_cutoff = 45,
                                   reflection_speed_cutoff = 20) {
  speed_in <- speed_out <- angle <- NULL
  # check data
  atl_check_data(data, names_expected = c(x, y, time))

  # set order
  data.table::setorderv(data, time)

  # get speed and angle
  data$speed_in <- atl_get_speed(
    data,
    x = x, y = y, time = time,
    type = "in"
  )
  data$speed_out <- atl_get_speed(
    data,
    x = x, y = y, time = time,
    type = "out"
  )
  data$angle <- atl_turning_angle(data, x = x, y = y, time = time)

  # handle bad angles due to identical positions
  data[, angle := data.table::fifelse((speed_in < 1e-5 | speed_out < 1e-5) & 
    is.na(angle), 0, angle)]

  # remove points that cannot be assessed
  # can't determine whether the last few points are reflections hence remove
  data <- data[
    !is.na(speed_in) & !is.na(speed_out) & !is.na(angle) & !is.nan(angle),
  ]

  # prepare a vector of rows to discard
  vec_discard <- integer()

  # identify the anomaly point
  anchor_point <- which(
    data$speed_in > reflection_speed_cutoff &
      data$angle > point_angle_cutoff
  )[1]

  # message
  message(glue::glue("first anchor at {anchor_point}"))

  while (anchor_point < (nrow(data) - 1)) {

    # find next point with speed out > S
    est_ref_end <- which(data[
      seq(
        anchor_point,
        nrow(data)
      ),
      speed_out
    ] > reflection_speed_cutoff)

    # handle case where there is no end, conservatively keep all data
    if (!any(est_ref_end)) {
      message("the reflection does not appear to end: keeping all points")
      break()
    } else {
      # identify end point
      est_ref_end <- anchor_point + est_ref_end

      # print message
      message(sprintf("reflection ends at %i", est_ref_end))

      # update discard vector
      vec_discard <- c(vec_discard, seq(anchor_point, est_ref_end))

      # set the next anchor relative to the ref end
      next_anchor <- which(
        (data$speed_in[seq(est_ref_end, nrow(data))] > reflection_speed_cutoff) &
          (data$angle[seq(est_ref_end, nrow(data))] > point_angle_cutoff)
      )[1]

      # break if there are no more reflections
      # must check for NA because we access the first element of an empty vec
      if (any(!any(next_anchor), is.na(next_anchor))) {
        message("no more reflections; ending")
        break()
      } else {
        # the absolute next anchor
        anchor_point <- est_ref_end + next_anchor
        # check for errors in order
        assertthat::assert_that(anchor_point > est_ref_end,
          msg = glue::glue("anchor point {anchor_point} is \\
                            before reflection end {reflection_end}")
        )
        # message
        message(glue::glue("next anchor is {anchor_point}"))
      }
    }
  }

  # return the rows to be kept
  vec_keep <- setdiff(seq_len(nrow(data)), vec_discard)
  data <- data[vec_keep, ]
  return(data)
}
