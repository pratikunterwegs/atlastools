
#' Compare reference and processed tracks.
#'
#' @param processed_data The processed movement track.
#' @param reference_data The refernece movement track.
#' @param x The X coordinate.
#' @param y The Y coordinate.
#' @param time The time coordinate.
#'
#' @return The median of the ratio of the displacement between the processed
#' track and the reference track, and the corresponding distance between 
#' consecutive points in the reference track.
#' @export
#' 
atl_compare_tracks <- function(processed_data,
                               reference_data,
                               x = "x",
                               y = "y",
                               time = "time") {
  distance <- track_diff <- x_proc <- x_ref <- y_proc <- y_ref <- NULL
  
  # check data for correct names
  names_required <- c(x, y, time)
  atl_check_data(processed_data, names_expected = names_required)
  atl_check_data(reference_data, names_expected = names_required)
  
  # align data by time
  aligned_data <- data.table::merge.data.table(reference_data, 
                                               processed_data,
                                               by = time,
                                               suffixes = c("_ref", "_proc"))
  
  # get the distance between reference and processed points
  aligned_data[, track_diff := sqrt((x_ref - x_proc) ^ 2
               + (y_ref - y_proc) ^ 2)]
  # get distance between reference points
  aligned_data[, distance := atlastools::atl_simple_dist(aligned_data,
                                                         x = "x_ref",
                                                         y = "y_ref")]
  # get the median of track difference (in m) by distance (also m)
  track_error <- stats::median(aligned_data$track_diff / aligned_data$distance,
                        na.rm = TRUE)
  
  return(track_error)
}
