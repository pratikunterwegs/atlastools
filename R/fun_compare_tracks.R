
#' Compare reference and processed tracks.
#'
#' @param processed_data The processed movement track.
#' @param reference_data The refernece movement track.
#' @param x The X coordinate.
#' @param y The Y coordinate.
#' @param time The time coordinate.
#'
#' @return The median of the ratio of the displacement between the processed 
#' track and the reference track, and the corresponding distance between consecutive
#' points in the reference track.
#' @export
#' 
atl_compare_tracks <- function(processed_data,
                               reference_data,
                               x = "x",
                               y = "y",
                               time = "time") {
  
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
  track_error <- median(aligned_data$track_diff / aligned_data$distance,
                        na.rm = TRUE)
  # get the median and sd of the distance at each rounded time point
  # summary_data <- aligned_data[, .(dist_diff_med = stats::median(dist_diff),
  #                  dist_diff_sd = stats::sd(dist_diff)),
  #              by = time]
  
  return(track_error)
}

# ggplot()+
#   geom_path(data = reference_data,
#             aes(x, y), size = 2)+
#   geom_path(data = processed_data,
#             aes(x, y), col = "red")
