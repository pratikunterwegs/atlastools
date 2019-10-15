#' betweenPatchDistance
#'
#' @param df A dataframe of or extending the class data.frame. This must contain two pairs of coordinates, the start and end X and Y coordinates of a feature.
#' @param x1 The first X coordinate or longitude; for inter-patch distances, this is the last coordinate (X_end) of a patch n0.
#' @param x2 The second X coordinate; for inter-patch distances, this is the first coordinate (X_start) of a subsequent patch n1.
#' @param y1 The first Y coordinate or latitude; for inter-patch distances, this is the last coordinate (Y_end) of a patch n0.
#' @param y2 The second Y coordinate; for inter-patch distances, this is the first coordinate (Y_start) of a subsequent patch n1.
#'
#' @return A numeric vector of the length of the number of patches, or rows in the input dataframe. If the input dataframe has only a single row, the vector has a length of one, and its only element is 0. In all other cases, the vector has as its elements NA, followed by n-1 distances, where n is the number of rows.
#' @export
#'
funcBwPatchDist = function(df, x1 = "x_end", x2 = "x_start",
                             y1 = "y_end", y2 = "y_start"){
  #check for basic assumptions
  assertthat::assert_that(is.data.frame(df),
                          is.character(x1),
                          is.character(y1),
                          msg = "some df assumptions are not met")
  # get distance
  dist <- dplyr::case_when(nrow(df) > 1 ~
                             # get x and y
                             {
                               {
                                 x1 <- df[,x1]
                                 x2 <- df[,x2]
                                 x1 <- x1[1:length(x1)-1];
                                 x2 <- x2[2:length(x2)]
                               }
                               {
                                 y1 <- df[,y1]
                                 y2 <- df[,y2]
                                 y1 <- y1[1:length(y1)-1];
                                 y2 <- y2[2:length(y2)]}
                               # get dist
                               c(NA, sqrt((x1 - x2)^2 + (y1 - y2)^2))},
                           nrow(df) == 1 ~ {0.0},
                           TRUE ~ {as.numeric(NA)})

  return(dist)
}

# end here
