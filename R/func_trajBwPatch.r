#' patchTraj
#'
#' @param df A dataframe with at least 2 pairs of coordinate columns, such as X_end Y_end, X_start, Y_start.
#' @param x1 Coordinate column for the beginning X coordinate of the linestring; this should be the last X coordinate of each patch if inter-patch linestrngs are to be constructed. It is possible to use the X_start coordinate, but this will lead to issues and nonsensical output at this point.
#' @param x2 Coordinate column for the end X coordinate of the linestring; this should be the first X coordinate of each patch if inter-patch linestrngs are to be constructed.
#' @param y1 Coordinate column for the beginning Y coordinate of the linestring; this should be the last Y coordinate of each patch if inter-patch linestrngs are to be constructed. It is possible to use the X_start coordinate, but this will lead to issues and nonsensical output at this point.
#' @param y2 Coordinate column for the end Y coordinate of the linestring; this should be the first Y coordinate of each patch if inter-patch linestrngs are to be constructed.
#'
#' @return An \code{sf MULTILINESTRING} object representing the linear path between N patches, starting at the end point of the first patch, and ending at the first point of the Nth patch.
#' @export
#'
funcPatchTraj <- function(df, x1 = "x_end", x2 = "x_start",
                          y1 = "y_end", y2 = "y_start"){
  # must assert df has correct columns

  # convert df to data_frame
  df <- sf::st_drop_geometry(df)
  data.table::setDT(df)

  # select cols from dfs
  {
    x1 <- df[[x1]]
    x2 <- df[[x2]]
    x1 <- x1[1:length(x1)-1];
    x2 <- x2[2:length(x2)]
  }
  {
    y1 <- df[[y1]]
    y2 <- df[[y2]]
    y1 <- y1[1:length(y1)-1];
    y2 <- y2[2:length(y2)]
  }
  # make temptib
  tempTib <- tibble::tibble(x1,y1,x2,y2)
  # add matrix as list col
  tempTib <- dplyr::mutate(tempTib, ptsMat = purrr::pmap(tempTib, function(x1,x2,y1,y2)
  {
    m <- matrix(c(x1,y1,x2,y2), ncol = 2, byrow = T)
    return(m)
  }))

  # make multilinestring from list col
  ml <- sf::st_multilinestring(tempTib$ptsMat)

  # return ml obj
  return(ml)
}

# ends here
