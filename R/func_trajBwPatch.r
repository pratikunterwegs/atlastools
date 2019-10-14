#' patchTraj
#'
#' @param df A dataframe with at least 2 pairs of coordinate columns, such as X_end Y_end, X_start, Y_start.
#' @param x1 Coordinate column for the beginning X coordinate of the linestring; this should be the last X coordinate of each patch if inter-patch linestrngs are to be constructed. It is possible to use the X_start coordinate, but this will lead to issues and nonsensical output at this point.
#' @param x2 Coordinate column for the end X coordinate of the linestring; this should be the first X coordinate of each patch if inter-patch linestrngs are to be constructed.
#' @param y1 Coordinate column for the beginning Y coordinate of the linestring; this should be the last Y coordinate of each patch if inter-patch linestrngs are to be constructed. It is possible to use the X_start coordinate, but this will lead to issues and nonsensical output at this point.
#' @param y2 Coordinate column for the end Y coordinate of the linestring; this should be the first Y coordinate of each patch if inter-patch linestrngs are to be constructed.
#'
#' @return An `sf` `MULTILINESTRING` object representing the linear path between N patches, starting at the end point of the first patch, and ending at the first point of the Nth patch.
#' @export
#'
funcPatchTraj <- function(df, x1 = "X_end", x2 = "X_start",
                          y1 = "Y_end", y2 = "Y_start"){
  # must assert df has correct columns

  # select cols from dfs
  {
    x1 <- pull(df, x1)
    x2 <- pull(df, x2)
    x1 <- x1[1:length(x1)-1];
    x2 <- x2[2:length(x2)]}
  {
    y1 <- pull(df, y1)
    y2 <- pull(df, y2)
    y1 <- y1[1:length(y1)-1];
    y2 <- y2[2:length(y2)]
  }
  # make temptib
  tempTib = tibble(x1,y1,x2,y2)
  # add matrix as list col
  tempTib = tempTib %>%
    mutate(ptsMat = pmap(tempTib, function(x1,x2,y1,y2){
      matrix(c(x1,y1,x2,y2), ncol = 2, byrow = T)
    }))

  # make multilinestring from list col
  ml = st_multilinestring(tempTib$ptsMat)

  # return ml obj
  return(ml)
}
