#' funcDistance
#'
#' @param x A column name in a data.frame object that contains the numeric X or longitude coordinate for position data.
#' @param y A column name in a data.frame object that contains the numeric Y or latitude coordinate for position data.
#' @param df A dataframe object of or extending the class data.frame, which must contain at least two coordinate columns for the X and Y coordinates.
#'
#' @return Returns a vector of distances between consecutive points.
#' @export
#'
funcDistance = function(df, x = "x", y = "y"){
  #check for basic assumptions
  assertthat::assert_that(is.data.frame(df),
                          is.character(x),
                          is.character(y),
                          msg = "some df assumptions are not met")

  dist <- dplyr::case_when(nrow(df) > 1 ~
                             # cases where sufficient data
                             {
                               dm <- as.matrix(dist(df[,c(x,y)]))
                               # add NA to the subdiagonal
                               dm <- c(NA, dm[row(dm) == col(dm) + 1])
                               # return dm
                               dm
                             },
                           nrow(df) == 1 ~ {0.0},
                           TRUE ~ {as.numeric(NA)})

  return(dist)
}

#### a function for patch end to patch start distances ####


