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
                               x1 <- df[1:nrow(df)-1,x]; x2 <- df[2:nrow(df),x]
                               y1 <- df[1:nrow(df)-1,y]; y2 <- df[2:nrow(df),y]

                               # get dist
                               c(NA, sqrt((x1 - x2)^2 + (y1 - y2)^2))
                             },
                           nrow(df) == 1 ~ {0.0},
                           TRUE ~ {as.numeric(NA)})

  return(dist)
}

#### a function for patch end to patch start distances ####
