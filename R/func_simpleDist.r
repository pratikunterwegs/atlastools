#' Calculate euclidean distances between successive points.
#'
#' @param x A column name in a data.frame object that contains the numeric X or longitude coordinate for position data.
#' @param y A column name in a data.frame object that contains the numeric Y or latitude coordinate for position data.
#' @param data A dataframe object of or extending the class data.frame, which must contain at least two coordinate columns for the X and Y coordinates.
#'
#' @return Returns a vector of distances between consecutive points.
#' @export
#'
atl_simple_dist <- function(data, x = "x", y = "y"){
  #check for basic assumptions
  assertthat::assert_that(is.data.frame(data),
                          is.character(x),
                          is.character(y),
                          msg = "simpleDist: some data assumptions are not met")

  dist <- dplyr::case_when(nrow(data) > 1 ~
                             # cases where sufficient data
                             { { x1 <- data[[x]][seq_len(nrow(data) - 1)]
                                 x2 <- data[[x]][2:nrow(data)]
                               }
                               { y1 <- data[[y]][seq_len(nrow(data) - 1)]
                                 y2 <- data[[y]][2:nrow(data)]
                               }

                               # get dist
                               c(NA, sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2))
                             },
                           nrow(data) == 1 ~ {0.0},
                           TRUE ~ { as.numeric(NA)})
  return(dist)
}

#### a function for patch end to patch start distances ####
