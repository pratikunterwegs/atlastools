#' A function to remove so-called attractor points which have an unknown source. To be run before cleaning the data.
#'
#' @param df A dataframe or extension which contains capitalised X and Y coordinates.
#' @param atp_xmin The min X coordinate of attractor locations.
#' @param atp_xmax The max X coordinate of attractor locations.
#' @param atp_ymin The min Y coordinate of attractor locations.
#' @param atp_ymax The max Y coordinate of attractor locations.
#'
#' @return A data frame of tracking locations with attractor points removed.
#' @export
#'
wat_rmAttractor <- function(df,
                            atp_xmin = 639470,
                            atp_xmax = 639472,
                            atp_ymin = 5887143,
                            atp_ymax = 5887145)
  X <- Y <- NULL
{
  # check input type
  assertthat::assert_that("data.frame" %in% class(df),
                          msg = "rmAttractor: input not a dataframe object!")

  # include asserts checking for required columns
  {
    dfnames <- colnames(df)
    namesReq <- c("X", "Y")
    for (i in 1:length(namesReq)) {
      assertthat::assert_that(namesReq[i] %in% dfnames,
                              msg = glue::glue('rmAttractor: {namesReq[i]} is
                         required but missing from data!'))
    }
  }

  # convert to data.table
  {
    # convert both to DT if not
    if(is.data.table(df) != TRUE) {setDT(df)}
  }

  # remove attractors
  {
    df <- df[!((X > atp_xmin) & (X < atp_xmax) &
                 (Y > atp_ymin) & (Y < atp_ymax)),]
  }

  assertthat::assert_that("data.frame" %in% class(df),
    msg = "cleanData: cleanded data is not a dataframe object!")

  return(df)
}

# ends here
