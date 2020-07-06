#' Remove positions within a bounding box.
#'
#' @param data A dataframe or extension which contains capitalised X and Y coordinates.
#' @param atp_xmin The min X coordinates of attractor locations.
#' @param atp_xmax The max X coordinates of attractor locations.
#' @param atp_ymin The min Y coordinates of attractor locations.
#' @param atp_ymax The max Y coordinates of attractor locations.
#'
#' @return A data frame of tracking locations with attractor points removed.
#' @export
#'
atl_rm_attractor <- function(data,
                            atp_xmin = 639470,
                            atp_xmax = 639472,
                            atp_ymin = 5887143,
                            atp_ymax = 5887145){
  X <- Y <- NULL
  # check input type
  assertthat::assert_that("data.frame" %in% class(data),
                          msg = "rmAttractor: input not a dataframe object!")

  # include asserts checking for required columns
  {
    names_req <- c("X", "Y")
    atlastools:::atl_check_data(data, names_req)
  }

  # check input length of attractors
  {
    assertthat::assert_that(length(unique(length(atp_xmin),
                                          length(atp_xmax),
                                          length(atp_ymin),
                                          length(atp_ymax))) == 1,
                          msg="rmAttractor: different attractor coord lengths")
  }

  # convert to data.table
  {
    # convert both to DT if not
    if(is.data.table(data) != TRUE) {data.table::setDT(data)}
  }

  # remove attractors
  {
    purrr::pwalk(list(atp_xmin, atp_xmax, atp_ymin, atp_ymax),
                function(axmin, axmax, aymin, aymax){

      data <- data[!((X > axmin) & (X < axmax) &
                 (Y > aymin) & (Y < aymax)),]
    })
  }

  assertthat::assert_that("data.frame" %in% class(data),
    msg = "cleanData: cleanded data is not a dataframe object!")

  return(data)
}

# ends here
