#' getPatchData
#'
#' @param resPatchData A tibble with a nested list column of the raw data underlying each patch. This column is specified in the argument \code{dataColumn}
#' @param whichData Which data to return, the raw data underlying the patch, or a spatial features object with only the patch summary.
#' @param dataColumn Which nested list column contains the data.
#'
#' @return An object of type \code{sf} or \code{tibble} depending on which data is requested.
#' @export
#'
#' @examples
funcGetPatchData = function(resPatchData,
                            dataColumn = "data",
                            whichData = "spatial")
{
  data <- NULL
  # check somedata is a data.frame and has a resTime column
  {
    assertthat::assert_that("data.frame" %in% class(resPatchData),
                            msg = "not a dataframe object!")
    assertthat::assert_that(dataColumn %in% names(resPatchData),
                            msg = "data column not present!")

  }
  # return only spatial summary if requested
  if(whichData %in% c("spatial","Spatial"))
  {
    assertthat::assert_that("sf" %in% class(resPatchData),
                            msg = "not a spatial object, cannot return spatial data!")


    thisdata <- dplyr::select(resPatchData, -data)
    return(thisdata)
  }else{
    this_data <- sf::st_drop_geometry(resPatchData)
    rm(resPatchData)
    # this_data <- dplyr::select(this_data, -polygons)
    this_data <- tidyr::unnest(this_data, cols = dataColumn,
                               names_repair = "universal")
    return(this_data)

  }

}

# ends here
