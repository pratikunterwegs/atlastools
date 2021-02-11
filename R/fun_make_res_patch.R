#' Construct residence patches from position data.
#'
#' A cleaned movement track can be classified into residence patches using the
#' function \code{atl_res_patch}.
#' The function expects a specific organisation of the data: there should be
#' at least the following columns, \code{x}, \code{y}, \code{time}, and
#' \code{id}, all named in lower case, and corresponding to the coordinates,
#' timestamp in the UNIX format (seconds since 1970), and the identity of the
#' tracked individual.
#' The result contains only the data that was classified as a residence patch
#' and removes transit between them.
#' \code{atl_res_patch} requires only three parameters: (1) the distance
#' threshold between positions (called \code{buffer_size}), (2) the distance
#' threshold between clusters of positions (called \code{lim_spat_indep}),
#' and (3) the time interval between clusters (called \code{lim_time_indep}).
#' Clusters formed of fewer than a minimum number of positions can be excluded.
#' The exclusion of clusters with few positions can help in removing bias due to
#'  short stops, but if such short stops are also of interest, they can be
#' included by reducing the \code{min_fixes} argument.
#' Position covariates such as speed may also be summarised patch-wise by
#' passing covariate names and  summary functions as character vectors to the
#' \code{summary_variables} and \code{summary_functions} arguments, respectively
#' .
#'
#' @author Pratik R. Gupte
#' @param data A dataframe of values of any class that is or extends data.frame.
#'  The dataframe must contain at least two spatial coordinates, \code{x} and
#'  \code{y}, and a temporal coordinate, \code{time}. The names of columns
#'  specifying these can be passed as arguments below. The column \code{id}
#'  indicating animal id is \emph{required}.
#' @param buffer_radius A numeric value specifying the radius of the buffer to
#' be considered around each coordinate point. May be thought of as the distance
#'  that an individual can access, assess, or otherwise cover when at a discrete
#'   point in space.
#' @param lim_spat_indep A numeric value of distance in metres of the spatial
#' distance between two patches for them to the considered independent.
#' @param lim_time_indep A numeric value of time in minutes of the time
#' difference between two patches for them to be considered independent.
#' @param min_fixes The minimum number of fixes for a group of
#' spatially-proximate number of ponts to be considered a preliminary residence
#' patch.
#' @param summary_variables Optional variables for which patch-wise summary
#' values are required. To be passed as a character vector.
#' @param summary_functions The functions with which to summarise the summary
#' variables; must return only a single value, such as median, mean etc. To be
#' passed as a character vector.
#'
#' @return A data.frame extension object. This dataframe has the added column
#' \code{patch} and \code{patchdata}, indicating the patch identity and the
#' data used to construct the patch. In addition, there are columns with patch
#' summary variables.
#' @import data.table
#' @examples
#' \dontrun{
#' patches <- atl_res_patch(
#'   data = track_data,
#'   buffer_radius = 10,
#'   lim_spat_indep = 100,
#'   lim_time_indep = 30,
#'   min_fixes = 3,
#'   summary_variables = c("speed"),
#'   summary_functions = c("mean", "sd")
#' )
#' }
#' @export
#'
atl_res_patch <- function(data,
                          buffer_radius = 10,
                          lim_spat_indep = 100,
                          lim_time_indep = 30,
                          min_fixes = 3,
                          summary_variables = c(),
                          summary_functions = c()) {
  area <- disp_in_patch <- NULL
  dist_bw_patch <- dist_in_patch <- duration <- NULL
  id <- newpatch <- nfixes <- patch <- NULL
  patchdata <- polygons <- spat_diff <- NULL
  time <- time_diff <- time_end <- time_start <- NULL
  x_end <- x_start <- y_end <- y_start <- NULL

  # check data is a data.frame and has a resTime column
  # check if data frame
  assertthat::assert_that(is.data.frame(data),
    msg = glue::glue("getResPatch: input not a \\
                          dataframe object, \\
                          has class {stringr::str_flatten(class(data),
                                           collapse = ' ')}!")
  )

  assertthat::assert_that(min(c(
    buffer_radius, lim_spat_indep,
    lim_time_indep, min_fixes
  )) > 0,
  msg = "atl_make_res_patch: function needs \\
                          positive arguments"
  )

  # convert variable units from minutes to seconds
  lim_time_indep <- lim_time_indep * 60

  # get names and numeric variables
  names_req <- c("id", "x", "y", "time", summary_variables)

  # include asserts checking for required columns
  atl_check_data(data, names_expected = names_req)

  # make datatable to use functions
  if (!is.data.table(data)) {
    data.table::setDT(data)
  }

  # sort by time
  data.table::setorderv(data, time)
  assertthat::assert_that(min(diff(data$time)) >= 0,
    msg = "data for segmentation is not ordered by time"
  )
  tryCatch(
    expr = {
      # identify spatial overlap
      # assign spat diff columns
      data[, `:=`(
        spat_diff = atlastools::atl_simple_dist(
          data = data,
          x = "x", y = "y"
        ),
        time_diff = c(Inf, as.numeric(diff(time)))
      )]

      # first spatial difference is infinity for calculation purposes
      data[1, c("spat_diff")] <- Inf

      # merge points if not spatially independent
      # compare distance from previous point to buffer_radius
      data <- data[, patch := cumsum(spat_diff > (2 * buffer_radius) |
        time_diff > lim_time_indep)]

      # count fixes and patch and remove small patches
      # count number of points per patch
      data <- data[, nfixes := .N, by = c("id", "patch")]

      # remove patches with 2 or fewer points
      data <- data[nfixes >= min_fixes, ]
      data[, nfixes := NULL]

      # get time mean and extreme points for spatio-temporal independence calc
      # nest data
      data <- data[, list(list(.SD)), by = list(id, patch)]
      setnames(data, old = "V1", new = "patchdata")
      data[, nfixes := as.integer(lapply(patchdata, nrow))]

      # summarise mean, first and last
      data[, patch_summary := lapply(patchdata, function(dt) {
        dt2 <- dt[, unlist(lapply(.SD, function(d) {
          list(
            median = as.double(stats::median(d)),
            start = as.double(data.table::first(d)),
            end = as.double(data.table::last(d))
          )
        }), recursive = FALSE), .SDcols = c("x", "y", "time")]

        setnames(
          dt2,
          stringr::str_replace(colnames(dt2), "\\.", "_")
        )

        return(dt2)
      })]

      # assess independence using summary data
      patch_summary <- data[, unlist(patch_summary, recursive = FALSE),
        by = list(id, patch)
      ]
      data[, patch_summary := NULL]

      # get time bewteen start of n+1 and end of n
      patch_summary[, time_diff := c(
        Inf,
        as.numeric(time_start[2:length(time_start)] -
          time_end[seq_len(length(time_end) -
            1)])
      )]
      # get spatial difference from last to first point
      patch_summary[, spat_diff :=
        c(atl_patch_dist(
          data = patch_summary,
          x1 = "x_end", x2 = "x_start",
          y1 = "y_end", y2 = "y_start"
        ))]
      # set spat_diff 1 to Inf
      patch_summary[1, "spat_diff"] <- Inf

      # assess independence and assign new patch
      patch_summary[, newpatch := cumsum((time_diff > lim_time_indep) |
        (spat_diff > lim_spat_indep))]

      # get cols with old and new patch
      patch_summary <- patch_summary[, list(patch, newpatch)]

      # basic patch metrics for new patches
      # join patchdata to patch summary by new patch
      # expand data to prepare for new patches
      data <- data[, unlist(patchdata, recursive = FALSE),
        by = list(id, patch)
      ]

      data <- data.table::merge.data.table(data, patch_summary, by = "patch")
      data[, `:=`(patch = newpatch, newpatch = NULL)]

      # nest data again
      data <- data[, list(list(.SD)), by = .(id, patch)]
      setnames(data, old = "V1", new = "patchdata")
      data[, nfixes := as.integer(lapply(patchdata, nrow))]

      # basic metrics by new patch
      data[, patch_summary := lapply(patchdata, function(dt) {
        # get mandatory metrics
        dt2 <- dt[, unlist(lapply(.SD, function(d) {
          list(
            median = as.double(stats::median(d)),
            start = as.double(data.table::first(d)),
            end = as.double(data.table::last(d))
          )
        }), recursive = FALSE), .SDcols = c("x", "y", "time")]

        setnames(
          dt2,
          stringr::str_replace(colnames(dt2), "\\.", "_")
        )

        # now get optional metrics if any asked
        if (length(summary_variables) > 0) {
          dt3 <- data.table::dcast(dt, 1 ~ 1,
            fun.aggregate = eval(lapply(
              summary_functions,
              as.symbol
            )),
            value.var = summary_variables
          )
          # remove this vestigial column
          dt3[, `.` := NULL]
          return(cbind(dt2, dt3))
        } else {
          return(dt2)
        }
      })]
      # advanced metrics on ungrouped data
      # distance in a patch in metres
      data[, dist_in_patch := as.double(lapply(patchdata, function(df) {
        sum(atlastools::atl_simple_dist(data = df), na.rm = TRUE)
      }))]

      # distance between patches
      temp_data <- data[, unlist(patch_summary, recursive = FALSE),
        by = list(id, patch)
      ]
      data[, patch_summary := NULL]
      data[, dist_bw_patch := atl_patch_dist(
        data = temp_data,
        x1 = "x_end", x2 = "x_start",
        y1 = "y_end", y2 = "y_start"
      )]
      # displacement in a patch
      # apply func bw patch dist reversing usual end and begin
      temp_data[, disp_in_patch := sqrt((x_end - x_start)^2 +
        (y_end - y_start)^2)]
      # even more advanced metrics
      temp_data[, duration := (time_end - time_start)]

      # true spatial metrics
      data[, polygons := lapply(patchdata, function(df) {
        p1 <- sf::st_as_sf(df, coords = c("x", "y"))
        p2 <- sf::st_buffer(p1, dist = buffer_radius)
        p2 <- sf::st_union(p2)
        return(p2)
      })]

      # add area and circularity
      data[, area := as.double(lapply(polygons, sf::st_area))]
      data[, `:=`(circularity = (4 * pi * area) / as.double(lapply(
        polygons,
        function(pgon) {
          boundary <- sf::st_boundary(pgon)
          perimeter <- sf::st_length(boundary)
          return(as.numeric(perimeter)^2)
        }
      )))]

      # remove polygons
      data[, polygons := NULL]

      # remove patch summary from some data and add temp data, then del tempdata
      data <- data.table::merge.data.table(data, temp_data,
        by = c("id", "patch")
      )

      assertthat::assert_that(!is.null(data),
        msg = "make_patch: patch has no data"
      )

      return(data)
    },
    # null error function, with option to collect data on errors
    error = function(e) {
      message(glue::glue("there was an error in {unique(data$id)}:
                       {as.character(e)}"))
    }
  )
}

# ends here
