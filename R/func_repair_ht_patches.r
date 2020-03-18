#' A function to repair high tide data.
#'
#' @param patch_data_list A list of data.tables, each the output of make_res_patch. Must have an sfc geometry column.
#' @param spatIndepLim The spatial independence limit.
#' @param tempIndepLim The temporal independence limit.
#' @param bufferSize The buffer size for spatial polygons.
#'
#' @return A datatable with repaired high tide patches.
#' @import data.table
#' @export
#'
wat_repair_ht_patches <- function(patch_data_list,
                                  spatIndepLim = 100,
                                  tempIndepLim = 30,
                                  bufferSize = 10){

  # set gloabl variables to NULL
  patch <- polygons <- tide_number <- NULL
  time_start <- time_end <- x_start <- NULL
  x_end <- y_start <- y_end <- newpatch <- NULL
  timediff <- spatdiff <- new_tide_number <- NULL
  patchdata <- id <- patchSummary <- x <- NULL
  y <- resTime <- tidaltime <- waterlevel <- NULL
  distInPatch <- distBwPatch <- dispInPatch <- NULL
  type <- duration <- area <- nfixes <- time <- NULL

  # check data assumptions
  {
    # check for dataframe and sf object
    # check if data frame
    assertthat::assert_that(is.list(patch_data_list),
       msg = glue::glue('wat_repair_ht: input not a
              list, has class
              {stringr::str_flatten(class(patch_data_list),
              collapse = " ")}!'))

    assertthat::assert_that(min(c(bufferSize, spatIndepLim,
                                  tempIndepLim)) > 0,
                msg = "wat_repair_ht: function needs positive arguments")
  }

  # check that list elements are data tables with correct names
    namesReq <- c("id", "tide_number", "patch",
                  "x_start", "y_start", "x_end", "y_end",
                  "time_start", "time_mean", "time_end",
                  "type")

  # convert variable units
  tempIndepLim <- tempIndepLim*60

  tryCatch({

  # bind all datatable into a single datatable
  patch_data_list <- patch_data_list[unlist(purrr::map(patch_data_list,
      function(l) {is.data.table(l) & nrow(l) > 0 & all(namesReq %in% colnames(l))}))]
  data <- rbindlist(patch_data_list)

  # select first and last rows from each tide_number
  # and assess independence
  {
    # subset edge cases from main data
    edge_data <- data[data[,.I[patch == min(patch) | patch == max(patch)],
                           by = .(tide_number)]$V1]

    data <- data[data[,.I[patch != min(patch) & patch != max(patch)],
                 by = .(tide_number)]$V1]

    edge_data_summary <- edge_data[,.(patch, time_start, time_end, x_start, x_end,
                                      y_start, y_end, tide_number)]

    edge_data_summary[,`:=`(timediff = c(Inf,
                            as.numeric(time_start[2:length(time_start)] -
                                         time_end[1:length(time_end)-1])),
               spatdiff = c(watlastools::wat_bw_patch_dist(df = edge_data_summary,
                                        x1 = "x_end", x2 = "x_start",
                                        y1 = "y_end", y2 = "y_start")))]

    edge_data_summary[1,'spatdiff'] <- Inf

    # which patches are independent?
    # assign NA as tide number of non-independent patches
    # and to the patch number of non-indep patches
    edge_data_summary[,newpatch := (timediff > tempIndepLim |
                                              spatdiff > spatIndepLim)]
    edge_data_summary[newpatch == FALSE, "tide_number"] <- NA
    edge_data_summary[,newpatch := ifelse(newpatch == TRUE, patch, NA)]

    # nafill with last obs carried forward for now NA tides and patches
    edge_data_summary[,`:=`(new_tide_number = nafill(tide_number, "locf"),
                            newpatch = nafill(newpatch, "locf"))]
    edge_data_summary <- edge_data_summary[,.(tide_number, patch,
                                              new_tide_number, newpatch)]

    # merge summary with data
    # make a temporary reeating seq of id, tide and patch
    temp_ed <- edge_data[,.(id, tide_number, patch)]
    temp_ed <- temp_ed[rep(seq_len(nrow(temp_ed)), purrr::map_int(edge_data$patchdata, nrow)),]
    edge_data <- cbind(temp_ed, rbindlist(edge_data$patchdata))
    rm(temp_ed)

    edge_data <- merge(edge_data, edge_data_summary,
                       by = c("tide_number", "patch"))

  }

  # recalculate patch ids among the new tides
  {
    edge_data[,`:=`(tide_number = new_tide_number, new_tide_number=NULL,
                    patch = newpatch, newpatch=NULL)]

    edge_data <- edge_data[,.(list(.SD)), by=.(id, tide_number, patch)]
    setnames(edge_data, old = "V1", new = "patchdata")

    # get basic data summaries
    edge_data[,patchSummary:= lapply(patchdata, function(dt){
      dt <- dt[,.(time, x, y, resTime, tidaltime, waterlevel)]
      dt <- setDF(dt)
      dt <- dplyr::summarise_all(.tbl = dt,
                                 .funs = list(start = dplyr::first,
                                              end = dplyr::last,
                                              mean = mean))
      return(setDT(dt))
    })]
  }

  # advanced metrics on ungrouped data
  {
    # distance in a patch in metres
    edge_data[,distInPatch := lapply(patchdata, function(df){
      sum(watlastools::wat_simple_dist(df = df), na.rm = TRUE)
    })]

    # distance between patches
    tempdata <- edge_data[,unlist(patchSummary, recursive = FALSE),
                         by = .(id, tide_number, patch)]

    edge_data[,patchSummary:=NULL]
    edge_data[,distBwPatch := watlastools::wat_bw_patch_dist(df = tempdata,
                                                            x1 = "x_end", x2 = "x_start",
                                                            y1 = "y_end", y2 = "y_start")]
    # displacement in a patch
    # apply func bw patch dist reversing usual end and begin
    tempdata[,dispInPatch := sqrt((x_end - x_start)^2 + (y_end - y_start)^2)]
    # type of patch
    edge_data[, type := lapply(patchdata, function(df){
      a <- ifelse(sum(c("real", "inferred") %in% df$type) == 2,
                  "mixed", first(df$type))
      return(a)
    })]
  }

  # even more advanced metrics
  {
    tempdata[, duration := (time_end - time_start)]
  }

  # true spatial metrics
  {
    edge_data[, polygons := lapply(patchdata, function(df){
      p1 <- sf::st_as_sf(df, coords = c("x", "y"))
      p2 <- sf::st_buffer(p1, dist = bufferSize)
      p2 <- sf::st_union(p2)
      return(p2)
    })]

    # add area and circularity
    edge_data[,area := purrr::map_dbl(polygons, sf::st_area)]
    edge_data[,`:=`(circularity = (4*pi*area)/purrr::map_dbl(polygons, function(pgon){
      boundary <- sf::st_boundary(pgon)
      perimeter <- sf::st_length(boundary)
      return(as.numeric(perimeter)^2)
    }))]
  }

  # remove polygons here too
  edge_data[, polygons := NULL]

  # remove patch summary from some data and add temp data, then del tempdata
  edge_data <- merge(edge_data, tempdata, by = c("id", "tide_number", "patch"))
  edge_data[,nfixes := unlist(lapply(patchdata, nrow))]

  # reattach edge cases to regular patch data and set order by start time
  data <- rbind(data, edge_data)
  setorder(data, time_start)

  # fix distance between patches
  data[, distBwPatch := wat_bw_patch_dist(data)]

  # fix patch numbers in tides
  data[,patch:=1:length(nfixes), by=.(tide_number)]

  return(data)
  },
  error= function(e)
  {
    message(glue::glue('\nthere was an error in repair\n'))
  })
}
