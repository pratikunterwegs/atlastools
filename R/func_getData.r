#' Get data from the NIOZ servers. Untested, unlikely to work off the NIOZ network.
#'
#' @param tag A three digit numeric representing the ToA tag.
#' @param tracking_time_start Character representation of time (Central European Time, +1 UTC) from which start point data should be retrieved.
#' @param tracking_time_end Character time representing the end point corresponding to the above start point.
#' @param database The database name on the host server. Defaults to \code{db} for unknown reasons.
#' @param host The server address on which the data are stored. Defaults to \code{abtdb1.nioz.nl}.
#' @param username Username to access the data.
#' @param password Password to access the data. Contact \code{allert.bijleveld@nioz.nl} to get access.
#' @param tag_prefix A numeric specifying the tag prefix. Defaults to \code{31001000}
#'
#' @return A dataframe of agent positions, filtered between the required tracking times.
#' @import RMySQL
#' @export
#'
wat_get_data <- function(tag,
                      tracking_time_start,
                      tracking_time_end,
                      tag_prefix="31001000",
                      database = "some_database",
                      host = "abtdb1.nioz.nl",
                      username = "someuser",
                      password = "somepassword"){

  # check data types
  {
    assertthat::assert_that(is.numeric(tag), msg = "tag provided must be numeric")
    assertthat::assert_that(is.character(tracking_time_start), msg = "start tracking time is not a character")
    assertthat::assert_that(is.character(tracking_time_end), msg = "end tracking time is not a character")
    assertthat::assert_that(as.character(tag_prefix) == "31001000", msg = "tag prefix is not 31001000")

    db_params = c(host, username, password)
    purrr::walk(db_params, function(this_param)
    {
      assertthat::assert_that(is.character(this_param),
                              msg = glue::glue('{this_param} is not a character'))
    })
  }

  # process function arguments for sql database
  {
    tag <- glue::glue('{tag_prefix}{tag}')
    # convert to POSIXct format
    tracking_time_start <- as.POSIXct(tracking_time_start, tz="CET")
    tracking_time_end <- as.POSIXct(tracking_time_end, tz="CET")
    # convert time to UTC (which is the database format)
    attributes(tracking_time_start)$tzone <- "UTC"
    attributes(tracking_time_end)$tzone <- "UTC"
    # convert to numeric and ms (database format)
    tracking_time_start <- as.numeric(tracking_time_start) * 1000
    tracking_time_end <- as.numeric(tracking_time_end) * 1000
  }

  # connect to the ATLAS server
  {
    mydb = RMySQL::dbConnect(RMySQL::MySQL(), user=username, password=password, dbname=database, host=host)
  }

  # SQL code to retrive data
  {
    sql_query <- glue::glue_sql("select TAG, TIME, X, Y, NBS, VARX, VARY, COVXY 
    FROM LOCALIZATIONS 
    WHERE TAG = {`tag`} 
      AND TIME > {`tracking_time_start`} 
      AND TIME < {`tracking_time_end`} 
    ORDER BY TIME ASC")
  }

  # getdata
  tmp_data <- DBI::dbGetQuery(mydb, sql_query)

  # add aproximate SD of localization
  # d$SD<-maxSD(d)
  # a bit indirect, but there were some strange warnings with NaN produced.
  if(nrow(tmp_data)>0)
  {
    tmp_SD <- tmp_data$VARX + tmp_data$VARY + 2*tmp_data$COVXY
    tmp_data$SD <- 0
    tmp_data$SD[tmp_SD > 0] <- sqrt(tmp_SD[tmp_SD > 0])
  }else{
    tmp_data$SD <- numeric(0)
  }

  # close connection
  RMySQL::dbDisconnect(mydb)

  # or close all connections
  # lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x)

  # return a dataframe of values
  return(tmp_data)
}
