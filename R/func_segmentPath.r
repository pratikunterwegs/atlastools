#' segPath
#'
#' @param revdata A string filepath to data in csv format. The data must be the output of recurse analysis, or must include, in addition to X, Y and time columns, a residence time column named resTime, id, and tidalcycle.
#' @param resTimeLimit A numeric giving the time limit in minutes against which residence time is compared.
#' @param travelSeg A numeric value of the number of fixes, or rows, over which a smoother function is applied.
#' @param inferPatches A logical indicating whether residence patches should be inferred from temporal gaps in the data.
#' @param infPatchTimeDiff A numeric duration in seconds, of the minimum time difference between two points, above which, it is assumed worthwhile to examine whether there is a missing residence patch to be inferred.
#' @param infPatchSpatDiff A numeric distance in metres, of the maximum spatial distance between two points, below which it may be assumed few extreme movements took place between them.
#'
#' @return A data.frame extension object. This dataframe has the added column `resPatch` based on cumulative patch summing. Depending on whether `inferPatches` is set `TRUE`, the dataframe has additional inferred points. An additional column is created in each case, indicating whether the data are empirical fixes ('real') or 'inferred'.
#' @import data.table
#' @export
#'

funcSegPath <- function(revdata, resTimeLimit = 2, travelSeg = 5,
                        infPatchTimeDiff = 1800, infPatchSpatDiff = 100,
                        inferPatches = TRUE){

  # handle global variable issues
  infPatch<-nfixes<-posId<-resPatch<-resTime<-resTimeBool<-rollResTime <- NULL
  spatdiff <- time <- timediff <- type <- x <- y <- NULL

  # adding the inferPatches argument to prep for inferring
  # residence patches from missing data between travel segments

  # print param assumpts
  print(glue::glue('param assumpts...\n residence time threshold = {resTimeLimit}\n travel segment smoothing = {travelSeg}'))

  # read the file in
  {
    df <- data.table::fread(revdata)
    print(glue::glue('individual {unique(df$id)} in tide {unique(df$tidalcycle)} has {nrow(df)} obs'))
  }

  # get names and numeric variables
  dfnames <- names(df)
  namesReq <- c("id", "tidalcycle", "x", "y", "time", "resTime")
  numvars <- c("x","y","time","resTime")

  # include asserts checking for required columns
  {
    for (i in 1:length(namesReq)) {
      assertthat::assert_that(namesReq[i] %in% dfnames,
                              msg = glue::glue('{namesReq[i]} is required but missing from data!'))
    }
  }
  # include assert checking for data type
  {
    for(i in 1:length(numvars)){
      assertthat::assert_that("numeric" %in% class(df[,numvars[i]]),
                              msg = glue::glue('{numvars[i]} must be numeric but is of type {class(df[,numvars[i]])}'))
    }
  }

  ## SET THE DF IN ORDER OF TIME ##
  data.table::setorder(df,time)

  # check this has worked
  {
    assertthat::assert_that(min(diff(df$time)) >= 0,
                            msg = "data for segmentation is not ordered by time")
  }

  if(inferPatches == TRUE){
    # get the max and min time
    maxtime <- max(df$time); mintime <- min(df$time)

    # make a df with id, tidalcycle and time seq, with missing x and y
    # identify where there are missing segments more than 2 mins long
    # there, create a sequence of points with id, tide, and time in 3s intervals
    # merge with true df
    tempdf <- df[!is.na(time),]
    # get difference in time and space
    tempdf <- tempdf[,`:=`(timediff = c(diff(time), NA),
                           spatdiff = funcDistance(df = tempdf, x = "x", y = "y"))
                     # find missing patches if timediff is greater than specified (default 30 mins)
                     # AND spatdiff is less than specified (100 m)
                     ][,infPatch := cumsum(timediff > infPatchTimeDiff & spatdiff < infPatchSpatDiff)
                       # subset the data to collect only the first two points
                       # of an inferred patch
                       ][,posId := 1:(.N), by = "infPatch"
                         ][posId <= 2 & !is.na(infPatch),]
    # handle cases where there are inferred patches
    # if(max(tempdf$infPatch > 0))
    {
      # make list column of expected times with 3 second interval
      # assume coordinate is the mean between 'takeoff' and 'landing'
      infPatchDf <- tempdf[,nfixes:=length(seq(time[1], time[2], by = 3)),
                          by = c("id", "tidalcycle", "infPatch")
                          ][,.(time = (seq(time[1], time[2], by = 3)),
                               x = mean(x),
                               y = mean(y),
                               resTime = resTimeLimit),
                            by = c("id", "tidalcycle", "infPatch","nfixes")
                            ][infPatch > 0,
                              ][,type:="inferred"]
    }

    print(glue::glue('\n {max(tempdf$infPatch)} inferred patches with {nrow(infPatchDf)} positions\n'))

  }
  rm(tempdf); gc()

  # add type to df
  df[,type:="real"]

  # merge inferred data to empirical data
  df <- base::merge(df, infPatchDf, by = intersect(names(df), names(infPatchDf)), all = T)

  # sort by time
  data.table::setorder(df, time)

  # check this has worked
  {
    assertthat::assert_that(min(diff(df$time)) >= 0,
                            msg = "data for segmentation is not ordered by time")
  }

  # prep to assign sequence to res patches
  # to each id.tide combination
  # remove NA vals in resTime
  # set residence time to 0 or 1 predicated on <= 10 (mins)
  df <- df[!is.na(resTime) #& between(tidaltime, 4*60, 9*60),
           ][,resTimeBool:= ifelse(resTime < resTimeLimit, F, T)
             # get breakpoints if the mean over rows of length travelSeg
             # is below 0.5
             # how does this work?
             # a sequence of comparisons, resTime <= resTimeLimit
             # may be thus: c(T,T,T,F,F,T,T)
             # indicating two non-residence points between 3 and 2 residence points
             # the rolling mean over a window of length 3 will be
             # c(1,.67,.33,.33,.33,.67) which can be used to
             # smooth over false negatives of residence
             ][,rollResTime:=(zoo::rollmean(resTimeBool, k = travelSeg, fill = NA) > 0.5)
               # drop NAs in rolling residence time evaluation
               # essentially the first and last elements will be dropped
               ][!is.na(rollResTime),
                 ][,resPatch:= c(as.numeric(resTimeBool[1]),
                                 diff(resTimeBool))
                   # keeping fixes where restime > 10
                   ][resTimeBool == T,
                     # assign res patch as change from F to T
                     ][,resPatch:= cumsum(resPatch)]


  # add param assumptions
  df$resTimeLimit = resTimeLimit; df$travelSeg = travelSeg

  # remove useless df columns
  data.table::set(df, ,c("rollResTime", "resTimeBool"), NULL)

  return(df)

}

# ends here
