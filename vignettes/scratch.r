## Getting raw data

`atlastools` has the function `wat_get_data` Gets data from the NIOZ server.
`wat_get_data` is untested because the data are stored on the NIOZ servers. I haven't worked out whether connectting and retrieving this data is actually possible. An example is provided, but will not work, because the data access arguments are not public.

```{r get_raw_data}
# get an example raw data file

prelim_data <- wat_get_data(tag = 435,
                            tracking_time_start = "<release_date_435>",
                            tracking_time_end = "2018-10-31",
                            username = "<database_username>",
                            password = "<database_password>s")
```

## Cleaning raw data

Cleaning the raw data involves removing so-called attractor points, and filtering data on some criteria.
The raw data should look like this when read in.

```{r peek_raw_data}
library(data.table)
raw_data <- fread("vignette_data/413_004_revisit.csv")

head(raw_data)
```

We first remove any points lying inside the region of attractor points.

```{r remove_attractors}
# here we remove attractor points which we have already identified
attractor_points <- fread("../private/attractor_points.txt")

temp_data <- wat_rm_attractor(df = raw_data,
                              atp_xmin = atp$xmin,
                              atp_xmax = atp$xmax,
                              atp$ymin = atp$ymin,
                              atp$ymax = atp$ymax)
```

Now we clean the data by applying a median filter, removing data with too few base stations, removing data above a standard deviation threshold, and removing data with unrealistic speeds (in kmph).

```{r clean_data}
# here we clean the data
clean_data <- wat_clean_data(somedata = temp_data,
                             moving_window = 3,
                             nbs_min = 0,
                             sd_threshold = 100,
                             filter_speed = TRUE,
                             speed_cutoff = 150)
```

## Aggregating data

For data quality and computational reasons, we might want to aggregate the data over a specific interval. Here, we use a 30 second interval for aggregation. This is done using `wat_agg_data`.

```{r clean_raw_data}
# aggregate over 30 seconds
agg_data <- wat_agg_data(df = clean_data, interval = 30)
```

# Adding covariates

## Adding tidal cycles

We want to eventually split the full track into subsets, these subsets to be separated by high tide.
This means adding the time of high tide to the data using `wat_add_tide`,
The function needs a data frame of high tide times; this is not covered here.

```{r add_tide_data}
# add tide data
agg_data <- wat_add_tide(df = agg_data,
                         tide_data = "../private/")
```