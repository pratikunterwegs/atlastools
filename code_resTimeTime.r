#### code to make restime by time plots ####

# load libs
library(data.table)
library(ggplot2)
library(dplyr)

# load data

data_files <- list.files("tests/testdata/recdata/", pattern = "recurse", full.names = TRUE)
tide_files <- list.files("tests/testdata/htdata/", full.names = TRUE)


#### infer residence ####
this_data <- list(data_files, tide_files)
this_data <- purrr::map(this_data, purrr::map, readr::read_csv)
this_data <- purrr::map2(this_data[[1]], this_data[[2]], watlasUtils::funcInferResidence)

#### classify path ####
classified_data <- purrr::map(this_data, watlasUtils::funcClassifyPath)

#### get residence patch sf objects ####
patch_data <- purrr::map(classified_data, watlasUtils::funcGetResPatch, makeSf = TRUE)

#### get basic patch data ####
# apply new function to get only basic data
patch_points <- purrr::map(patch_data, watlasUtils::funcGetPatchData)
# make single dataframe
patch_points <- dplyr::bind_rows(patch_points)
patch_points <- dplyr::mutate_at(patch_points, dplyr::vars(dplyr::starts_with("time")), function(x){as.POSIXct(x, origin = "1970-01-01")})
summary_data <- dplyr::distinct(patch_points, id, tidalcycle, patch, time_mean, time_end, time_start, distBwPatch)

#### plot restime by time ####
fig_time <- ggplot(summary_data)+
  geom_hline(yintercept = 2, col = 2)+
  geom_line(data = patch_points, aes(time, resTime, group = tidalcycle), col = "grey", size = 0.3)+
  geom_point(data = patch_points,
             aes(time, resTime, shape = type, col = factor(patch)),
             alpha = 0.2, size = 2)+
  facet_wrap(~tidalcycle, ncol = 1, scales = "free_x",
             labeller = "label_both")+
  scale_x_time(labels = scales::time_format(format = "%Y-%m-%d\n %H:%M"))+
  geom_text(aes(time_mean, 100, label = patch))+
  geom_vline(aes(xintercept = time_end), lty = 3, size = 0.2)+
  scale_color_manual(values = pals::kovesi.rainbow(14))+
  ggthemes::theme_few()+
  labs(x = "time", y = "residence time (mins)", col = "patch",
       title = "residence time ~ time")

#### plot patch over time ####
# get spatial data
data_spat <- purrr::map(patch_data, watlasUtils::funcGetPatchData, whichData = "spatial")
data_spat <- do.call(what = sf:::rbind.sf, args = data_spat)
{patch_traj <- purrr::map(patch_data, watlasUtils::funcPatchTraj)
  data_traj <- sf::st_drop_geometry(data_spat) %>%
    dplyr::distinct(id, tidalcycle) %>%
    dplyr::mutate(lines = patch_traj)
  patch_traj <- sf::st_as_sf(data_traj, sf_column_name = "lines")
}
# make plot
fig_spat <- ggplot()+
  geom_sf(data = data_spat, aes(fill = factor(patch)), alpha = 0.7, col = 'transparent')+
  geom_sf(data = patch_traj, col = "red", size = 0.2)+
  #geom_text(aes(x_mean, y_mean, label = patch))+
  facet_wrap(~tidalcycle, ncol = 1, labeller = label_both)+
  scale_fill_manual(values = pals::kovesi.rainbow(14))+
  ggthemes::theme_few()+
  labs(x = "long", y = "lat", fill = "patch",
       title = "patches in space")

# combine
library(gridExtra)
{pdf(file = "tests/testdata/fig_testPatches.pdf", width = 16, height = 8)
grid.arrange(fig_time,fig_spat, ncol = 2)
dev.off()}

# ends here
