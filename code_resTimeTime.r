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
patch_data <- purrr::map(patch_data, watlasUtils::funcGetPatchData)
# make single dataframe
patch_data <- dplyr::bind_rows(patch_data)
patch_data <- dplyr::mutate_at(patch_data, dplyr::vars(dplyr::starts_with("time")), function(x){as.POSIXct(x, origin = "1970-01-01")})
summary_data <- dplyr::distinct(patch_data, id, tidalcycle, patch, time_mean, time_end, time_start, distBwPatch)

#### plot restime by time ####
ggplot(summary_data)+
  geom_hline(yintercept = 2, col = 2)+
  geom_path(data = patch_data, aes(time, resTime, group = tidalcycle), col = "grey")+
  geom_point(data = patch_data, aes(time, resTime, shape = type, col = factor(patch)))+
  facet_wrap(~tidalcycle, ncol = 1, scales = "free_x")+
  scale_x_time(labels = scales::time_format(format = "%Y-%m-%d %H:%M"))+
  geom_text(aes(time_mean, 100, label = patch))+
  geom_vline(aes(xintercept = time_end), lty = 3, size = 0.2)+
  scale_color_manual(values = pals::kovesi.rainbow(14))+
  ggthemes::theme_few()
