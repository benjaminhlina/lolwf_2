# ---- Load packages ----
{
  library(amt)
  library(dplyr)
  library(ggplot2)
  library(glatos)
  library(ggtext)
  library(here)
  library(janitor)
  library(lubridate)
  library(purrr)
  library(qs)
  library(stringr)
  library(sf)
  library(tidyr)

}

# ---- bring in data -----

det <- qread(here("data-saved",
                  "detection-data",
                  "removed-dead-dets",
                  "final_cleaned_depth_detection_data.qs"))

glimpse(det)
# take out oswego arrray
#
# ---- amt ----
lake_o_e <- st_read(here("shapefiles",
                         "lake-ontario",
                         "east-ext",
                         "east_lake_o_ext.shp")) %>%
  st_transform(crs = 4326)

lake_o <- st_read(here("shapefiles",
                       "lake-ontario",
                       "Shapefiles_LO",
                       "Thewatermask.shp")) %>%
  st_transform(crs = 4326)

# ---- crop to only have eastern lake o ----


eastern_lake_o <- st_crop(x = lake_o, y = lake_o_e)

# ---- remove the first week of behaviour

glimpse(det)

det <- det %>%
arrange(tag_serial_number, date) %>%
  group_by(tag_serial_number) %>%
  filter(date > min(date) + hours(168)) %>%
  ungroup()

# ---- create coa ----

glimpse(det)



coa <- det %>%
  mutate(
    time_bins = floor_date(detection_timestamp_est, unit = "20 mins"),
    month_no = month(detection_timestamp_est),
    month_abb = month(detection_timestamp_est, abbr = TRUE,
                      label = TRUE)
  ) %>%
  group_by(common_name_e, tag_serial_number, time_bins, month_no,
           month_abb, length,
           weight, sex, release_group, release_location, release_latitude,
           release_longitude,
           sensor_type, units) %>%
  summarise(
    n_det = n(),
    n_rec = n_distinct(station),
    y = mean(deploy_lat),
    x = mean(deploy_long),
    mean_sensor = mean(converted_sensor),
    sd_sensor = sd(converted_sensor),
    sem_sensor = sd(converted_sensor) / sqrt(n())
  ) %>%
  ungroup() %>%
  st_as_sf(coords = c("x", "y"),
           crs = 4326) %>%
  st_intersection(eastern_lake_o) %>%
  st_transform(crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs") %>%
  mutate(
    x = st_coordinates(.)[,"X"],
    y = st_coordinates(.)[,"Y"]
  ) %>%
  st_drop_geometry()

glimpse(coa)

qsave(coa, here("data-saved",
           "coa",
           "coa_20_min.qs"))
