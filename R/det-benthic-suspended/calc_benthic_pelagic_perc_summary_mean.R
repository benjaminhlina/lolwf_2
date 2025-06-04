
# ---- Load packages ----
{
  library(amt)
  library(dplyr)
  library(data.table)
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
                  "final_cleaned_rec_detection_data.qs"))
glimpse(det)




det <- det %>%
  arrange(tag_serial_number, date) %>%
  group_by(tag_serial_number) %>%
  filter(date > min(date) + hours(168)) %>%
  ungroup()


dput(unique(det$tag_family))

depth_rec_range <- qread(here::here("data-saved",
                                    "rec-range-depth",
                                    "rec_range_depth_random.qs")) %>%
  select(-sttn_ds) %>%
  rename(
    deploy_long = dply_ln,
    deploy_lat = dply_lt,
    rec_radius = rec_rds
  ) %>%
  filter(!is.na(bathy_raster)) %>%
  mutate(
    rec_radius = as.character(rec_radius),
    deploy_long = as.character(deploy_long),
    deploy_lat = as.character(deploy_lat)
  ) %>%
  tidyr::separate_wider_delim(station, names = c("glatos_array",
                                                 "station_no"), delim = "-",
                              cols_remove = FALSE) %>%
  filter(glatos_array != "OSM")

unique(is.na(depth_rec_range$y))

glimpse(depth_rec_range)

t <- depth_rec_range %>%
  st_as_sf(coords = c("x", "y"),
           crs = 32618)
mapview::mapview(t)

glimpse(det)
det <- det %>%
  mutate(
    rec_radius = case_when(
      tag_family %in% c("V16TP-4x", "V16P-4x-BLU-1-0136m", "V16-4x-BLU-1") &
        units == "m" & converted_sensor < 11 ~ 1400,
      tag_family %in% c("V16TP-4x", "V16P-4x-BLU-1-0136m", "V16-4x-BLU-1") &
        units == "m" & converted_sensor > 11 ~ 1700,
      tag_family %in% c("V13P-1x-BLU-1-0136m",
                        "V13-1x-BLU-1") &
        units == "m" ~ 1300),
    rec_radius = as.character(rec_radius),
    deploy_long = as.character(deploy_long),
    deploy_lat = as.character(deploy_lat)


  ) %>%
  filter(glatos_array != "OSM")
# )

det_select <- det %>%
  select(tag_serial_number, station, detection_timestamp_est, rec_radius,
         deploy_lat, deploy_long, units, converted_sensor) %>%
  filter(units == "m")


det_select_dt <- setDT(det_select)
glimpse(det_select_dt)

depth_rec_range_sum <- depth_rec_range %>%
  group_by(glatos_array, station_no, station, deploy_long, deploy_lat) %>%
  summarise(
    mean_depth = round(mean(bathy_raster), 1),
    sd_depth = round(sd(bathy_raster), 1),
    sem_depth = round((sd(bathy_raster) / sqrt(n())), 1)
  ) %>%
  ungroup() %>%
  glimpse()

# rm(det)
# rm(det_select)
# gc()

# det_select <- det_select %>%
#   filter(tag_serial_number == "1382382")

det_join <- det_select_dt %>%
  split(.$tag_serial_number) %>%
  map(~ .x %>%
        left_join(depth_rec_range, relationship = "many-to-many"),
      .progress = TRUE)

glimpse(det_join)



# rm(depth_rec_range)
# rm(det_select_dt)
gc()



# n <- 2
#
# x <- lapply(det_join, function(x) split(unlist(x),
#                                         cut(seq_along(unlist(x)),
#                                             n, labels = FALSE)))
# y <- unlist(x, recursive = F)
# str(y)
#


# t <- det_join$`1382382` %>%
#   filter(bathy_raster > 0) %>%
#   filter(converted_sensor <= bathy_raster) %>%
#
#   mutate(
#     bathy_limit = case_when(
#       bathy_raster < 5 ~ bathy_raster,
#       bathy_raster > 5 ~ bathy_raster - 5,
#     ),
#     benthic_susp = case_when(
#       converted_sensor <= bathy_limit  ~ "suspended",
#       converted_sensor >= bathy_limit ~ "benthic",
#     ),
#   )
# t
# unique(t$benthic_susp)
#
#
# summary(t$bathy_raster)
#
# t %>%
#   filter(is.na(benthic_susp))

# t %>%
#   filter(detection_timestamp_est %in%
#            ymd_hms("2021-11-18 16:07:21", tz = "EST")) %>%
#   summary()
#
# t %>%
#   distinct(bathy_limit)
#
# t
# summary(t$bathy_limit)
# ggplot(data = t, aes(x = bathy_limit, y = converted_sensor)) +
# geom_point()
# something happens here to create NAs
det_bpa <- det_join %>%
  map(~ .x %>%
        filter(bathy_raster > 0) %>%
        filter(converted_sensor <= bathy_raster)

  )

depth_rec_range_sum <- det_bpa %>%
  bind_rows(.id = "tag_serial_number") %>%
  group_by(tag_serial_number, detection_timestamp_est,
           glatos_array, station_no, station, deploy_long, deploy_lat,
           converted_sensor, units) %>%
  summarise(
    mean_bathy_depth = mean(bathy_raster),

    sd_bathy_depth = round(sd(bathy_raster), 1),
    sem_bathy_depth = round((sd(bathy_raster) / sqrt(n())), 1)
  ) %>%
  ungroup() %>%
  mutate(
    bathy_limit = case_when(
      mean_bathy_depth < 5 ~ mean_bathy_depth,
      mean_bathy_depth > 5 ~ mean_bathy_depth - 5,
    ),
    benthic_susp = case_when(
      converted_sensor < bathy_limit  ~ "suspended",
      converted_sensor > bathy_limit ~ "benthic",
    ),
    doy = yday(detection_timestamp_est),
    month_no = lubridate::month(detection_timestamp_est),
    month_abb = lubridate::month(detection_timestamp_est, abbr = TRUE,
                                 label = TRUE),
  ) %>%
  glimpse()




qsave(depth_rec_range_sum, here("data-saved",
                                "det-benthic-suspended",
                                "detection_benthic_suspended_summary_lwf.qs"))








