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
  library(mapview)

}

# ---- bring in data -----

det <- qread(here("data-saved",
                  "detection-data",
                  "removed-dead-dets",
                  "final_cleaned_detection_data.qs"))

lake_o_e <- st_read(here("shapefiles",
                         "lake-ontario",
                         "east-ext",
                         "east_lake_o_ext.shp")) %>%
  st_transform(crs = 4326)


mapview(lake_o_e)

glimpse(det)
month()
det_summary <- det %>%
  mutate(
    month = month(detection_timestamp_est, abbr = TRUE, label = TRUE)
  ) %>%
  group_by(month, tag_serial_number, station, deploy_long, deploy_lat) %>%
  summarise(
    n = n_distinct(station)
  ) %>%
  ungroup()



det_summary

ds <- readr::read_csv(here::here("data-raw",
                           "metadata",
                           "GLATOS_receiverLocations_20240627_210634.csv")) %>%
  janitor::clean_names() %>%
  mutate(
    year = year(deploy_date_time)
  ) %>%
  filter(year >= 2021)

ds

ds_sf <- ds %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"),
           crs = 4326) %>%
  filter(glatos_project %in% c("LODIC", "LOIBM",
                               "ELOMA", "ELOCS",
                               "GOATS"))

mapview(ds_sf, zcol = "glatos_array")

# ds %>%
#   filter(station == "LOK-030")

ds_sf %>%
  st_crop(lake_o_e) %>%
  # distinct(glatos_project)
  filter(glatos_project %in% c("LODIC", "LOIBM",
                               "ELOMA", "ELOCS",
                               "GOATS")
         & !(glatos_array %in% c(
           # "TNT",
                                 # "TNN",
                                 # "TRR",
                                 # "DBG",
                                 "MPT",
                                 # "MOR",
                                 "KNP",
                                 # "TGN",
                                 # "NPR",
                                 # "LRH",
                                 # "HBY",
                                 # "GNR",
                                 # "CNW",
                                 "HOW",
                                 "HLD",
                                 "BTC",
                                 "FAS",
                                 "LND",
                                 "EHK",
                                 "WHK"
                                 ))) %>%
  mapview(zcol = "glatos_project")

mapview(dat_sf)

lake_o_e_rec <- ds_sf %>%
  st_crop(lake_o_e) %>%
  # distinct(glatos_project)
  filter(glatos_project %in% c("LODIC", "LOIBM",
                               "ELOMA", "ELOCS",
                               "GOATS")
         & !(glatos_array %in% c(
           # "TNT",
                                 # "TNN",
                                 # "TRR",
                                 # "DBG",
                                 "MPT",
                                 # "MOR",
                                 "KNP",
                                 # "TGN",
                                 # "NPR",
                                 # "LRH",
                                 # "HBY",
                                 # "GNR",
                                 # "CNW",
                                 "HOW",
                                 "HLD",
                                 "BTC",
                                 "FAS",
                                 "LND",
                                 "EHK",
                                 "WHK"
         )))


mapview(lake_o_e_rec)

qs::qsave(lake_o_e_rec, here::here("data-saved",
                                   "rec-location",
                                   "eastern_lake_ontario_arrays_2021_plus_bq.qs"))


det_sf <- det %>%
  group_by(station, deploy_lat, deploy_long) %>%
  summarise(
    n = n()
  ) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"),
           crs = 4326)

# det_sf %>%
#   # distinct(station, deploy_long, deploy_lat) %>%
# mapview()
#
#
#
#
# det_summary
#

glimpse(det_summary)
glimpse(lake_o_e_rec)

lake_o_e_rec_f <- lake_o_e_rec %>%
  filter(
    !(station %in% det_summary$station)
  ) %>%
  dplyr::select(station,
                # year, deploy_date_time, recover_date_time,
         glatos_seasonal) %>%
  mutate(
    deploy_long = st_coordinates(.)[,"X"],# grab lon
    deploy_lat = st_coordinates(.)[,"Y"],# grab lat
  ) %>%
  st_drop_geometry() %>%
  mutate(
    n = 0
  )




det_summary


mapview(lake_o_e_rec_f)

det_summary_false <- expand_grid(
  month = month.abb,
  tag_serial_number = unique(det_summary$tag_serial_number),
  lake_o_e_rec_f

)



det_summary_update <- bind_rows(
  det_summary,
  det_summary_false
)


det_summary_update_sf <- det_summary_update %>%
  st_as_sf(
    coords = c("deploy_long",
               "deploy_lat"),
    crs = 4326
  )




det_summary_updatea_t <- det_summary_update %>%
  distinct(tag_serial_number, station, n, deploy_lat, deploy_long) %>%
  st_as_sf(
    coords = c("deploy_long",
               "deploy_lat"),
    crs = 4326
  )


mapview(det_summary_updatea_t, zcol = "n")
ggplot() +
  geom_sf(data = det_summary_updatea_t, aes(colour = factor(n))) +
  facet_wrap(~ tag_serial_number)

# lake_o_e_rec %>%
#   filter(
#     !(station %in% det_summary$station)
#   ) %>%
#   mapview()




