
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

dput(unique(det$tag_family))

depth_rec_range <- qread(here::here("data-saved",
                                    "rec-range-depth",
                                    "rec_range_depth.qs")) %>%
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
                                                 "station_no"), delim = "-") %>%
  filter(glatos_array != "OSM")

unique(is.na(depth_rec_range$y))

glimpse(depth_rec_range)
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
rm(det)
rm(det_select)
gc()

# det_select <- det_select %>%
#   filter(tag_serial_number == "1382382")

det_join <- det_select_dt %>%
  split(.$tag_serial_number) %>%
  map(~ .x %>%
        left_join(depth_rec_range, relationship = "many-to-many"),
      .progress = TRUE)

glimpse(det_join)



rm(depth_rec_range)
rm(det_select_dt)
gc()



# n <- 2
#
# x <- lapply(det_join, function(x) split(unlist(x),
#                                         cut(seq_along(unlist(x)),
#                                             n, labels = FALSE)))
# y <- unlist(x, recursive = F)
# str(y)

# something happens here to create NAs
det_bpa <- det_join %>%
  map(~ .x %>%
        mutate(
          benthic_susp = if_else(converted_sensor < (bathy_raster - 5),
                                 true = "suspended", false = "benthic"

          ),
          # month_no = month(detection_timestamp_est),
          # month_abb = month(detection_timestamp_est, abbr = TRUE,
          #                   label = TRUE),
          # year = year(detection_timestamp_est)
        ), .progress = TRUE
  )
rm(det_join)




# det_t <- det_bpa %>%
#   map(~ .x %>%
#         filter(is.na(benthic_susp)))
#
# det_ts <- det_t %>%
#   bind_rows(.id = "tag_serial_number")
#
#
# det_ts %>%
#   distinct(x)

# det_ts_sf <- det_ts %>%
#   filter(!is.na(x)) %>%
#   st_as_sf(coords = c("x", "y"),
#            crs = 32618)
#
# mapview::mapview(det_ts_sf)

# det_ts %>%
#   distinct(station)
# depth_rec_range %>%
#   filter(station == "OSM-056")

det_bpa_combo <- det_bpa %>%
  bind_rows(.id = "tag_id")

qsave(det_bpa_combo, here:::here(
  "data-saved",
  "det-benthic-suspended",
  "detection_benthic_suspended_list_lwf.qs"))

dep_bp_sum <- det_bpa %>%
  map(~ .x %>%
        group_by(detection_timestamp_est, benthic_susp) %>%
        summarise(
          n = n(),

        ) %>%
        ungroup() %>%
        group_by(detection_timestamp_est) %>%
        mutate(
          n_tot = sum(n),
          perc = n / n_tot
        ) %>%
        ungroup(),
      .progress = TRUE
  ) %>%
  bind_rows(.id = "tag_serial_number") %>%
  mutate(
    doy = yday(detection_timestamp_est),
    month_no = lubridate::month(detection_timestamp_est),
    month_abb = lubridate::month(detection_timestamp_est, abbr = TRUE,
                      label = TRUE),
  )

rm(det_bpa)
gc()
dep_bp_sum

qsave(dep_bp_sum, here:::here(
  "data-saved",
  "det-benthic-suspended",
  "detection_benthic_suspended_wf.qs"))
