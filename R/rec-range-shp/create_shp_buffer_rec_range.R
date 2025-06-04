# ---- Load packages ----
{
  library(dplyr)
  library(ggplot2)
  library(here)
  library(mapview)
  library(qs)
  library(purrr)
  library(sf)
  library(terra)
  library(tidyterra)
}

# ---- bring in data -----

det <- qread(here("data-saved",
                  "detection-data",
                  "removed-dead-dets",
                  "final_cleaned_depth_detection_data.qs"))


glimpse(det)



# ---- add radius based on tag ----
det <- det %>%
  mutate(
    rec_radius = case_when(
      tag_family %in% c("V16TP-4x", "V16P-4x-BLU-1-0136m", "V16-4x-BLU-1") &
        units == "m" & converted_sensor < 11 ~ 1400,
      tag_family %in% c("V16TP-4x", "V16P-4x-BLU-1-0136m", "V16-4x-BLU-1") &
        units == "m" & converted_sensor > 11 ~ 1700,
      tag_family %in% c("V13P-1x-BLU-1-0136m",
                        "V13-1x-BLU-1") &
        units == "m" ~ 1300,

    )
  )


# ---- get station names and locs ----

rec_locs <- det %>%
  distinct(
    station, deploy_long, deploy_lat,
    rec_radius
  ) %>%
  filter(!(is.na(rec_radius))) %>%
  mutate(
    station_dist = paste(station, rec_radius, sep = "_")
  )

rec_locs





# ---- convert to sf  ----

rec_locs_sf <- rec_locs %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"),
           crs = 4326)

# first create a data frame of distances to iterate over

dists <- data.frame(
  distance = unique(rec_locs_sf$rec_radius)
)
# next we will split the data frame by distance and iterate over it using map
buffer_rings <- dists %>%
  split(.$distance) %>%
  map(~ st_buffer(dist = .x$distance, rec_locs_sf)) %>%
  bind_rows(.id = "distance") %>%
  mutate(
    station_dist = paste(station, distance, sep = "_")
  )
  # st_cast("LINESTRING", warn = FALSE)




# now view buffer rings
mapview(buffer_rings)


# ---- export as shp ----

st_write(buffer_rings, here::here("data-saved",
                                  "rec-range-shp",
                                  "rec_range_buffer.shp"),
         append = FALSE)



# ---- create equally spaced pts to sample from ----

buffer_rings_split <- buffer_rings %>%
  mutate(
    station_dist = paste(station, distance, sep = "_")
  ) %>%
  split(.$station_dist) %>%
  map(~  .x %>%
        st_transform(crs = 32618) %>%
        st_make_grid(cellsize = 500, square = FALSE, what = "corners") %>%
        st_as_sf(),
      .progress = TRUE)

# buffer_rings_split$`LOH-032_1700`




# ---- combine and add meta data ----
buffer_rings_pt_combine <- buffer_rings_split %>%
  bind_rows(.id = "station_dist") %>%
  left_join(rec_locs, relationship = "many-to-many")

buffer_rings_pt_combine


mapview(buffer_rings_pt_combine)

st_write(buffer_rings_pt_combine, here::here("data-saved",
                                  "rec-range-shp",
                                  "rec_range_buffer_pts.shp"),
         append = FALSE)






