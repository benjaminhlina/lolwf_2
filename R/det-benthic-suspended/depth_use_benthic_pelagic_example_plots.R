
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
# ---- amt ----
lake_o_e <- st_read(here("shapefiles",
                         "lake-ontario",
                         "east-ext",
                         "east_lake_o_ext.shp")) %>%
  st_transform(crs = 32618)

lake_o <- st_read(here("shapefiles",
                       "lake-ontario",
                       "Shapefiles_LO",
                       "Thewatermask.shp")) %>%
  st_transform(crs = 32618)

# ---- crop to only have eastern lake o ----


eastern_lake_o <- st_crop(x = lake_o, y = lake_o_e)

ggplot() +
  geom_sf(data = eastern_lake_o)

# ---- bring in data -----

det <- qread(here:::here(
  "data-saved",
  "det-benthic-suspended",
  "detection_benthic_suspended_list_lwf_random.qs"))


unique(det$tag_serial_number)

setkey(det, tag_serial_number)

det_1 <- det[.("1575335")]

setkey(det_1, NULL)

unique(det_1$detection_timestamp_est)
det_1 %>%
  distinct(detection_timestamp_est) %>%
  tail(n = 300)
setkey(det_1, detection_timestamp_est)
det_1_dets <- det_1[.(ymd_hms("2024-04-17 00:11:52", tz = "EST"))]


det_1_sf <- det_1_dets %>%
  st_as_sf(coords = c("x", "y"),
           crs = 32618)

det_1_rec <- det_1_dets %>%
  distinct(station, deploy_long, deploy_lat) %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"),
           crs = 4326)



p_det <- ggplot() +
  # geom_sf(data = eastern_lake_o) +
  geom_sf(data = det_1_sf, aes(fill = converted_sensor),
          shape = 21, size = 4) +
  geom_sf(data = det_1_rec, size = 4) +
  scale_fill_viridis_c(name = "Depth (m)", option = "H",
                       guide = guide_colorbar(reverse = TRUE,)) +
  # coord_sf(xlim = c(-77.1, -77.2)) +
  theme_bw(
    base_size = 10
  ) +
  guides(fill = guide_colourbar(reverse = TRUE,
                                theme = theme(
                                  # legend.key.width  = unit(0.5, "lines"),
                                  legend.ticks = element_line(colour = "black"),
                                  legend.key.height = unit(10, "lines")
                                ))) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = paste(unique(det_1_sf$tag_serial_number),
                  paste(unique(det_1_sf$detection_timestamp_est), "EST"), sep = " - ")
  )

p_det

ggsave(filename = here("plots",
                       "benthic-pelagic-example",
                       "detection_example_depth.png"),
       height = 5.5, width = 5.5, plot = p_det)

p_bathy <- ggplot() +
  # geom_sf(data = eastern_lake_o) +
  geom_sf(data = det_1_sf, aes(fill = bathy_raster),
          shape = 21, size = 4) +
  geom_sf(data = det_1_rec, size = 4) +
  scale_fill_viridis_c(name = "Depth (m)", option = "H",
                       guide = guide_colorbar(reverse = TRUE,)) +
  # coord_sf(xlim = c(-77.1, -77.2)) +
  theme_bw(
    base_size = 15
  ) +
  guides(fill = guide_colourbar(reverse = TRUE,
                                theme = theme(
                                  # legend.key.width  = unit(0.5, "lines"),
                                  legend.ticks = element_line(colour = "black"),
                                  legend.key.height = unit(10, "lines")
                                ))) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = paste(unique(det_1_sf$tag_serial_number),
                  paste(unique(det_1_sf$detection_timestamp_est), "EST"), sep = " - ")
  )
p_bathy


ggsave(filename = here("plots",
                       "benthic-pelagic-example",
                       "detection_example_bathy.png"),
       height = 5.5, width = 5.5, plot = p_bathy)


p_bahave <- ggplot() +
  geom_sf(data = det_1_sf, aes(fill = benthic_susp),
          shape = 21, size = 4) +
  geom_sf(data = det_1_rec, size = 4) +
  scale_fill_viridis_d(name = "Behaviour", option = "A",
                       begin = 0.6, end = 0.9, labels = c(
                         "Benthic", "Pelagic"
                       )) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = paste(unique(det_1_sf$tag_serial_number),
                  paste(unique(det_1_sf$detection_timestamp_est), "EST"), sep = " - ")
  )

p_bahave


ggsave(filename = here("plots",
                       "benthic-pelagic-example",
                       "detection_example_behave.png"),
       height = 5.5, width = 5.5, plot = p_bahave)




det_1_sum_1 <- det_1_dets %>%
  group_by(benthic_susp, detection_timestamp_est) %>%
  summarise(
    n = n(),
  ) %>%
  ungroup() %>%
  group_by(detection_timestamp_est) %>%
  mutate(
    n_tot = sum(n),
    perc = n / n_tot
  ) %>%
  ungroup() %>%
  mutate(
    date = floor_date(detection_timestamp_est, "1 day")
  )



det_1_sum <- det_1 %>%
  group_by(benthic_susp, detection_timestamp_est, station) %>%
  summarise(
    n = n(),
  ) %>%
  ungroup() %>%
  group_by(detection_timestamp_est) %>%
  mutate(
    n_tot = sum(n),
    perc = n / n_tot
  ) %>%
  ungroup() %>%
  mutate(
    date = floor_date(detection_timestamp_est, "1 day")
  )



p_mar <- det_1_sum %>%
  filter(detection_timestamp_est %in%  c(
    ymd_hms("2024-04-17 00:06:50", tz = "EST"),
    ymd_hms("2024-04-17 00:11:52", tz = "EST"),
    ymd_hms("2024-04-17 00:29:47", tz = "EST"),
    ymd_hms("2024-04-17 00:40:09", tz = "EST"),
    ymd_hms("2024-04-17 00:43:24", tz = "EST")
  )) %>%
  ggplot(
    aes(x = detection_timestamp_est, y = perc,
        fill = benthic_susp)) +
  geom_bar(stat = "identity",
           colour = "black",
           aes(group = station),
           # position = position_dodge()
  ) +
  # geom_label(aes(x = detection_timestamp_est, y = perc,
  #               label = station,
  #               group = benthic_susp),
  #            fill = "white") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  # scale_y_datetime(breaks = date_breaks("1 hour"),
  #                  labels = date_format("%H:%M")) +
  scale_fill_viridis_d(name = "Behaviour", option = "A",
                       begin = 0.4, end = 0.8, labels = c(
                         "Benthic", "Pelagic"
                       )) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = paste(unique(det_1_sf$tag_serial_number), sep = " - ")
  )

p_mar

ggsave(filename = here("plots",
                       "benthic-pelagic-example",
                       "detection_example_perc_bar_april.png"),
       height = 8.5, width = 16, plot = p_mar)
