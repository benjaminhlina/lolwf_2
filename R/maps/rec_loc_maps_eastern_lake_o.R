# ---- Load packages ----
{
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(glatos)
  library(gratia)
  library(ggtext)
  library(here)
  library(janitor)
  library(mgcv)
  library(lubridate)
  library(purrr)
  library(qs)
  library(stringr)
  library(sf)
  library(tidyr)

}

# ---- bring in data -----



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
# st_transform(crs = 32618)


rec_locs <- qread(here("BenthicModShare",
                       "eastern_lake_ontario_arrays_2021_plus_bq.qs"))




rec_loc_filter <- rec_locs %>%
  mutate(
    month_abb = month(deploy_date_time, label = TRUE, abbr = TRUE)
  ) %>%
  distinct(station, glatos_array, geometry, year,
           # month_abb
  ) %>%
  arrange(glatos_array) %>%
  print(n = 43) %>%
  filter(!(glatos_array %in% c("OSM", "OSW", "OSE", "OSJ", "OSA")))
# ---- crop to only have eastern lake o ----


eastern_lake_o <- st_crop(x = lake_o, y = lake_o_e)


# ----- Plot ----
p <- ggplot() +
  geom_sf(data = eastern_lake_o, fill = "slategray3",
          alpha = 0.4) +
  geom_sf(data = rec_loc_filter,
          # aes(fill = glatos_array),
          shape = 21,
          alpha = 0.85,
          size = 3.5,
          fill = "darkorange2") +
  # scale_fill_viridis_d(option = "B",
  #                    name = "GLATOS Array") +
  facet_wrap(~ year) +
  annotation_north_arrow(location = "tl") +
  annotation_scale(location = "br") +
  theme_bw(
    base_size = 15
  ) +
  # guides(fill = guide_legend(override.aes = list(size = 1.5))) +
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(),
    # legend.position = "inside",
    # legend.position.inside = c(0.9225, 0.3),
    # legend.text = element_text(size = 11),
    # legend.title = element_text(size = 12),
    # legend.background = element_blank(),
    # legend.key.width = unit(0.3, "cm"),
    # legend.key.height = unit(0.3,"cm")
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

p

ggsave(here("plots",
            "receiver-maps",
            "glatos_station_map_eastern_lake_o.png"),
       width = 15, height = 14.5, plot = p)

