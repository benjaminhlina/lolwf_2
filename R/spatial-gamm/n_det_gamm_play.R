# ---- Load packages ----
{
  library(amt)
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

det <- qread(here("data-saved",
                  "coa",
                  "coa_20_min.qs"))

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
  distinct(station, glatos_array, geometry,
           # month_abb
  ) %>%
  arrange(glatos_array) %>%
  print(n = 43) %>%
  filter(!(glatos_array %in% c("OSM", "OSW", "OSE", "OSJ", "OSA")))
# ---- crop to only have eastern lake o ----


eastern_lake_o <- st_crop(x = lake_o, y = lake_o_e)

ggplot() +
  geom_sf(data = eastern_lake_o) +
  geom_sf(data = rec_loc_filter, aes(colour = glatos_array))



mapview::mapview(rec_loc_filter)


glimpse(det)



# det_sf <- det %>%
#   st_as_sf(coords = c("x", "y"),
#            crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")

# mapview::mapview(det_sf)
# ---- spatial modell ----

fitdistrplus::descdist(det$n_det, discrete = TRUE)


ggplot(data = det, aes(x = n_det)) +
  geom_histogram()


fit_nb <- fitdistrplus::fitdist(det$n_det, discrete = TRUE, distr = "nbinom")

plot(fit_nb)

summary(det$n_det)

det %>%
  group_by(n_det) %>%
  summarise(
    n()
  ) %>%
  ungroup() %>%
  print(n = 39)

m <- bam(n_det ~ month_abb +
           # "ad", "gp", or "tp" or "so"
           s(x, y, bs = "tp", by = month_abb),
         family = nb(theta = 6),
         data = det,
         method = "fREML")

# warnings()
appraise(m)
# draw(m)

plot(eastern_lake_o)

# ---- predict ----

lake_pred <- eastern_lake_o %>%
  st_transform(crs = 32618) %>%
  st_make_grid(cellsize = 1000, square = TRUE, what = "centers") %>%
  st_as_sf()

st_geometry(lake_pred) <- "geometry"

lake_pred <- st_intersection(lake_pred, eastern_lake_o %>%
                               st_transform(crs = 32618)) %>%
  dplyr::select(geometry)


lake_pred <- lake_pred %>%
  st_transform(crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs")


lake_pred_df <- lake_pred %>%
  mutate(
    x = st_coordinates(.)[,"X"],
    y = st_coordinates(.)[,"Y"],
  ) %>%
  st_drop_geometry()


lake_pred_df <- expand_grid(
  lake_pred_df,
  month_abb = factor(month.abb,
                     levels = month.abb)
)

lake_pred_df
levels(lake_pred_df$month_abb)
summary(det$n_det)
fv1 <- fitted_values(m, data = lake_pred_df, scale = "response") %>%
  mutate(
    .fitted = .fitted
  ) %>%
  filter(.fitted > 0 & .fitted < 39) %>%
  st_as_sf(
    coords = c("x", "y"),
    crs = "+proj=merc +lon_0=0 +datum=WGS84 +units=km +no_defs"
  ) %>%
  st_transform(
    crs = 32618
  ) %>%
    mutate(
      x = st_coordinates(.)[,"X"],
      y = st_coordinates(.)[,"Y"],
    ) %>%
    st_drop_geometry()


eastern_lake_o_utm <- eastern_lake_o %>%
  st_transform(crs = 32618)

# ggplot() +
#   geom_sf(data = eastern_lake_o, fill = NA, colour = "black") +
#   geom_sf(data = fv1, aes(color = .fitted)) +
#   facet_wrap(~month_abb)


ggplot() +
  geom_tile(data = fv1, aes(x = x, y = y, fill = .fitted)) +
  geom_sf(data = eastern_lake_o_utm, fill = NA, colour = "black") +
  scale_fill_viridis_c(name = "n_detect", option = "H") +
  facet_wrap(~month_abb)
  # theme_void(
  #   base_size = 15
  # ) +
  # theme(
  #   legend.background = element_blank(),
  #   legend.position = c(0.98, 0.82),
  # ) +
  # guides(fill = guide_colourbar(
  #   frame.linewidth = 0.3,
  #   ticks.colour = 'black',
  #   frame.colour = 'black')) +
  # labs(x = "Longitude",
  #      y = "Latitude")
