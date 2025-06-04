## ----load libraries----------------------------------------------------------------------------------------------------------------------------------------------

{
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(here)
  library(raster)
  library(sf)
  library(terra)
  library(tidyterra)
  library(rasterVis)
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


# add in bathymetry raster
bathy <- rast(here("data-saved",
                   "east-lake-ontario-bathymetry",
                   "eastern_lake_ontario_bathymetry.tif"))

bathy


# ---- bring in buffer rings ----
buffer_rings <- st_read(here::here("data-saved",
                                   "rec-range-shp",
                                   "rec_range_buffer.shp")) %>%
  st_transform(crs = 32618)

buffer_rings

buffer_rings_pts <- st_read(here::here("data-saved",
                                       "rec-range-shp",
                                       "rec_range_buffer_pts.shp"))




# crop and mask

buffer_vct <- vect(buffer_rings)
buffer_vct

cm <- crop(bathy, buffer_vct, mask = TRUE)

# ---- [plot] ----
ggplot() +
  geom_spatraster(data = cm) +
  scale_fill_whitebox_c(
    palette = "muted",
    n.breaks = 20,
    name = "Depth (m)",
    guide = guide_legend(reverse = FALSE)
  )



glimpse(cm$bathy_raster)
buffer_pts <- vect(buffer_rings_pts)
buffer_pts

test <- terra::extract(cm, y = buffer_pts, xy = TRUE, bind = TRUE)
test


test_pt <- as(test, "Spatial") %>%
  st_as_sf() %>%
  st_drop_geometry()

test_pt

qs::qsave(test_pt, here("data-saved",
                        "rec-range-depth",
                        "rec_range_depth.qs"))

test_pt_sf <- as(test, "Spatial") %>%
  st_as_sf(crs = 32618) %>%
  st_transform(crs = 4326) %>%
  st_transform(crs = 32618)

mapview::mapview(test_pt_sf, zcol = "bathy_raster")


p <- ggplot() +
  geom_sf(data = eastern_lake_o, fill = NA, colour = "black") +
  geom_sf(data = test_pt_sf, aes(fill = bathy_raster),
          shape = 21,
          # colour = NA
  ) +
  scale_fill_whitebox_c(
    palette = "muted",
    n.breaks = 20,
    name = "Depth (m)",
    guide = guide_legend(reverse = FALSE)
  ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )
p

p1 <- ggplot() +
  geom_sf(data = eastern_lake_o, fill = NA, colour = "black") +
  geom_spatraster(data = cm) +
  scale_fill_whitebox_c(
    palette = "muted",
    n.breaks = 20,
    name = "Depth (m)",
    # na.value = "white",
    guide = guide_legend(reverse = FALSE)
  ) +
  annotation_north_arrow(location = "tl") +
  annotation_scale(location = "br") +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )
p1

ggsave(filename = here::here("plots",
                             "bathy-rec-ring",
                             "bathy_rec_ring_eastern_lake_o.png"),
       plot = p1,
       width = 10, height = 8)
