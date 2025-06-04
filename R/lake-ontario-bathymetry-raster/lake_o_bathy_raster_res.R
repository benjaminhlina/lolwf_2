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



# add in bathymetry raster
bathy <- raster(here("BenthicModShare",
                     "bathy_raster.tif"))
# crop by eastern lake o
bathy <- crop(bathy, extent(-77.7, 43.3, -76.1, 44.3), mask = TRUE)



# keep as default raster variable names
bathy_raster <- bathy

rm(bathy)

plot(bathy_raster)


# ---- convert to UTMs ----
project_str <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs"

utm_bathy <- projectRaster(bathy_raster, crs = project_str)
utm_bathy

plot(utm_bathy)
proj4string(utm_bathy)

# ----- increase resoultion ----
utm_bathy_res <- raster(resolution = c(50, 50),
             crs = proj4string(utm_bathy),
             ext = extent(utm_bathy)
             )
res(utm_bathy_res) # 5 by 5

# ---- resample ----
utm_bathy_hr <- resample(utm_bathy, utm_bathy_res)

utm_bathy_hr

# ---- create to spatraster ----

utm_bathy_hr_spr <- rast(utm_bathy_hr)


# ---- plot ----


ggplot() +
  geom_spatraster(data = utm_bathy_hr_spr) +
  scale_fill_whitebox_c(
    palette = "muted",
    n.breaks = 20,
    name = "Depth (m)",
    guide = guide_legend(reverse = FALSE)
  )


# ---- save spatraster ----
#

writeRaster(utm_bathy_hr_spr,
            filename = here("data-saved",
                            "east-lake-ontario-bathymetry",
                            "eastern_lake_ontario_bathymetry.tif"))
