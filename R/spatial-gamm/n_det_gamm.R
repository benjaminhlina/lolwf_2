
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
det





det_summary <- det %>%
  mutate(
    doy = yday(detection_timestamp_est),
    month = month(detection_timestamp_est),
    year = year(detection_timestamp_est)
  ) %>%
  group_by(
    tag_serial_number, doy, month, year
  ) %>%
  summarise(
    n_det = n(),
    mean_lon = mean(deploy_long),
    mean_lat = mean(deploy_lat)
  ) %>%
  ungroup()

det_summary %>%
  st_as_sf(coords = c("mean_lon",
                      "mean_lat"),
           crs = 4326) %>%
  ggplot() +
  geom_sf(aes(size = n_det)) +
  facet_wrap(~ month)


library(gratia)
library(mgcv)


m <- bam(n_det ~
           s(doy, bs = "cr", k = 13) +
           s(mean_lon, mean_lat),
         family = poisson(),
         method = "fREML",
         data = det_summary
         )

appraise(m)

draw(m)


ds1 <- data_slice(m,
                  doy = evenly(doy, n = 50),
                  mean_lat = evenly(mean_lat),
                  # mean_lon = evenly(mean_lon),
                  )




ds1

fv1 <- fitted_values(m, data = ds1, scale = "response")
fv1



ggplot(data = fv1, aes(x = mean_lon, y = mean_lat,
                       size = .fitted)) +
  geom_point() +
  facet_wrap(~doy)

ggplot(data = fv1, aes(x = mean_lon, y = mean_lat,
                       fill = .fitted)) +
  geom_tile() +
  facet_wrap(~doy) +
  scale_fill_viridis_c()

