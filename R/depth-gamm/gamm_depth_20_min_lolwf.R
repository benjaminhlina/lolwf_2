  # ---- Load packages ----
  {
    library(dplyr)
    library(fitdistrplus)
    library(ggplot2)
    library(gratia)
    library(here)
    library(janitor)
    library(lubridate)
    library(mgcv)
    library(purrr)
    library(qs)
    library(stringr)
    library(tidyr)
    library(sf)
  }

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


  lake_o_east <- lake_o %>%
    st_crop(lake_o_e) %>%
    st_transform(crs = 32618)


  plot(lake_o_east)

  # ---- bring in data -----

  det <- qread(here("data-saved",
                    "coa",
                    "coa_20_min.qs"))


  glimpse(det)
  det

  unique(det$sensor_type)

  # ---- filter out depths ----
  det_depth <- det %>%
    filter(sensor_type == "P") %>%
    mutate(
      time_bins_no = as.numeric(time_bins),
      doy = yday(time_bins),
      year = factor(year(time_bins)),
      tag_serial_number = factor(tag_serial_number)
    )


  det_depth %>%
    distinct(common_name_e)
  glimpse(det_depth)


  ggplot(data = det_depth, aes(x = time_bins, y = mean_sensor,
                               colour = tag_serial_number)) +
    geom_point(alpha = 0.4) +
    scale_y_reverse()


fitdistrplus::descdist(data = sqrt(det_depth$n_det),
                       discrete = TRUE)


# m <- bam((n_det) ~
#            # s(mean_sensor, bs = "cr", k = 10) +
#            s(x, y, bs = "tp") +
#            te(doy, mean_sensor,
#               bs = "cc", k = 12) +
#            s(tag_serial_number, bs = "re") +
#            s(year, bs = "re"),
#          method = "fREML",
#          family = poisson(link = "log"),
#          data = det_depth,
#          discrete = TRUE,
#          select = TRUE
# )
#
#
# appraise(m)
#
# draw(m)
#
#
# ds1 <- data_slice(m,
#                   x = evenly(x, n = 100),
#                   y = evenly(y, n = 100),
#                   doy = evenly(doy, n = 100),
#                   mean_sensor = evenly(mean_sensor, n = 100))
#
#
#
# fit <-
# ds1 %>%
#   distinct(doy)
#
#




# ---- look at distr ----
# should be gamma

ggplot(data = det_depth,
       aes(x = mean_sensor)) +
  geom_histogram()

(descdist(det_depth$mean_sensor))

fit_dis <- fitdist(det_depth$mean_sensor, method = "mme", distr = "gamma")
plot(fit_dis)


# ---- run the model ----
glimpse(det_depth)
det_depth <- det_depth %>%
  mutate(
    tag_serial_number = factor(tag_serial_number),
    year = factor(year),
  )
# filter(
#   year == "2022"
# )

m <- bam(mean_sensor ~ s(doy,
                         bs = "cc", k = 20),
           # s(tag_serial_number, bs = "re") +
         # s(year, bs = "re"),
         family = Gamma(),
         data = det_depth,
         select = TRUE,
         # discrete = TRUE,
         method = "fREML"
)

# gam.check(m)
appraise(m)

draw(m)


# ---- predict ----
# create new datafreame with dummmy variables for RE for plotting
dat_2 <- det_depth %>%
  mutate(
    tag_serial_number = "a",
    year = 0
  )

glimpse(dat_2)

# use prediction to get interpolated points
fits <- predict.gam(m, newdata = dat_2,
                    # type = "response",
                    se = TRUE, discrete = TRUE,
                    exclude = c("s(tag_serial_number)",
                                "s(year)"
                    ),
                    # newdata.guaranteed = TRUE
)



# combine fits with dataframe for plotting and calc upper and lower
# add in month abb for plotting
predicts <- data.frame(dat_2, fits) %>%
  as_tibble() %>%
  mutate(
    # lower = (fit - 1.96 * se.fit),
    #
    # upper = (fit + 1.96 * se.fit),
    lower = (1 / (fit - 1.96 * se.fit)),

    upper = (1 / (fit + 1.96 * se.fit)),
    fit = (1 / fit),
    # lower = exp(1) ^ (fit - 1.96 * se.fit),
    #
    # upper = exp(1) ^ (fit + 1.96 * se.fit),
    # fit = exp(1) ^ fit,
    # month_abb = month(time_bin, label = TRUE, abbr = TRUE),
    # month_abb = factor(month_abb,
    #                    levels = c("May", "Jun", "Jul",
    #                               "Aug", "Sep", "Oct",
    #                               "Nov", "Dec", "Jan",
    #                               "Feb", "Mar", "Apr"))
  ) %>%
  arrange(tag_serial_number, doy)

glimpse(det_depth)
depth_means <- det_depth %>%
  mutate(
    date = floor_date(time_bins, unit = "1 day")
  ) %>%
  group_by(time_bins, date, doy) %>%
  summarise(
    mean_depth = mean(mean_sensor),
  ) %>%
  ungroup() %>%
  arrange(doy)

glimpse(predicts)
# ----- plot ----
# det_depths <- det_depth %>%
#   group_by(doy) %>%
#   summarise(
#     mean_depth = mean(mean_sensor)
#   ) %>%
#   ungroup()


ggplot() +
  stat_summary(data = det_depth, aes(x = doy,
                                     y = mean_sensor),
               fun = mean, geom = "point",
               shape = 21, alpha = 0.5) +
  geom_ribbon(data = predicts, aes(x = doy, y = fit, ymin = lower,
                                   ymax = upper),
              alpha = 0.2) +
  geom_line(data = predicts, aes(x = doy, y = fit)) +

  scale_y_reverse()




glimpse(det_depth)
# ---- next model ----
m3 <- bam(mean_sensor ~
            s(doy,
            bs = "cc", k = 20) +
            s(x, y, bs = "tp", k = 10) +
           s(tag_serial_number, bs = "re"),
           # s(year, bs = "re"),
         family = Gamma(),
         data = det_depth,
         select = TRUE,
         # discrete = TRUE,
         method = "fREML"
)

appraise(m3)


draw(m3)


# create new datafreame with dummmy variables for RE for plotting
dat_3 <- det_depth %>%
  mutate(
    tag_serial_number = "a",
    year = 0
  )

glimpse(dat_3)

plot(lake_o_e)

lake_pred <- lake_o_east %>%
  st_make_grid(cellsize = 15000, square = TRUE, what = "centers") %>%
  st_as_sf()
st_geometry(lake_pred) <- "geometry"

lake_pred <- st_intersection(lake_pred, lake_o_east) %>%
  dplyr::select(geometry) %>%
  st_transform(crs = 4326)

lake_pred_df <- lake_pred %>%
  mutate(
    x = st_coordinates(.)[,"X"],
    y = st_coordinates(.)[,"Y"],
  ) %>%
  st_drop_geometry()
  # mutate(
  #   tag_serial_number = "a",
  #   year = 0
  # )


lake_pred_df <- expand_grid(
  lake_pred_df,
  tibble(
    doy = seq(1, 365, 36.5)
  )
  ) %>%
  mutate(
    tag_serial_number = "a",
#     year = 0
  )

lake_pred_df
pred <- broom::augment(m3, newdata = lake_pred_df,
                discrete = TRUE,
                exclude = c("s(tag_serial_number)"
                            # "s(year)")
                ))
# pred <- broom::augment(m3, newdata = lake_pred_df,
#                 discrete = TRUE,
#                 exclude = c("s(tag_serial_number)",
#                             "s(year)"))
pred <- pred %>%
  mutate(
    lower = 1 / (.fitted - 1.96 * .se.fit),
    higher = 1 / (.fitted + 1.96 * .se.fit),
    .fitted = 1 / (.fitted)
  ) %>%
  st_as_sf(coords = c("x", "y"),
           crs = 4326) %>%
  st_transform(crs = 32618) %>%
  mutate(
    x = st_coordinates(.)[,"X"],
    y = st_coordinates(.)[,"Y"],
  ) %>%
  st_drop_geometry() %>%
  filter(between(.fitted, 0, 100))


summary(pred$.fitted)


ggplot() +
  geom_tile(data = pred, aes(x = x, y = y, fill = .fitted)) +
  geom_sf(data = lake_o_east, fill = NA, colour = "black") +
  scale_fill_viridis_c(name = "Depth (m)",
                       trans = "reverse",
                       # breaks = rev(seq(0, 60, 15))
  ) +
  theme_void(
    base_size = 15
  ) +
  facet_wrap(~ doy) +
  theme(
    # legend.background = element_blank(),
    # legend.position = c(0.98, 0.82),
  ) +
  # guides(fill = guide_colourbar(
  #   frame.linewidth = 0.3,
  #   ticks.colour = 'black',
  #   frame.colour = 'black')) +
  labs(x = "Longitude",
       y = "Latitude")
#> Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
#> 3.5.0.
#> ℹ Please use the `legend.position.inside` argument of `theme()` instead.






ds1 <- data_slice(m3,
                  x = evenly(x, n = 100),
                  y = evenly(y, n = 100),
                  doy = evenly(doy, n = 100))







fv1 <- fitted_values(m3, data = ds1, scale = "response",
                     exclude = c("s(tag_serial_number)",
                                           "s(year)"))
fv1

ggplot() +
  geom_raster(data = fv1, aes(x = x, y = y, fill = .fitted)) +
  # geom_sf(data = lake_o_east, fill = NA, colour = "black") +
  # scale_fill_viridis_c(name = "Depth (m)",
  #                      trans = "reverse",
  #                      breaks = rev(seq(0, 60, 15))
  # ) +
  # theme_void(
  #   base_size = 15
  # ) +
  facet_wrap(~ doy)




# use prediction to get interpolated points
fits <- predict.gam(m3, newdata = lake_pred_df,
                    # type = "response",
                    se = TRUE, discrete = TRUE,
                    exclude = c("s(tag_serial_number)",
                                "s(year)"
                    ),
                    # newdata.guaranteed = TRUE
)


predicts_update <- data.frame(dat_2, fits) %>%
  as_tibble() %>%
  mutate(
    # lower = (fit - 1.96 * se.fit),
    #
    # upper = (fit + 1.96 * se.fit),
    lower = (1 / (fit - 1.96 * se.fit)),

    upper = (1 / (fit + 1.96 * se.fit)),
    fit = (1 / fit),
    # lower = exp(1) ^ (fit - 1.96 * se.fit),
    #
    # upper = exp(1) ^ (fit + 1.96 * se.fit),
    # fit = exp(1) ^ fit,
    # month_abb = month(time_bin, label = TRUE, abbr = TRUE),
    # month_abb = factor(month_abb,
    #                    levels = c("May", "Jun", "Jul",
    #                               "Aug", "Sep", "Oct",
    #                               "Nov", "Dec", "Jan",
    #                               "Feb", "Mar", "Apr"))
  ) %>%
  arrange(tag_serial_number, doy)

glimpse(predicts_update)



