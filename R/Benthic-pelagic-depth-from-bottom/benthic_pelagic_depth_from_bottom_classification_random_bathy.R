
# ---- Load packages ----
{
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(ggtext)
  library(here)
  library(janitor)
  library(lubridate)
  library(purrr)
  library(patchwork)
  library(qs)
  library(stringr)
  library(tidyr)

}

# ---- bring in data -----

det <- qread(here:::here(
  "data-saved",
  "det-benthic-suspended",
  "detection_benthic_suspended_list_lwf_random.qs"))


glimpse(det)


# ---- remove columns -----

det[, c("tag_id", "bathy_limit",
        "benthic_susp", "glatos_array",
        "station_no") := NULL]

det

# to GB
object.size(det) / 1e+9

# ---- add from_bottom ----

det[, from_bottom := (bathy_raster - converted_sensor)]

det[, c("doy", "month_no",
        "month_abb") := list(yday(detection_timestamp_est),
                             month(detection_timestamp_est),
                             month(detection_timestamp_est, label = TRUE,
                                   abbr = TRUE)),][]



dt_m <- melt(det, measure.vars = c("converted_sensor",
                                   "bathy_raster", "from_bottom"),
             variable.name = "type", value.name = "depth")

dt_m
glimpse(dt_m)


# ---- set key ----




rm(det)
gc()

dt_ms <- dt_m %>%
  filter(type %in% c("converted_sensor", "bathy_raster"))

# rm(dt_m)
gc()


p <- ggplot() +
  geom_boxplot(data = dt_ms,
               aes(x = month_abb, y = depth,
                   fill = type)) +
  scale_y_reverse() +
  scale_fill_brewer(type = "div",
                    name = "Classification", direction = -1,
                    labels = c(
                      "Fish Depth",
                      "Bottom Depth")
  ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.15)
  ) +
  labs(
    x = "Month",
    y = "Depth (m)"
  )

rm(dt_ms)
gc()
dt_fb <- dt_m %>%
  filter(type %in% c("from_bottom"))

rm(dt_m)
gc()


p1 <- ggplot(data = dt_fb, aes(x = month_abb,
                       y = depth,
                       fill = type)) +
  geom_boxplot(width = 0.35) +
  geom_hline(yintercept = 5, linetype = 3) +
  geom_hline(yintercept = 0) +
  # scale_y_reverse() +
  scale_fill_manual(
    values = c('#5ab4ac'),
    # type = "div",
    name = "Classification",
    # direction = -1,
    labels = c(
      "Depth From Bottom")
    #            "Bottom Depth")
  ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.background = element_blank(),
    legend.position.inside = c(0.935, 0.87)
  ) +
  labs(
    x = "Month",
    y = "Depth (m)"
  )

p3 <- p / p1


ggsave(here("plots",
            "benthic-plots",
            "depth_use_bottom_depth_random_bathy.png"),
       width = 16, height = 9, plot = p3)

