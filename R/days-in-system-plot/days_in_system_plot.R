# ---- Load packages ----
{
  library(dplyr)
  library(ggplot2)
  library(glatos)
  library(ggtext)
  library(here)
  library(janitor)
  library(lubridate)
  library(purrr)
  library(qs)
  library(stringr)
  library(tidyr)

}

# ---- bring in data -----

det <- qread(here("data-saved",
                  "detection-data",
                  "removed-dead-dets",
                  "final_cleaned_detection_data.qs"))

glimpse(det)
det <- det %>%
  mutate(
    tag_activation_date = mdy(tag_activation_date),
    month_year = format_ISO8601(tag_activation_date, precision = "ym")
  )
glimpse(det)


det %>%
  distinct(tag_model)

# ---- cretea summary of how long in array ----
sum_det <- det %>%
  group_by(tag_serial_number, triskit_tag_no,
           tag_type, tag_family, tag_model, month_year) %>%
  arrange(tag_serial_number, detection_timestamp_est) %>%
  summarise(
    first_heard = first(detection_timestamp_est),
    last_heard = last(detection_timestamp_est)
  ) %>%
  mutate(
    days_heard = round(difftime(last_heard, first_heard, units = "days"), 1)
  ) %>%
  ungroup() %>%
  pivot_longer(cols = -c(tag_serial_number, triskit_tag_no,
                         tag_type, tag_family,
                         tag_model, month_year, days_heard),
               names_to = "detected",
               values_to = "time_stamp") %>%
  mutate(
    month_year = factor(month_year, level = c("2022-11", "2021-11")),
    trisket_present = factor(if_else(!is.na(triskit_tag_no), true = "Yes",
                                     false = "No"), level = c("Yes", "No")
    ),
    tag_vemco = case_when(
      tag_model %in% c("V16P-4x", "V16P-4x-BLU-1-0136m") ~ "V16P",
      tag_model %in% c("V16-4x", "V16-4x-BLU-1") ~ "V16",
      tag_model %in% "V16-TP-4x" ~ "V16TP",
      tag_model %in% "V13P-1x" ~ "V13P",
    ),
    tag_info = case_when(
      tag_model %in% "V16P-4x" & trisket_present == "Yes" ~ paste(tag_vemco, "Trisket", sep = " "),
      tag_model %in% "V16P-4x" & trisket_present == "No" ~ tag_vemco,
      tag_model %in% "V16-4x" & trisket_present == "No" ~ tag_vemco,
      tag_model %in% "V16-4x" & trisket_present == "Yes" ~ paste(tag_vemco, "Trisket", sep = " "),
      tag_model %in% "V16-4x-BLU-1" & trisket_present == "No" ~ tag_vemco,
      tag_model %in% "V16-TP-4x" & trisket_present == "Yes" ~ paste(tag_vemco, "Trisket", sep = " "),
      tag_model %in% "V13P-1x" & trisket_present == "Yes" ~ paste(tag_vemco, "Trisket", sep = " "),
      tag_model %in% "V16P-4x-BLU-1-0136m" & trisket_present == "No" ~ tag_vemco,
    ),
    tag_info = factor(tag_info, label = c("V13P Trisket", "V16",
                                          "V16 Trisket",
                                          "V16P",
                                          "V16P Trisket", "V16TP Trisket")
    )
  ) %>%
  arrange(month_year, -days_heard) %>%

  mutate(
    tag_serial_number = factor(tag_serial_number,
                               levels = unique(tag_serial_number)),
    tag_serial_number_update = factor(tag_serial_number,
                                      levels =
                                        unique(
                                          tag_serial_number[order(
                                            month_year,
                                            forcats::fct_rev(tag_info))]),
                                      ordered = TRUE)
  )


glimpse(sum_det)
# mutate(
#   tag_label = detected %>%
#     stringr::str_replace(pattern = "_", replacement = " ") %>%
#     paste(tag_info, sep = " "),
#   tag_label = factor(tag_vemco, level = c("V13P-1x Trisket", "V16-4x",
#                                           "V16-4x Trisket",
#                                           "V16-4x-BLU-1",
#                                           "V16P-4x",
#                                           "V16P-4x Trisket",
#                                           "V16P-4x-BLU-1-0136m",
#                                           "V16-TP-4x Trisket",
#   ))
# )

sum_det
sum_det %>%
  distinct(tag_info) %>%
  arrange(tag_info)
# dput(unique(sum_det$tag_info))
# ----- plot ----

highlight_dates <- data.frame(
  start_date = as.POSIXct(c("2023-04-01 00:00:00")),
  end_date = as.POSIXct(c("2023-11-01 00:00:00"))
  # label = c("Period 1", "Period 2")
)

# Create the plot


p <- ggplot() +

  geom_line(data = sum_det, aes(x = time_stamp, y = tag_serial_number),
            colour = "grey70") +
  geom_rect(data = highlight_dates, aes(xmin = start_date,
                                        xmax = end_date,
                                        ymin = -Inf, ymax = Inf),
            fill = "orange1", alpha = 0.2,
            colour = "black") +
  geom_point(data = sum_det, aes(x = time_stamp, y = tag_serial_number,
                                 fill = detected),
             size = 2,
             shape = 21) +
  scale_fill_viridis_d(
    begin = 0.3, end = 0.7,
    option = "A",
    name = "Tag Type",
    label = c("First detected",
              "Last detected")
  ) +
  # scale_shape_manual(values = c(21, 22), name = "Detection",
  #                    label = c("First detected",
  #                              "Last detected")
  # ) +
  guides(fill = guide_legend(override.aes = list(shape = 21,
                                                 colour = "black",
                                                 stroke = 0.8,
                                                 size = 2.5)),
         shape = guide_legend(override.aes = list(
           size = 2.5
         ))) +
  theme_bw(base_size = 15) +
  theme(
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    # panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.93),
  ) +
  labs(
    x = "Date",
    y = "Fish ID"
  )

p

ggsave(here::here("plots",
                  "days-in-system",
                  "first_and_last_heard.png"),
       plot = p, width = 11, height = 8.5)
#
p1 <- ggplot(data = sum_det, aes(x = time_stamp, y = tag_serial_number_update)) +
  geom_line(colour = "grey70") +
  geom_point(size = 3, aes(fill = tag_info, shape = detected)) +
  scale_fill_viridis_d(
    option = "A",
    name = "Tag Type",
    # label = c("First detected",
    # "Last detected")
  ) +
  scale_shape_manual(values = c(21, 22), name = "Detection",
                     label = c("First detected",
                               "Last detected")
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21,
                                                 colour = "black",
                                                 stroke = 0.8,
                                                 size = 3.5)),
         shape = guide_legend(override.aes = list(
           size = 3.5
         ))) +
  theme_bw(base_size = 15) +
  theme(
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    # panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.25),
  ) +
  labs(
    x = "Date",
    y = "Fish ID"
  )

p1

ggsave(here::here("plots",
                  "days-in-system",
                  "first_and_last_heard_update.png"),
       plot = p1, width = 11, height = 8.5)
#
#
#
#
#
#
#
# unique(det$tag_model)
#
#
# det %>%
#   group_by(tag_serial_number) %>%
#   filter(date <= stop_date) %>%
#   ungroup() %>%
#   filter(tag_serial_number == "1304911") %>%
#   ggplot() +
#   geom_point(aes(x = detection_timestamp_est, y = station,
#                  colour = converted_sensor), size = 3)
#
#
# glimpse(tag_911)

