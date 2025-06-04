
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


det_select <- det %>%
  dplyr::select(tag_serial_number, detection_timestamp_est, station,
                deploy_long, deploy_lat,
                converted_sensor, units, mean_bottom_depth)

det_select_d <- det_select %>%
  filter(units == "m")



det_select_d <- det_select_d %>%
  mutate(
    bathy_limit = case_when(
      mean_bottom_depth < 5 ~ mean_bottom_depth,
      mean_bottom_depth > 5 ~ mean_bottom_depth - 5,
    ),
    benthic_susp = case_when(
      converted_sensor <= bathy_limit ~ "pelagic",
      converted_sensor >= bathy_limit ~ "benthic",
    ),
    doy = yday(detection_timestamp_est),
    month_abb = lubridate::month(detection_timestamp_est, abbr = TRUE,
                                 label = TRUE),
    month_no = lubridate::month(detection_timestamp_est)
  ) %>%
  glimpse() %>%
  filter(!(is.na(benthic_susp)))

det_select_d


det_bs <- det_select_d %>%
  mutate(
    date = floor_date(detection_timestamp_est, "1 day")
  ) %>%
  group_by(tag_serial_number) %>%
  mutate(
    n_date = n_distinct(date)
  ) %>%
  ungroup()

det_bs %>%
  distinct(tag_serial_number, n_date) %>%
  arrange(n_date) %>%
  print(n = 46)


det_bs_1 <- det_bs %>%
  filter(n_date > 36)


det_bs_1 %>%
  distinct(tag_serial_number, n_date) %>%
  arrange(n_date) %>%
  print(n = 46)


det_sel <- det_bs_1 %>%
  group_by(tag_serial_number, doy, month_abb, benthic_susp) %>%
  summarise(
    n_b = n(),
  ) %>%
  ungroup() %>%
# mutate(
#     perc = n_b / n_tot
#   )  %>%
  print(n = 25)


doy_select <- det_bs_1 %>%
  distinct(doy, month_abb, month_no)

doy_select
month_doy <- doy_select %>%
  group_by(month_abb) %>%
  summarise(first = first(doy),
            last = last(doy)) %>%
  ungroup() %>%
  # mutate(
  #   month_abb = forcats::fct_relevel(month_abb, "Jan",
  #                                    "Feb", "Mar", "Apr", "May", "Jun",
  #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # ) %>%
  # arrange(month_abb) %>%
  # mutate(
  #   first = if_else(
  #     month_abb %in% "May", true = 123, false = first
  #   )
  # ) %>%
  .$first
month_doy <- if_else(month_doy == 322, true = 307,
                     false = month_doy)
doy_select %>%
  filter(doy %in% month_doy) %>%
  group_by(month_abb) %>%
  summarise() %>%
  # mutate(
  #   month_abb = forcats::fct_relevel(month_abb, "Jan",
  #                                    "Feb", "Mar", "Apr", "May", "Jun",
  #                                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # ) %>%
  # arrange(month_abb) %>%
  .$month_abb -> month_label
month_label




ggplot(data = det_sel, aes(x = month_abb, y = n_b)) +
  geom_boxplot(aes(fill = benthic_susp))

det_select_d

ggplot(data = det_sel,
       aes(x = doy, y = n_b,
           fill = benthic_susp)) +
  geom_point(shape = 21, stroke = 0.1, colour = "black",
             size = 3,
             # position = position_jitter(width = 0.2)
             ) +
  facet_wrap(~ tag_serial_number) +
  scale_fill_viridis_d(option = "A",
                       begin = 0.1,
                       end = 0.9, alpha = 0.5,
                       direction = -1,
                       name = "Behaviour",
                       labels = c("Benthic",
                                  "Suspended")) +
  scale_x_continuous(breaks = month_doy,
                     label = month_label) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.035, 0.965),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Date",
    y = "Percentage"
  ) -> p2
p2






ggsave(here("plots",
            "benthic-raw",
            "benthic_pelagic_raw_depth_rec.png"),
       width = 11 * 2, height = 8.5 * 2, plot = p2)

p3 <- ggplot(data = det_sel, aes(x = month_abb, y = n_b)) +
  geom_boxplot(aes(fill = benthic_susp)) +
  # geom_hline(yintercept = 0.5, linetype = 3) +
  scale_fill_viridis_d(option = "A",
                       begin = 0.1,
                       end = 0.9, alpha = 0.5,
                       direction = -1,
                       name = "Behaviour",
                       labels = c("Benthic",
                                  "Pelagic")) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    # legend.position = "inside",
    # legend.position.inside = c(0.85, 0.05),
    # axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Month",
    y = "Number of Occurances"

  )


ggsave(here("plots",
            "benthic-raw",
            "benthic_pelagic_raw_depth_rec.png"),
       width = 11 * 2, height = 8.5 * 2, plot = p2)
