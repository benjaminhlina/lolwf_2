
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
  library(patchwork)
  library(qs)
  library(stringr)
  library(sf)
  library(tidyr)

}

# ---- bring in letters ----

sig_letter <- readr::read_csv(here("results",
                                   "off-bottom",
                                   "off_bottom_sig_letters.csv")) %>%
  pivot_longer(cols = -measurement,
               names_to = "month_abb",
               values_to = "letters")

sig_letter

sig_letters_cld <- readr::read_csv(here::here("results",
                                              "off-bottom",
                                              "cld.csv"))


sig_letters_cld <- sig_letters_cld %>%
  mutate(
    .group_fix = case_when(
      .group == "b" ~ "a",
      .group == "c" ~ "b",
      .group == "a" ~ "c",
      .group == "f" ~ "d",
      .group == "d" ~ "e",
      .group == "de" ~ "ef",
      .group == "e" ~ "f",


      # .group == "bc" ~ "ab",
      # .group == "d" ~ "c",
      # .group == "b" ~ "a",
      # .group == "e" ~ "f",
      # .group == "f" ~ "e",
      # .group == "cd" ~ "bc",
      # .group == "a" ~ "d"

    )
  )


sig_letters_cld
sig_letters_cld_d <- readr::read_csv(here::here("results",
                                              "depth-use",
                                              "cld.csv"))


sig_letters_cld_d <- sig_letters_cld_d %>%
    mutate(
      .group = factor(.group),
      .group_fixed = case_when(
        .group == "e" ~ "a",
        .group == "g" ~ "b",
        .group == "h" ~ "c",
        .group == "c" ~ "e",
        .group == "ef" ~ "af",
        .group == "cd" ~ "ed",
        .group == "a" ~ "g",
        .group == "b" ~ "h",
        .default = .group
      )
    )
sig_letters_cld_d


# ---- bring in data -----
t <- qread(here("data-saved",
                "det-benthic-suspended",
                "detection_benthic_suspended_summary_lwf.qs"))
t
glimpse(t)



det_select <- t %>%
  dplyr::select(tag_serial_number, detection_timestamp_est, station,
                deploy_long, deploy_lat,
                converted_sensor, units, mean_bathy_depth)

det_select_d <- det_select %>%
  filter(units == "m")



det_select_d <- det_select_d %>%
  mutate(
    bathy_limit = case_when(
      mean_bathy_depth < 5 ~ mean_bathy_depth,
      mean_bathy_depth > 5 ~ mean_bathy_depth - 5,
    ),
    benthic_susp = case_when(
      converted_sensor <= bathy_limit ~ "pelagic",
      converted_sensor >= bathy_limit ~ "benthic",
    ),
    doy = yday(detection_timestamp_est),
    month_abb = lubridate::month(detection_timestamp_est, abbr = TRUE,
                                 label = TRUE),
    month_no = lubridate::month(detection_timestamp_est),
    hour = hour(detection_timestamp_est)
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

det_bs_1 <- det_bs_1 %>%
  mutate(
    from_bottom = mean_bathy_depth - converted_sensor
  )

det_1_flip <- det_bs_1 %>%
  mutate(
    year = year(detection_timestamp_est)
  ) %>%
  dplyr::select(-c(benthic_susp)) %>%
  pivot_longer(cols = -c("tag_serial_number", "detection_timestamp_est", "date", "year",
                         "n_date",
                        "hour", "doy",
                         "month_no", "month_abb", "station",
                         "deploy_long",
                         "deploy_lat", "units"),
               names_to = "measurment",
               values_to = "depth")

det_1_flip





det_summary <- det_1_flip %>%
  group_by(
    tag_serial_number, hour, doy, month_abb, year,
    measurment
  ) %>%
  summarise(
    n_dec = n(),
    n_rec = n_distinct(station),
    mean_use = round(mean(depth), 2),
    sd = round(sd(depth), 2),
    sem = round((sd(depth) / sqrt(n())), 2)
  ) %>%
  ungroup() %>%
  print()





p <- det_summary %>%
  filter(!(measurment %in% c("bathy_limit"
                             , "from_bottom"
  ))) %>%
  ggplot(data = ., aes(x = month_abb,
                       y = mean_use,
                       fill = measurment)) +
  geom_boxplot() +
  # geom_hline(yintercept = 5) +
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

p1 <- det_summary %>%
  filter((measurment %in% c(
    # "bathy_limit"
    "from_bottom"
  ))) %>%
  ggplot(data = ., aes(x = month_abb,
                       y = mean_use,
                       fill = measurment)) +
  geom_boxplot(width = 0.35) +
  geom_hline(yintercept = 5) +
  geom_hline(yintercept = 0, linetype = 3) +
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
# plot_layout(guides = "collect")
p3

ggsave(here("plots",
            "benthic-plots",
            "depth_use_bottom_depth_.png"),
       width = 16, height = 9, plot = p3)




# ----- plot violin ----


dt_sums <- det_summary %>%
  filter(!(measurment %in% c("bathy_limit"
                             , "from_bottom"
  ))) %>%
  group_by(measurment, month_abb) %>%
  summarise(
    mean_depth = mean(mean_use),
    median_depth = median(mean_use),
  ) %>%
  ungroup() %>%
  pivot_longer(-c(month_abb, measurment),
               names_to = "stat",
               values_to = "est")

depth_used <- det_summary %>%
  filter((measurment %in% c("converted_sensor")))




sig_letters_cld_dt <- sig_letters_cld_d %>%
  left_join(
    det_summary %>%
      filter((measurment %in% c("converted_sensor"))) %>%
      group_by(month_abb) %>%
      summarise(
        min_depth = min(mean_use) - 2.5
      ) %>%
      ungroup()
  ) %>%
  mutate(
    min_depth = case_when(
      month_abb == "Feb" ~ min_depth - 3,
      month_abb == "Mar" ~ min_depth - 1,
      month_abb == "May" ~ min_depth - 2.75,
      month_abb == "Nov" ~ min_depth - 2.25,
      month_abb == "Dec" ~ min_depth - 2.25,
      .default = min_depth
    )
  )

sig_letters_cld_dt

p4 <- ggplot() +
  geom_violin(data = depth_used,
              aes(x = month_abb,
                  y = mean_use,
                         fill = measurment)) +
  stat_summary(
    data = depth_used,
    aes(x = month_abb,
        y = mean_use),
    geom = "errorbar",
    width = 0.1,
    fun.data = mean_se
  ) +
  stat_summary(
    data = depth_used,
    aes(x = month_abb,
        y = mean_use),
    geom = "point",
    size = 3, shape = 21,
    fun = mean
  ) +
  geom_text(data = sig_letters_cld_dt,
            aes(x = month_abb,
                y = min_depth,
                label = .group_fixed)) +
  geom_hline(yintercept = 0, linetype = 3) +
  # geom_point(data = dt_sums, aes(x = month_abb,
  #                               y = est,
  #                               shape = stat,
  #                               group = measurment), size = 3,
  # position = position_dodge(width = 0.9)) +
  scale_y_reverse() +
  # scale_shape_manual(values = c(16, 4),
  #                    name = "Statistic",
  #                    labels = c("Mean",
  #                               "Median")) +
  scale_fill_manual(
    values = c("#f5f5f5"),
    name = "Classification",
    labels = c("Fish Depth")) +
  # scale_fill_brewer(type = "div",
  #                   name = "Classification", direction = -1,
  #                   labels = c(
  #                     "Fish Depth",
  #                     "Bottom Depth")
  # ) +
  guides(
    fill = guide_legend(ncol = 2),
    shape = guide_legend(ncol = 2)
    ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.94, 0.10),
    legend.box = "horizontal"
  ) +
  labs(
    x = "Month",
    y = "Depth (m)"
  )
p4


dt_sum <- det_summary %>%
  filter((measurment %in% c("from_bottom"))) %>%
  group_by(month_abb) %>%
  summarise(
    mean_depth = mean(mean_use),
    median_depth = median(mean_use),
  ) %>%
  ungroup() %>%
  pivot_longer(-c(month_abb),
               names_to = "stat",
               values_to = "est")



sig_letter_locs <- sig_letters_cld %>%
  left_join(
    det_summary %>%
  filter((measurment %in% c("from_bottom"))) %>%
  group_by(month_abb) %>%
  summarise(
    max_depth = max(mean_use) + 2.5
  ) %>%
  ungroup()
  )

det_summary_bt <- det_summary %>%
  filter((measurment %in% c("from_bottom")))



p5 <- ggplot() +
  geom_violin(data = det_summary_bt,
              aes(x = month_abb,
                  y = mean_use,
                  fill = measurment)) +
  stat_summary(
    data = det_summary_bt,
    aes(x = month_abb,
        y = mean_use),
    geom = "errorbar",
    width = 0.1,
    fun.data = mean_se
  ) +
  stat_summary(
    data = det_summary_bt,
    aes(x = month_abb,
        y = mean_use),
    geom = "point",
    size = 3, shape = 21,
    fun = mean
  ) +
  # facet_wrap(~ year, scale = "free_x") +
  geom_text(data = sig_letter_locs,
            aes(x = month_abb,
                y = max_depth,
                label = .group_fix)) +
  # geom_point(data = dt_sum, aes(x = month_abb,
  #                               y = est,
  #                               shape = stat), size = 3,
  # ) +
  geom_hline(yintercept = 5, linetype = 3) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = c(16, 4),
                     name = "Statistic",
                     labels = c("Mean",
                                "Median")) +
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
  # coord_cartesian(ylim = c(0,))
  guides(
    fill = guide_legend(ncol = 2, order = 1),
    shape = guide_legend(ncol = 2, order = 2)
  ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.background = element_blank(),
    legend.position.inside = c(0.93, 0.92),
    legend.box = "horizontal",
    strip.background = element_blank()

  ) +
  labs(
    x = "Month",
    y = "Distance from Bottom (m)"
  )

p5

p6 <- p4 / p5 +
  plot_annotation(tag_levels = "a", tag_suffix = ")"

  )

p4
p6


p6
det_summary_bt %>%
  group_by(month_abb) %>%
  summarise(
    mean_d = mean(mean_use),
    sd = sd(mean_use),
    sem = sd(mean_use) / sqrt(n()),
  )


depth_used %>%
  group_by(month_abb) %>%
  summarise(
    mean_d = mean(mean_use),
    # sd = sd(mean_use),
    sem = sd(mean_use) / sqrt(n()),
  )




ggsave(here("plots",
            "benthic-plots",
            "depth_use_bottom_depth_violin_update.png"),
       width = 16, height = 11, plot = p6)




