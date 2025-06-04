# ----- bring in packages -----

{
  library(dplyr)
  library(fitdistrplus)
  library(here)
  library(ggplot2)
  library(gratia)
  library(lubridate)
  library(mgcv)
  library(qs)
}


# ---- bring in benthic_suspsended dataframe ----
det_bs <- qread(
  here:::here(
    "data-saved",
    "det-benthic-suspended",
    "detection_benthic_suspended_wf_random.qs")
)

glimpse(det_bs)


det_bs <- det_bs %>%
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
# ----- create summary table ----

det_bs_sum <- det_bs_1 %>%
  mutate(
    tag_serial_number = factor(tag_serial_number),
    year = factor(year(detection_timestamp_est)),
    benthic_susp = factor(benthic_susp),
    hour = hour(detection_timestamp_est),
  ) %>%
  group_by(tag_serial_number, benthic_susp, hour, doy, year) %>%
  summarise(
    mean_perc = mean(perc, na.rm = TRUE)
  ) %>%
  ungroup()


glimpse(det_bs_sum)


doy_select <- det_bs %>%
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



# ---- look at the distribution ----

percs <- det_bs_sum$mean_perc

descdist(percs)

fit_beta <- fitdist(percs, distr = "beta", method = "mme")

plot(fit_beta)

ggplot(data = det_bs_sum, aes(x = mean_perc)) +
  geom_histogram()



ggplot(data = det_bs_1,
       aes(x = detection_timestamp_est, y = perc,
           fill = benthic_susp)) +
  geom_point(shape = 21, stroke = 0.1, colour = "black",
             # position = position_jitter(width = 0.2),
             size = 2) +
  facet_wrap(~ tag_serial_number) +
  scale_fill_viridis_d(option = "A",
                       begin = 0.1,
                       end = 0.9, alpha = 0.5,
                       name = "Behaviour",
                       direction = -1,
                       labels = c("Benthic",
                                  "Pelagic")) +
  # scale_x_continuous(breaks = month_doy,
  #                    label = month_label) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.05),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Date",
    y = "Percentage"
  ) -> p1

p1

ggsave(here("plots",
            "benthic-raw",
            "benthic_pelagic_raw_perc.png"), width = 11 * 2, height = 8.5 * 2, plot = p1)


ggplot(data = det_bs_1,
       aes(x = doy, y = perc,
           fill = benthic_susp)) +
  geom_point(shape = 21, stroke = 0.1, colour = "black",
             # position = position_jitter(width = 0.2)
             size = 2) +
  facet_wrap(~ tag_serial_number) +
  scale_fill_viridis_d(option = "A",
                       begin = 0.1,
                       end = 0.9, alpha = 0.5,
                       direction = -1,
                       name = "Behaviour",
                       labels = c("Benthic",
                                  "Pelagic")) +
  scale_x_continuous(breaks = month_doy,
                     label = month_label) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.05),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = "Date",
    y = "Percentage"
  ) -> p2

p2

ggsave(here("plots",
            "benthic-raw",
            "benthic_pelagic_raw_perc_doy.png"), width = 11 * 2,
       height = 8.5 * 2, plot = p2)


