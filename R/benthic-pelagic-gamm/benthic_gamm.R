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






# at the benthic



# det_bs_sum_na <- det_bs_sum %>%
#   filter(!is.na(benthic_susp))

# ---- model it -----
?te
m <- bam(mean_perc ~
           benthic_susp +
           s(doy, bs = c("cc"),
             by = benthic_susp,
             k = c(40)) +
           s(tag_serial_number, bs = "re") +
           s(year, bs = "re"),
         family = betar(link = "logit",
                        eps = 0.001,
                        # theta = 1.5
         ),
         data = det_bs_sum,
         # filter(benthic_susp == "suspended"),
         method = "fREML")

# summary(m)
# ---- asesses model fit -----
appraise(m, method = "simulate")
# draw(m)

# ----- dataslice ----
ds1 <- data_slice(m,
                  doy = doy,
                  benthic_susp = evenly(benthic_susp, 100)
) %>%
  mutate(
    tag_serial_number = "a",
    year = "0"
  )
ds1 <- expand_grid(
  ds1,
  hour = seq(0, 23, 1)
)


## mean depth and variance

# detection depth / receiver depth
#
#
#
#
#
# ---
#
# # create new datafreame with dummmy variables for RE for plotting
# dat_2 <- det_depth %>%
#   mutate(
#     tag_serial_number = "a",
#     year = 0
#   )
#
# glimpse(dat_2)
#
# # use prediction to get interpolated points
# fits <- predict.gam(m, newdata = dat_2,
#                     # type = "response",
#                     se = TRUE, discrete = TRUE,
#                     exclude = c("s(tag_serial_number)",
#                                 "s(year)"
#                     ),
#                     # newdata.guaranteed = TRUE
# )



# combine fits with dataframe for plotting and calc upper and lower
# add in month abb for plotting
# predicts <- data.frame(dat_2, fits) %>%
#   as_tibble() %>%
#
# doy_select <- det_bs %>%
#   distinct(doy, month_abb, month_no)
#
# doy_select

# ---- predict ----

fv1 <- fitted_values(m,
                     data = ds1, scale = "response",
                     exclude = c("s(tag_serial_number)",
                                 "s(year)"))

fv1
fv1 %>%
  distinct(hour)
glimpse(fv1)
# ---- plot -----

det_bs_sum %>%
  group_by(doy, benthic_susp) %>%
  summarise(
    perc = mean(mean_perc)
  ) %>%
  ungroup()


ggplot() +
  stat_summary(data = det_bs_sum,
               # filter(benthic_susp == "suspended"),
               fun = mean, geom = "point",
               aes(x = doy, y = mean_perc,
                   fill = benthic_susp
               ),
               shape = 21,
               alpha = 0.75,
               size = 3) +
  # stat_summary(data = det_bs_sum_na,
  #              fun.data = mean_se, geom = "errorbar",
  #              aes(x = doy, y = mean_perc,
  #                  group = benthic_susp),
  #              width = 0.15) +
  # facet_wrap(
  #   ~ benthic_susp
  # ) +
  # geom_line(data = fv1,
  #           aes(
  #             x = doy, y = .fitted,
  #             colour = benthic_susp
  #           )) +
  # scale_y_continuous(limits = c(0, 1)) +
  # geom_ribbon(
  #   data = fv1,
  #   aes(x = doy,
  #       ymin = .lower_ci,
  #       ymax = .upper_ci,
  #       group = benthic_susp
  #   ),
  #   alpha = 0.15
  # ) +
  scale_colour_viridis_d(option = "E",
                         begin = 0.3,
                         end = 0.85, direction = -1,
                         name = "Behaviour",
                         labels = c("Benthic",
                                    "Pelagic")) +
  scale_fill_viridis_d(option = "E",
                       begin = 0.3,
                       end = 0.85,
                       direction = -1,
                       name = "Behaviour",
                       labels = c("Benthic",
                                  "Pelagic")) +
  scale_x_continuous(breaks = month_doy,
                     label = month_label
  ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.92, 0.15)
  ) +
  labs(
    x = "Date",
    y = "Mean Percentage"
  ) -> p

p
ggsave(here("plots",
            "benthic-gamm",
            "benthic_pelagic_gamm_lwf.png"), width = 16, height = 8.5, plot = p)

ggplot(data = det_bs_1,
       aes(x = detection_timestamp_est, y = perc,
           fill = benthic_susp)) +
  geom_point(shape = 21, stroke = 0.1, colour = "black",
             position = position_jitter(width = 0.2)) +
  facet_wrap(~ tag_serial_number) +
  scale_fill_viridis_d(option = "A",
                       begin = 0.1,
                       end = 0.9, alpha = 0.5,
                       name = "Behaviour",
                       labels = c("Benthic",
                                  "Suspended")) +
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

# p1

ggsave(here("plots",
            "benthic-raw",
            "benthic_pelagic_raw_perc.png"), width = 11 * 2, height = 8.5 * 2, plot = p1)



ggplot() +
  # stat_summary(data = det_bs_sum,
  #              # filter(benthic_susp == "suspended"),
  #              fun = mean, geom = "point",
  #              aes(x = doy, y = mean_perc,
  #                  fill = benthic_susp
  #              ),
  #              shape = 21,
  #              alpha = 0.75,
  #              size = 3) +
  # stat_summary(data = det_bs_sum_na,
  #              fun.data = mean_se, geom = "errorbar",
  #              aes(x = doy, y = mean_perc,
  #                  group = benthic_susp),
  #              width = 0.15) +
  # facet_wrap(
  #   ~ benthic_susp
  # ) +
  geom_line(data = fv1,
            aes(
              x = doy, y = .fitted,
              colour = benthic_susp
            )) +
  facet_wrap(~ hour) +
  # scale_y_continuous(limits = c(0, 1)) +
  # geom_ribbon(
  #   data = fv1,
  #   aes(x = interaction(hour, doy),
  #       ymin = .lower_ci,
  #       ymax = .upper_ci,
  #       group = benthic_susp
  #   ),
  #   alpha = 0.15
  # ) +
  scale_colour_viridis_d(option = "A",
                         begin = 0.2,
                         end = 0.8,
                         name = "Behaviour",
                         labels = c("Benthic",
                                    "Suspended")) +
  scale_fill_viridis_d(option = "A",
                       begin = 0.2,
                       end = 0.8,
                       name = "Behaviour",
                       labels = c("Benthic",
                                  "Suspended")) +
  # scale_x_continuous(breaks = month_doy,
  #                    label = month_label
  # ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.92, 0.15)
  ) +
  labs(
    x = "Date",
    y = "Mean Percentage"
  ) -> p1

p1
