
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
month_doy <- if_else(month_doy == 321, true = 307,
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
                         "doy",
                         "month_no", "month_abb", "station",
                         "deploy_long",
                         "deploy_lat", "units"),
               names_to = "measurment",
               values_to = "depth")

det_1_flip





det_summary <- det_1_flip %>%
  group_by(
    tag_serial_number, doy, month_abb, year,
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


det_summary %>%
  filter(!(measurment %in% c("bathy_limit"
                             , "from_bottom"
  ))) %>%
  ggplot() +
  geom_point(aes(x = doy, y = mean_use,
                 fill = measurment), shape = 21,
             alpha = 0.5, size = 2.5) +
  # geom_errorbar(aes(x = doy, y = mean_use,
  #                   ymin = mean_use - sem,
  #                   ymax = mean_use + sem), width = 0.1) +
  scale_y_reverse(name = "Depth (m)") +
  scale_fill_brewer(type = "div",
                    name = "Classification", direction = -1,
                    labels = c(
                      "Fish Depth",
                      "Bottom Depth")
  ) +
  # theme_bw(
  #   base_size = 15
  # ) +
  theme(
    # panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.15)
  ) +
  labs(
    x = "Date"
  )


det_summary %>%
  filter(!(measurment %in% c("bathy_limit"
                             , "from_bottom"
  ))) %>%
  ggplot(aes(x = mean_use)) +
  geom_histogram() +
  facet_wrap(~ measurment)


library(mgcv)
library(gratia)

det_summary <- det_summary %>%
  mutate(
    measurment = factor(measurment),
    tag_serial_number = factor(tag_serial_number),
    year = factor(year)
  )

m <- bam(mean_use ~ measurment + year,
           s(doy, by = measurment, k = 15, bs = "cc") +
           s(tag_serial_number, bs = "re") +
           s(year, bs = "re"),
         family = Gamma(link = "log"),
         method = "fREML",
         data = det_summary %>%
           filter(!(measurment %in% c("bathy_limit"
                                      , "from_bottom"
           ))))

draw(m)
appraise(m)

anova(m)



ds1 <- det_summary %>%
  mutate(
    tag_serial_number = "a",
    year = "x"
  ) %>%
  filter(!(measurment %in% c("bathy_limit"
                             , "from_bottom"
  )))

draw(m)



fv1 <- fitted_values(m, data = ds1, scale = "response",
                     exclude = c("s(tag_serial_number)",
                                 "s(year)"))


ggplot() +
  geom_line(data = fv1,
            aes(x = doy, y = .fitted,
                colour = measurment)) +
  geom_ribbon(
    data = fv1,
    aes(x = doy,
        ymin = .lower_ci,
        ymax = .upper_ci,
        group = measurment
    ),
    alpha = 0.15
  ) +
  scale_x_continuous(breaks = month_doy,
                     label = month_label
  ) +
  scale_y_reverse()


det_summary %>%
  filter((measurment %in% c("from_bottom"))) %>%
  summary()


det_summary %>%
  filter((measurment %in% c("from_bottom"))) %>%
  ggplot(aes(x = mean_use)) +
  geom_histogram()


?family.mgcv
m1 <- bam(mean_use ~
            year +
            s(doy, by = year, k = 30, bs = "cc") +
            s(tag_serial_number, bs = "re"),
            # s(year, bs = "re"),
          family = tw(a = 1.89),
          method = "fREML",
          data = det_summary %>%
            filter(measurment %in% c("from_bottom")))


appraise(m1)
draw(m1)
anova(m1)


ds2 <- det_summary %>%
  mutate(
    tag_serial_number = "a",
    # year = "x"
  ) %>%
  filter((measurment %in% c("from_bottom"
  )))


fv2 <- fitted_values(m1, data = ds2, scale = "response",
                     exclude = c("s(tag_serial_number)",
                                 # "s(year)"
                                 ))





ggplot() +
  # stat_summary(data = det_summary %>%
  #                filter((measurment %in% c("from_bottom"))),
  #              geom = "errorbar",
  #              aes(y = mean_use,
  #                  x = doy),
  #              fun.data = mean_se, width = 0.1,
  #              colour = "grey75") +
  stat_summary(data = det_summary %>%
                 filter((measurment %in% c("from_bottom"))),
               geom = "point",
               aes(y = mean_use,
                   x = doy),
               fun = mean,
               shape = 21, size = 2.5 ) +

  geom_line(data = fv2,
            aes(x = doy, y = .fitted,
                colour = measurment)) +
  geom_ribbon(
    data = fv2,
    aes(x = doy,
        ymin = .lower_ci,
        ymax = .upper_ci,
        group = measurment
    ),
    alpha = 0.15
  ) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 5, linetype = 3) +
  scale_colour_manual(
    values = c('#5ab4ac'),
    # type = "div",
    name = "Classification",
    # direction = -1,
    labels = c(
      "Depth From Bottom")
    #            "Bottom Depth")
  ) +
  scale_x_continuous(breaks = month_doy,
                     label = month_label
  ) +
  theme_bw(
    base_size = 15
  ) +
  theme(
    panel.grid = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.9)
  ) +
  labs(
    x = "Date",
    y = "Depth (m)"
  ) -> p

p
