
# ---- Load packages ----
{
  library(amt)
  library(dplyr)
  library(data.table)
  library(ggplot2)
  library(glatos)
  library(glmmTMB)
  library(ggtext)
  library(emmeans)
  library(multcomp)
  library(multcompView)
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


# det <- qread(here("data-saved",
#                   "detection-data",
#                   "removed-dead-dets",
#                   "final_cleaned_rec_detection_data.qs"))
# glimpse(det)




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
  pivot_longer(cols = -c("tag_serial_number", "detection_timestamp_est","hour", "date", "year",
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

glimpse(det)

# det %>%
#   distinct(
#     tag_serial_number, tag_family, serial_no, units
#   ) %>%
#   print(n = 1000)
#
#
# det_temp <- det %>%
#   filter(units == "°C") %>%
#   group_by(date, serial_no) %>%
#   summarise(
#     mean_temp = round(mean(converted_sensor, 1)),
#     sd_temp = round(sd(converted_sensor, 1)),
#     sem_temp = round((sd(converted_sensor) / sqrt(n())), 1)
#   ) %>%
#     ungroup() %>%
#     mutate(
#       doy = yday(date),
#       month_abb = month(date, label = TRUE),
#       year = year(date)
#
#     ) %>%
#   select(-date) %>%
#   filter(serial_no %in% det_summary$tag_serial_number)
#
# det_temp

# ---- look at distribution -----

unique(det_summary$measurment)

det_summary_bt <- det_summary %>%
  filter(measurment == "converted_sensor" & mean_use >= 0)



ggplot(data = det_summary_bt, aes(x = mean_use)) +
  geom_histogram()

summary(det_summary_bt$mean_use)


fitdistrplus::descdist(det_summary_bt$mean_use)


# ---- model ----

m <- glmmTMB(mean_use~ month_abb +
               (1 | tag_serial_number) +
               (1 | year),
             family = Gamma(link = "log"),
             # ziformula = ~1,
             # REML = TRUE,
             data = det_summary_bt)


# ---- model fit ----

res <- DHARMa::simulateResiduals(m)


plot(res, asFactor = T)
hist(resid(m))
# ypred <- predict(m)
# ress <- residuals(m, type = 'pearson')
# plot(ypred,ress)

DHARMa::testDispersion(res)



summary(m)
# ----- main effects ----

am <- car::Anova(m)

amb <- broom.mixed::tidy(am)

amb$statistic
am
summary(m)


multi_comp <- m %>%
  emmeans(pairwise ~ month_abb, type = "response")




contrast_effects <- multi_comp %>%
  contrast(method = "pairwise",
           adjust = "bonferroni"
  )




#
#
#  contrast(multi_comp, method = "pairwise",
#                              adjust = "Tukey")

contrast_month <- contrast_effects %>%
  broom::tidy() %>%
  clean_names() %>%
  arrange(adj_p_value, contrast) %>%
  separate_wider_delim(contrast, delim = " / ", names = c("month_a",
                                                          "month_b"),
                       cols_remove = FALSE)


contrast_month
#
# print(basin_season_contrast, n = 66)
#
#
compares <- cld(
  object = multi_comp,
  adjust = "sidak",
  Letters = letters,
  alpha = 0.05,
) %>%
  as_tibble() %>%
  arrange(month_abb)

compares

# compares <- compares %>%
#   mutate(
#     .group = factor(.group),
#     .group_fixed = case_when(
#       .group == "     e   " ~ "a",
#       .group == "g" ~ "b",
#       .group == "h" ~ "c",
#       .group == "c" ~ "e",
#       .group == "ef" ~ "af",
#       .group == "cd" ~ "ed",
#       .group == "a" ~ "g",
#       .group == "b" ~ "h"
#     )
#   )

compares
compares %>%
  readr::write_csv(here::here("results",
                              "depth-use",
                              "cld.csv"))

# comparesmonth.abb# compares$.group

# contrast_month %>%
#   filter(adj_p_value <= 0.05) %>%
#   mutate(
#     month_a = factor(month_a, level = month.abb),
#     month_b = factor(month_b, level = month.abb),
#   ) %>%
#   arrange(month_a, month_b) %>%
#
#   openxlsx::write.xlsx(here::here("results",
#                                   "off-bottom",
#                                   "glmm_compare_month_off_bottom.xlsx"))





contrast_month %>%
  filter(adj_p_value <= 0.05) %>%
  mutate(
    month_a = factor(month_a, level = month.abb),
    month_b = factor(month_b, level = month.abb),
  ) %>%
  arrange(month_a, month_b) %>%
  print(n = 100)


# ---- kruskal-wallice test ----

kw <- kruskal.test(x = det_summary_bt$mean_use, g = det_summary_bt$month_abb)


kw


dts <- dunn.test::dunn.test(det_summary_bt$mean_use,
                            det_summary_bt$month_abb,wrap = TRUE, label = TRUE)


det_summary_bt %>%
  group_by(month_abb) %>%
  summarise(
    mean_d = mean(mean_use),
    sd = sd(mean_use),
    sem = sd(mean_use) / sqrt(n()),
  )

