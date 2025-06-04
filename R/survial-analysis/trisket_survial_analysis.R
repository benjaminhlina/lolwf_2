# ---- Load packages ----
{
  library(dplyr)
  library(ggplot2)
  library(glatos)
  library(ggtext)
  library(ggfortify)
  library(ggsurvfit)
  library(here)
  library(janitor)
  library(lubridate)
  library(purrr)
  library(qs)
  library(stringr)
  library(survival)
  library(ranger)
  library(tidyr)

}

# ---- bring in data -----

det <- qread(here("data-saved",
                  "detection-data",
                  "removed-dead-dets",
                  "final_cleaned_detection_data.qs"))

ggplot(det, aes(x = detection_timestamp_est,
                y = station)) +
  geom_point(aes(colour = tag_serial_number))

glimpse(det)
det_1 <- det %>%
  mutate(
    date = floor_date(detection_timestamp_est, unit = "day"),
    tag_activation_date = mdy(tag_activation_date),
    month_year = format_ISO8601(tag_activation_date, precision = "ym"),
    year = year(first_det),
    trisket_present = factor(if_else(!is.na(triskit_tag_no), true = "Yes",
                                     false = "No"), level = c("Yes", "No")
    ),
    est_tag_life = str_remove(est_tag_life, " days") %>%
      as.numeric()
  )

det_1 <- det_1 %>%
  group_by(tag_serial_number) %>%
  mutate(
    first_det = first(detection_timestamp_est),
    year = year(first_det),
    month = month(first_det)
  ) %>%
  ungroup()
glimpse(det_1)
det_1 %>%
  distinct(month_year, month, year)

# ---- cretea summary of how long in array ----
sum_det <- det_1 %>%
  group_by(tag_serial_number, sex, length, weight,
           capture_location, triskit_tag_no, trisket_present,
           wild_or_hatchery, month_year, est_tag_life,
           year) %>%
  arrange(tag_serial_number, detection_timestamp_est) %>%
  summarise(
    number_det = n(),
    n_rec = n_distinct(station),
    first_heard = first(detection_timestamp_est),
    last_heard = last(detection_timestamp_est),
    n_det = n_distinct(date),
    days_heard = round(difftime(last_heard, first_heard, units = "days"), 1) %>%
      as.numeric(),
    days_percent = round((n_det / days_heard) * 100, digits = 1)
  ) %>%
  ungroup() %>%
  mutate(
    month_year = factor(month_year, level = c("2022-11", "2021-11")),
    year = factor(year)
  ) %>%
  arrange(month_year, -days_heard) %>%
  mutate(
    tag_serial_number = factor(tag_serial_number, unique(tag_serial_number))
  )



glimpse(sum_det)

# figure out last_heard
sum_det <- sum_det %>%
  mutate(
    status = if_else(last_heard >= as.POSIXct("2024-07-01 00:00:00"),
                     true = 0, false = 1),
    tp = if_else(trisket_present %in% "Yes", true = 1,
                 false = 2),
    yr = as.numeric(year)
  )

glimpse(sum_det)


ggplot(data = sum_det, aes(x = days_heard, y = tag_serial_number,
                           colour = trisket_present)) +
  geom_point()

glimpse(sum_det)

# ---- surival analysis ----

km <- with(sum_det, Surv(days_heard, status))

km_fit <- survfit(Surv(days_heard, status) ~ trisket_present, data = sum_det)

km_estimated_time <- broom::tidy(km_fit, times = c(1,30,60,90*(1:10)))

openxlsx::write.xlsx(km_estimated_time, here::here("results",
                                                   "survivial-analysis",
                                                   "km",
                                                   "model_results_estimated_surival.xlsx"

))


surv_plot <- autoplot(km_fit, surv.geom = "step") +
  scale_fill_viridis_d(name = "Trisket Tag Present",
                       end = 0.7, begin = 0.3) +
  scale_colour_viridis_d(name = "Trisket Tag Present",
                         end = 0.7, begin = 0.3) +
  scale_x_continuous(breaks = seq(0, 725, 50)) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.9)
    # panel.grid = element_blank()
  ) +
  labs(
    x = "Days",
    y = "Overall surival probablity"
  )

surv_plot
ggsave(plot = surv_plot, here::here("plots",
                             "survival-analysis-plots",
                             "km",
                             Sys.Date(),
                             "trisket_only_survival_analysis_update.png"),
       width = 11, height = 8.5)
# ---- model for year ----

glimpse(sum_det)
km <- with(sum_det, Surv(days_heard, status))

km_fit <- survfit(Surv(days_heard, status) ~ year, data = sum_det)

km_estimated_time <- broom::tidy(km_fit, times = c(1,30,60,90*(1:10)))

openxlsx::write.xlsx(km_estimated_time, here::here("results",
                                                   "survivial-analysis",
                                                   "km",
                                                   "model_results_estimated_surival_yr.xlsx"

))


surv_plot <- autoplot(km_fit, surv.geom = "step") +
  scale_fill_viridis_d(name = "Tagging Year",
                       end = 0.95,
                       # begin = 0.2
                       ) +
  scale_colour_viridis_d(name = "Tagging Year",
                         end = 0.95,
                         # begin = 0.2
                         ) +
  scale_x_continuous(breaks = seq(0, 725, 50)) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.9)
    # panel.grid = element_blank()
  ) +
  labs(
    x = "Days",
    y = "Overall surival probablity"
  )

surv_plot
ggsave(plot = surv_plot, here::here("plots",
                             "survival-analysis-plots",
                             "km",
                             Sys.Date(),
                             "trisket_only_survival_analysis_update_yr.png"),
       width = 11, height = 8.5)

# pivot_longer(cols = -c(tag_serial_number, month_year, days_heard),
#              names_to = "detected",
#              values_to = "time_stamp") %>%
# mutate(
#   month_year = factor(month_year, level = c("2022-11", "2021-11"))
# ) %>%
# arrange(month_year, -days_heard) %>%
# mutate(
#   tag_serial_number = factor(tag_serial_number, unique(tag_serial_number))
# )
cox <- coxph(Surv(days_heard, status) ~ trisket_present + sex + year +
               length + weight,
             data = sum_det)

summary(cox)
summary(cox, times = c(1,30,60,90*(1:10)))





cox_fit <- survfit(cox)
glimpse(cox_fit)
autoplot(cox_fit)

glimpse(sum_det)


aa_fit <- aareg(Surv(days_heard, status) ~ trisket_present + sex + year +
                  length + weight,
             data = sum_det)

autoplot(aa_fit)

is.na

r_fit <- ranger(Surv(days_heard, status) ~ trisket_present + sex + year + length,
                data = sum_det,
                mtry = 3,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

death_times <- tibble(
  id = 1:87,
  time = c(r_fit$unique.death.times, rep(last(r_fit$unique.death.times), 18))
)


surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)


glimpse(r_fit$survival)

survi <- as_tibble(r_fit$survival) %>%
    mutate(
    id = 1:nrow(.)
  ) %>%
  pivot_longer(cols = -id,
               names_to = "itteration",
               values_to = "percentage") %>%
  mutate(
    itteration = str_remove(itteration, "V") %>%
      as.numeric()
  ) %>%
  arrange(
    itteration
  ) %>%
  mutate(
    itteration = factor(itteration)
  ) %>%
  left_join(
    death_times, by = "id"
  )
survi
r_fit$unique.death.times

unique(survi$itteration)
unique(survi$id)

# test
# test
ggplot(data = survi, aes(x = time, y = percentage,
                        colour = itteration)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1))


  for (n in sample(c(2:dim(sum_det)[1]), 20)){
  r_fit$unique.death.times
}


plot(r_fit$unique.death.times,
     r_fit$survival[1,],
     type = "l",
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

cols <- colors()
for (n in sample(c(2:dim(sum_det)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7,
       legend = c('Average = black')
       )


vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)
cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)
# Set up for ggplot
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()
