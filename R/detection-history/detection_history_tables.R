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
glimpse(det)


# ---- create months ----
month
det <- det %>%
  mutate(
    month_no = month(detection_timestamp_est),
    month_abb = month(detection_timestamp_est,abbr = TRUE,
                      label = TRUE),
    year = year(detection_timestamp_est),
  ) %>%
  arrange(tag_serial_number, detection_timestamp_est) %>%
  mutate(
    year_first_heard = first(detection_timestamp_est) %>%
      year()
  )

glimpse(det)
det_sum_mo <- det %>%
  group_by(month_abb, year) %>%
  summarise(
    n_detections = n(),
    n_tags = n_distinct(tag_serial_number),
    n_stations = n_distinct(station)
  ) %>%
  ungroup() %>%
  arrange(year, month_abb)

det_sum_mo

openxlsx::write.xlsx(det_sum_mo,
                     here::here("results",
                                "summary-movement",
                                "monthly_detection_history.xlsx"))


det_sum_id <- det %>%
  group_by(tag_serial_number, tag_activation_date,
           year_first_heard) %>%
  summarise(
    n_detections = n(),
    n_station = n_distinct(station),
    n_month = n_distinct(month_abb)
  ) %>%
  ungroup() %>%
  arrange(tag_serial_number)
det_sum_id

openxlsx::write.xlsx(det_sum_id,
                     here::here("results",
                                "summary-movement",
                                "individual_fish_detection_history.xlsx"))


det_sum_id_mo <- det_sum_id %>%
  group_by(
    n_month
  ) %>%
  summarise(
    tags_detected = n_distinct(tag_serial_number)
  ) %>%
  ungroup()

det_sum_id_mo

openxlsx::write.xlsx(det_sum_id_mo,
                     here::here("results",
                                "summary-movement",
                                "number_of_months_fish_detection_history.xlsx"))
