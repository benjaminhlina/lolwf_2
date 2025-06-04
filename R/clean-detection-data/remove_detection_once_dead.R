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


# ---- bring in detection data -----

det <- qread(here("data-saved",
                  "detection-data",
                  "false-detection",
                  "cleaned_detection_data.qs"))

glimpse(det)


# ---- created date -----
det <- det %>%
  mutate(
    date = floor_date(detection_timestamp_est, unit = "day")
  )



test <- det %>%
  filter(tag_serial_number %in% "1575331"
         # & date > as.Date("2023-07-30")
         # & date < as.Date("2023-08-01")
  ) %>%
  arrange(detection_timestamp_est)
glimpse(test)

test_name <- unique(test$tag_serial_number)
test_sen <- unique(test$units)

test %>%
  ggplot(aes(x = detection_timestamp_est, y = station)) +
  geom_point(aes(fill = converted_sensor), shape = 21, size = 3, alpha = 0.5) +
  geom_line(aes(group = 1)) + # we can remove line if it's distracting
  scale_fill_viridis_c(
    # begin = 0.25, end = 0.75,
    option = "D", name = test_sen) +
  theme_bw(
    base_size = 15
  ) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    title = paste("Transmitter ID:", test_name, sep = " "),
    x = "Date",
    y = "Receiver")



# ------ stop date ----

det <- det %>%
  filter(!(tag_serial_number %in% c("1575310",
                                    "1575315",
                                    "1575322",
                                    "1575323",
                                    "1575331"))) %>%
  arrange(tag_serial_number, detection_timestamp_est) %>%
  group_by(tag_serial_number) %>%
  mutate(
    stop_date = last(date),
    stop_date = case_when(
      tag_serial_number %in% "1304911" ~ as.Date("2022-12-28"),
      tag_serial_number %in% "1382384" ~ as.Date("2022-07-08"),
      # tag_serial_number %in% "1382385" ~ as.Date("2021-11-21"),
      tag_serial_number %in% "1382390" ~ as.Date("2022-04-10"),
      tag_serial_number %in% "1382391" ~ as.Date("2021-12-02"),
      tag_serial_number %in% "1382392" ~ as.Date("2023-01-09"),
      tag_serial_number %in% "1396205" ~ as.Date("2022-10-01"),
      # tag_serial_number %in% "1382393" ~ as.Date("2021-11-20"),
      tag_serial_number %in% "1382398" ~ as.Date("2022-07-08"),
      tag_serial_number %in% "1382626" ~ as.Date("2021-11-19"),
      tag_serial_number %in% "1396207" ~ as.Date("2022-06-23"),
      tag_serial_number %in% "1514400" ~ as.Date("2022-12-05"),
      tag_serial_number %in% "1382386" ~ as.Date("2022-11-25"),
      tag_serial_number %in% "1382623" ~ as.Date("2022-12-05"),
      tag_serial_number %in% "1510195" ~ as.Date("2023-08-03"),
      tag_serial_number %in% "1528232" ~ as.Date("2024-01-01"),
      # tag_serial_number %in% "1575316" ~ as.Date("2022-12-15"),
      # tag_serial_number %in% "1575320" ~ as.Date("2022-12-05"),
      # tag_serial_number %in% "" ~ as.Date("2022-12-05"),
      .default = stop_date
    )
  ) %>%
  ungroup()



# ---- plot new abacus plots -----
det %>%
  filter(date <= stop_date) %>%
  split(.$tag_serial_number) %>%
  map(~ ggsave(
    filename = here("plots",
                    "Innovasea",
                    "abacus plots",
                    "removed-dead-det",
                    Sys.Date(),
                    paste0(unique(.$tag_serial_number),'.png')),
    height = 7,
    width = 11,
    plot =
      ggplot(data = ., aes(x = detection_timestamp_est, y = station)) +
      geom_point(aes(fill = station), shape = 21, size = 3, alpha = 0.5) +
      geom_line(aes(group = 1)) + # we can remove line if it's distracting
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Receiver") +
      theme_bw(
        base_size = 15
      ) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = paste("Transmitter ID:", unique(.$tag_serial_number), sep = " "),
        x = "Date",
        y = "Receiver")
  )
  )


det_filtered <- det %>%
  filter(date <= stop_date)

det_filtered_1 <- det_filtered %>%
  group_by(units) %>%
  filter(converted_sensor > 0 & converted_sensor <= 100 |
           is.na(converted_sensor)) %>%
  ungroup()

det_filtered_1 %>%
  group_by(units) %>%
  summarise(
    max = max(converted_sensor)
  )


# ---- save ----
qsave(det_filtered_1, here("data-saved",
                         "detection-data",
                         "removed-dead-dets",
                         "final_cleaned_detection_data.qs"))


