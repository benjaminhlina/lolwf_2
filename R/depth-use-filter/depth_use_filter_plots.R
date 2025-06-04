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
  library(plotly)
  library(qs)
  library(stringr)
  library(tidyr)

}


# ---- bring in data ----
det_clean <- qread(here("data-saved",
                        "detection-data",
                        "removed-dead-dets",
                        "final_cleaned_detection_data.qs"))

glimpse(det_clean)

det_clean %>%
  distinct(units)
# ---- det depth ----
det_clean %>%
  filter(units %in% "m") %>%
  # filter(date <= stop_date) %>%
  split(.$tag_serial_number) %>%
  map(~
        ggsave(
    filename = here("plots",
                    "depth use",
                    # "abacus plots",
                    # "removed-dead-det",
                    Sys.Date(),
                    paste0(unique(.$tag_serial_number),'.png')),
    height = 7,
    width = 11,
    plot =
      ggplot(data = ., aes(x = detection_timestamp_est, y = converted_sensor)) +
      geom_point(aes(fill = station), shape = 21, size = 3, alpha = 0.5) +
      geom_line(aes(group = 1)) + # we can remove line if it's distracting
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Receiver") +
      scale_y_reverse() +
      theme_bw(
        base_size = 15
      ) +
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.5)
      ) +
      labs(
        title = paste("Transmitter ID:", unique(.$tag_serial_number), sep = " "),
        x = "Date",
        y = "Depth")
  )

)




det_clean <- det_clean %>%
  arrange(tag_serial_number, detection_timestamp_est) %>%
  group_by(tag_serial_number) %>%
  mutate(
    stop_date = case_when(
      tag_serial_number %in% "1382385" ~ as.Date("2021-11-22"),
      tag_serial_number %in% "1382388" ~ as.Date("2021-12-20"),
      tag_serial_number %in% "1382389" ~ as.Date("2022-05-11"),
      tag_serial_number %in% "1382390" ~ as.Date("2021-12-14"),
      tag_serial_number %in% "1382620" ~ as.Date("2021-12-07"),
      tag_serial_number %in% "1382621" ~ as.Date("2022-04-20"),
      tag_serial_number %in% "1514394" ~ as.Date("2022-12-07"),
      tag_serial_number %in% "1517812" ~ as.Date("2023-01-01"),
      .default = stop_date
    )
  ) %>%
  ungroup()

det_filtered <- det_clean %>%
  filter(date <= stop_date)


det_filtered %>%
  filter(units %in% "m") %>%

  split(.$tag_serial_number) %>%
  map(~
        ggsave(
          filename = here("plots",
                          "depth use",
                          "fixed",
                          # "removed-dead-det",
                          Sys.Date(),
                          paste0(unique(.$tag_serial_number),'.png')),
          height = 7,
          width = 11,
          plot =
            ggplot(data = ., aes(x = detection_timestamp_est, y = converted_sensor)) +
            geom_point(aes(fill = station), shape = 21, size = 3, alpha = 0.5) +
            geom_line(aes(group = 1)) + # we can remove line if it's distracting
            scale_fill_viridis_d(begin = 0.25, end = 0.75,
                                 option = "D", name = "Receiver") +
            scale_y_reverse() +
            theme_bw(
              base_size = 15
            ) +
            theme(panel.grid = element_blank(),
                  plot.title = element_text(hjust = 0.5)
            ) +
            labs(
              title = paste("Transmitter ID:", unique(.$tag_serial_number), sep = " "),
              x = "Date",
              y = "Depth")
        )

  )

glimpse(det_filtered)
det_filtered %>%
  distinct(glatos_array) %>%
  arrange(glatos_array) %>%
  print(n = 30)


det_filtered <- det_filtered %>%
  filter(glatos_array != "OSM")


det_filtered %>%
  distinct(station, deploy_long, deploy_lat) %>%
  sf::st_as_sf(coords = c("deploy_long",
                      "deploy_lat"),
           crs = 4326) %>%
  mapview::mapview()

qsave(det_filtered, here("data-saved",
                        "detection-data",
                        "removed-dead-dets",
                        "final_cleaned_depth_detection_data.qs"))

glimpse(det_clean)
