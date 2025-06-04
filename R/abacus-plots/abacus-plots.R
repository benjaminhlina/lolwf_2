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

# ---- make abacus plots ----

det %>%
  split(.$tag_serial_number) %>%
  map(~
        ggsave(
    filename = here("plots",
                    "Innovasea",
                    "abacus plots",
                    Sys.Date(),
                    paste0(unique(.$tag_serial_number),'.png')),
    height = 7,
    width = 11,
    plot =
      ggplot(data = ., aes(x = detection_timestamp_utc, y = station)) +
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
