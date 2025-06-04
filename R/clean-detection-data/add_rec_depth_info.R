# ---- Load packages ----
{
  library(amt)
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
  library(sf)
  library(tidyr)

}

# ---- bring in data -----

det <- qread(here("data-saved",
                  "detection-data",
                  "removed-dead-dets",
                  "final_cleaned_depth_detection_data.qs"))

glimpse(det)

rec_loc <- readr::read_csv(here("data-raw",
                                "metadata",
                                "GLATOS_receiverLocations_20240719_194605.csv"))


glimpse(rec_loc)

rec_loc_1 <- rec_loc %>%
  filter(station %in% det$station) %>%
  dplyr::select(consecutive_deploy_no, station, bottom_depth,
         riser_length, instrument_depth)

rec_loc %>%
  filter(station == "CBC-014") %>%
  glimpse()

rec_loc_2 <- rec_loc_1 %>%
  group_by(station) %>%
  summarise(
    mean_bottom_depth = mean(bottom_depth, na.rm = TRUE),
    mean_riser_length = mean(riser_length, na.rm = TRUE),
    mean_instrument_depth = mean(instrument_depth, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  print(n = 171)


# rec_loc_1 %>%
#   group_by(station) %>%
#   summarise(
#     n_dup = n_distinct(bottom_depth)
#   ) %>%
#   print(n = 171)
det
glimpse(det)

rec_loc_1 %>%
  filter(station == "PPE-003")




det_rec_depth <- det %>%
  left_join(rec_loc_2, by = c("station"))


glimpse(det_rec_depth)


qsave(det_rec_depth,
             here("data-saved",
                  "detection-data",
                  "removed-dead-dets",
                  "final_cleaned_rec_detection_data.qs"))
