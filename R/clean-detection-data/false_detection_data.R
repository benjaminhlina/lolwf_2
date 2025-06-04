#
# remotes::install_github('ocean-tracking-network/glatos', build_vignettes = TRUE)
# load packages
{
  library(data.table)
  library(dplyr)
  library(here)
  library(glatos)
  library(qs)
  library(purrr)
  library(readxl)
}

# ---- bring in detection data ----

df <- qread(here("data-saved",
                 "detection-data",
                 "ammended_detection_data.qs"))

class(df)

glimpse(df)

# 1,122,910 × 55
# df %>%
#   distinct(transmitter_id, min_delay, max_delay) %>%
#   print(n = 57)
# df <- min_lag(det)

# ---- run false_detection ----
det_map <- df %>%
  group_by(tag_serial_number) %>%
  arrange(detection_timestamp_utc) %>%
  split(.$tag_serial_number) %>%
  map(~ false_detections(det = .x, tf = .x$mean_delay * 30)) %>%
  bind_rows(.id = "tag_serial_number") %>%
  ungroup()


# test <- false_detections(df, tf = df$mean_delay * 30, show_plot = TRUE)

# remove false dtections
df_1 <- det_map %>%
  filter(passed_filter == 1)


# save data
qsave(df_1, here("data-saved",
                 "detection-data",
                 "false-detection",
                 "cleaned_detection_data.qs"))
