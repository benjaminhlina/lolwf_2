# ---- bring in packages ----

{
  library(data.table)
  library(dplyr)
  library(here)
  library(lubridate)
  library(qs)
  library(readr)
  library(readxl)
  library(tidyr)
}

# ---- bring in detection data ----

df <- read_csv(here("data-raw",
                    "detections",
                    "LOIBM_detectionsWithLocs_20240719_204215.csv"))

glimpse(df)
df
unique(df$sensor_unit)
unique(df$sensor_value)

df %>%
  distinct(transmitter_id, sensor_value, sensor_unit)

df <- df %>%
  mutate(
    tag_serial_number = as.character(tag_serial_number),
    transmitter_id = as.character(transmitter_id)
  )

# ---- bring in tagging metadata ----
tag_metadata <- read_csv(here("data-raw",
                              "metadata",
                              "LOIBM_GLATOS_Tagging 2.csv")) %>%
  janitor::clean_names()

glimpse(tag_metadata)



# ---- bring tag specs -----
tag_specs <- read_csv(here("data-raw",
                           "transmitter-metadata",
                           "lolwf_tag_specs.csv")) %>%
  janitor::clean_names()


glimpse(tag_specs)

# ----- grab columns we need from tag_specs ----

tag_specs_select <- tag_specs %>%
  select(serial_no, tag_family, vue_tag_id,
         step_1_min_delay_sec, step_1_max_delay_sec, sensor_type:intercept) %>%
  rename(
    min_delay = step_1_min_delay_sec,
    max_delay = step_1_max_delay_sec
  )

glimpse(tag_specs_select)

# fix code space issue
# need to make transmitter ID column to line up tag specs with
# detection data.
tag_specs_select <- tag_specs_select %>%
  separate_wider_delim(vue_tag_id,names = c("frequency",
                                            "codespace",
                                            "transmitter_id"),
                       delim = "-", cols_remove = FALSE) %>%
  mutate(
   # paste frequency and codespace together and
     transmitter_codespace = paste(frequency, codespace, sep = "-"),
  ) %>%
  select(serial_no, tag_family,vue_tag_id,
         transmitter_codespace, transmitter_id:intercept) %>%
  mutate(
    serial_no = as.character(serial_no),
    transmitter_id = as.character(transmitter_id)
  )
glimpse(tag_specs_select)


# # ---- filter out tags that aren't real ----
# glimpse(df)
# glimpse(tag_metadata)
# ---- compare columns from detections to tag_metadata -----

tst <- list(df, tag_metadata)


compared <- janitor::compare_df_cols(tst)

compared

tag_metadata %>%
  distinct(wild_or_hatchery)

# capture location, capt_long, aptur lat,
# Floy, glatos_external_tag_id1
# marturity_gonad_size, lenght_type,
# dna_taken, est_tag_life, glatos release,
# tag_activation_datae seems like there might be an error here,
# trisket_tag_no
# tag_metadata is fish tagging data
tag_metadata_select <- tag_metadata %>%
  dplyr::select(tag_serial_number, capture_longitude,
                capture_latitude, capture_depth,
                # floy,
                glatos_external_tag_id1,
                # maturity_gonad_size,
                length_type, dna_sample_taken, length_type,
                est_tag_life,
                glatos_release_date_time,
                tag_activation_date,
                triskit_tag_no, wild_or_hatchery) %>%
  mutate(
    tag_serial_number = as.character(tag_serial_number)
  )


# ---- Join tagging data ----

df_1 <- df %>%
  left_join(tag_metadata_select, by = "tag_serial_number")

# fish tagging data is now joined with detection dataframe
#
# ---- join tagging specs ----
glimpse(tag_specs_select)

# check min and max delays
tag_specs_select %>%
  distinct(serial_no, min_delay, max_delay) %>%
  print(n = 100)

glimpse(df_1)


# join the tagging specs to detection dataframe
df_2 <- df_1 %>%
  left_join(tag_specs_select)


glimpse(df_2)
unique(tag_specs_select$units)
# ---- calculate mean delay ----

df_2 <- df_2 %>%
  mutate(
    mean_delay = (min_delay + max_delay) / 2
  )
glimpse(df_2)



# ---- conveted sensor ----
df_3 <- df_2 %>%
  mutate(
    detection_timestamp_est = with_tz(detection_timestamp_utc, "EST"),
    converted_sensor = round((sensor_value * slope) + intercept, digits = 2),
    units = case_when(
      units %in% "Meters" ~ "m",
      units %in% "\xb0C" ~ "°C",
      units %in% is.na(units) ~ NA
    )
  )

glimpse(df_3)
df_3 %>%
  filter(units %in% c("°C", "m")) %>%
  glimpse()

# df_2 %>%
#   distinct(tag_serial_number, min_delay, max_delay) %>%
#   arrange(tag_serial_number) %>%
#   print(n = 57)



# print()
# tag_specs_select %>%
#   distinct(serial_no) %>%
#   arrange(serial_no) %>%
#   print(n = 67)
# df[2699, ] %>%
#   glimpse()

# tag_issue <- tag_metadata %>%
#   filter(tag_serial_number == "1382381")
# openxlsx::write.xlsx(tag_issue, here::here("data-issue",
#                                            "tag_issue_bh.xlsx"))


glimpse(df_3)
df_3 %>%
  arrange(tag_serial_number, detection_timestamp_est) %>%
  glimpse()


unique(df_3$units)
# ---- write qs file ----

qsave(df_3, here("data-saved",
                 "detection-data",
                 paste("ammended_detection_data_update_", Sys.Date(), ".qs",
                       sep ="")))
