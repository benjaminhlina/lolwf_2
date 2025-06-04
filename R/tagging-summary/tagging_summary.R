# ---- library packags -----
{
  library(dplyr)
  library(ggplot2)
  library(here)
  library(lubridate)
  library(readr)
  library(tidyr)
}

# ---- bring in data ----

lc <- read_csv(here("data-raw",
                    "length-conversion",
                    "GFS_Gillnet_Len-Len_coefficients.csv")) %>%
  janitor::clean_names()






# ---- bring in data ----
tag_metadata <- read_csv(here("data-raw",
                              "metadata",
                              "LOIBM_GLATOS_Tagging 2.csv")) %>%
  janitor::clean_names()

glimpse(tag_metadata)


tag_metadata %>%
  distinct(glatos_release_date_time)



tag_metadata <- tag_metadata %>%
  mutate(
    # tag_activation_date = mdy(tag_activation_date),
    year_activation = year(tag_activation_date),
    # glatos_release_date_time = mdy_hms(glatos_release_date_time),
    year_release = year(glatos_release_date_time)
  )

glimpse(tag_metadata)
# ----- fork length to tl conversion -----
# ---- filter out lwf -----
lc_lwf <- lc %>%
  filter(spc == 91) %>%
  arrange(n)

lc_lwf


# ----- plot line ----

preds <- expand_grid(
  lc_lwf,
  pred_fl = seq(0, 800, 10)
) %>%
  mutate(
    pred_tl = flen_slope * pred_fl + intercept
  )


ggplot(data = preds %>%
         filter(year != 1958),
       aes(x = pred_fl,
           y = pred_tl,
           colour = factor(year))) +
  geom_line()



preds_avg <- lc_lwf %>%
  group_by(spc, common) %>%
  summarise(
    mean_intercept = mean(intercept),
    mean_slope = mean(flen_slope)
  ) %>%
  ungroup()


glimpse(tag_metadata)

# tagging summary table -----
tag_summary <- tag_metadata %>%
  filter(common_name_e %in% "Lake Whitefish") %>%
  mutate(
    length = length * 1000,
    length = if_else(length_type == "Total",
                     true = (length -
                       preds_avg$mean_intercept) / preds_avg$mean_slope,
                     false = length
    ),
    tag_model = case_when(
      tag_model %in% "V16-4x-BLU-1" ~ "V16-4x",
      tag_model %in% "V16P-4x-BLU-1-0136m" ~ "V16P-4x",
      tag_model %in% "V16-TP-4x" ~ "V16TP-4x",


      .default = tag_model
    ) %>%
      stringr::str_remove("\\-\\d[x]"),
    est_tag_life = case_when(
      est_tag_life %in%  "730  days" ~ "730 days",
      est_tag_life %in%  "3083 days" ~ "3080 days",
                           .default = est_tag_life)
  ) %>%
  # filter(length_type %in% c("TLEN", "Total")) %>%
  group_by( year_release, tag_model, est_tag_life, sex) %>%
  summarise(
    n = n_distinct(tag_serial_number),
    mean_length = round(mean(length, na.rm = TRUE), 0),
    sem_length = round((sd(length) / sqrt(n())), 0),
    mean_weight = round(mean(weight, na.rm = TRUE), 2),
    sem_weight = round((sd(weight) / sqrt(n())), 2),
  ) %>%
  ungroup() %>%
  arrange(year_release, tag_model)
tag_summary

sum(tag_summary$n)
tag_summary


tag_summary <- tag_summary %>%
  mutate(
    length_mm = paste(mean_length, "±", sem_length),
    weight_kg = paste(mean_weight, "±", sem_weight),
  ) %>%
  dplyr::select(-c("mean_length", "sem_length", "mean_weight",
                   "sem_weight"))

tag_summary

openxlsx::write.xlsx(tag_summary, here("results",
                                       "summary-morphometrics",
                                       "tagging_summary_sex.xlsx"))


